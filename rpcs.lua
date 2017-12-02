-- These functions will be called by programs running on station OSes, but they
-- have access to functionality that isn't exposed inside the OS sandbox.

local lume = require("lume")
local utils = require("utils")
local bencode = require("bencode")
local clearances = require("data.clearances")
local base_prices = require("data.prices")
local upgrades = require("data.upgrades")
local mission = require("mission")
local utf8 = require("polywell.utf8")

local get_price = function(good, amount, prices, direction)
   local other_direction = direction == "sell" and "buy" or "sell"
   return amount * prices[good][other_direction]
end

local space_for = function(amount, ship, direction)
   return direction=="sell" or
      (ship:cargo_amount() + amount <= ship.cargo_capacity)
end

local in_stock = function(station, ship, good, amount, direction)
   local from = direction=="sell" and ship or station
   return from.cargo[good] and (from.cargo[good] >= amount)
end

local loan_borrow = function(ship, amount_string)
   local amount = tonumber(amount_string)
   if(not amount) then
      return "Please provide a numeric amount."
   elseif(amount <= 0) then
      return "Please provide a positive amount."
   elseif(amount ~= math.floor(amount)) then
      return "Please provide an integer amount."
   elseif(ship.loan + amount > 4096) then
      return "Credit limit exceeded."
   else
      ship.credits = ship.credits + amount
      ship.loan = ship.loan + math.ceil(amount * 1.2)
      return "OK, current loan balance is " .. ship.loan .. "."
   end
end

local loan_repay = function(ship, amount_string)
   local amount = tonumber(amount_string)
   if(not amount) then
      return "Please provide a numeric amount."
   elseif(amount <= 0) then
      return "Please provide a positive amount."
   elseif(amount ~= math.floor(amount)) then
      return "Please provide an integer amount."
   elseif(amount > ship.loan) then
      return "Can't repay more than you owe."
   else
      ship.credits = ship.credits - amount
      ship.loan = ship.loan - amount
      return "OK, current loan balance is " .. ship.loan .. "."
   end
end

local loan_balance = function(ship)
   if(ship.loan == 0) then
      return "No active loans at this time."
   else
      return "Current loan balance: " .. ship.loan
   end
end

return {
   buy_user = function(ship, port, fs_raw, username, password)
      if(port.account_price and ship.credits >= port.account_price) then
         local fs = port.os.fs.proxy(fs_raw, "root", fs_raw)
         port.os.fs.add_user(fs, username, password)
         ship.credits = ship.credits - port.account_price
         return true
      elseif(port.account_price) then
         return false, "Insufficient credits."
      else
         return false, "This port does not sell accounts."
      end
   end,

   refuel = function(ship, port, amount)
      if(amount < 0) then
         return false, "Nice try."
      elseif(port.fuel_price) then
         local cost = port.fuel_price * amount
         local open_fuel_capacity = ship.fuel_capacity - ship.fuel
         if(open_fuel_capacity <= 0) then
            return false, "Fuel tank is full."
         elseif(amount > open_fuel_capacity) then
            return false, "Fuel tank only has room for " .. open_fuel_capacity .. "."
         elseif(cost < ship.credits) then
            ship.fuel = ship.fuel + amount
            ship.credits = ship.credits - cost
            return amount, "Purchased " .. amount .. " fuel for " .. cost .. "."
         else
            return false, "Insufficient credits."
         end
      else
         return false, "This station does not sell fuel."
      end
   end,

   buy_upgrade = function(ship, port, name)
      local price = port.upgrade_prices and port.upgrade_prices[name]
      if(not price) then
         return false, port.name .. " does not sell " .. name
      elseif(ship.credits < price) then
         return false, "Insufficient credits; need " .. price
      elseif(utils.includes(ship.upgrade_names, name)) then
         return false, "You already have this upgrade."
      else
         if(upgrades[name].buy) then upgrades[name].buy(ship) end
         table.insert(ship.upgrade_names, name)
         ship:recalculate()
         ship.credits = ship.credits - price
         return price
      end
   end,

   sell_upgrade = function(ship, port, name)
      local price = math.floor(((port.upgrade_prices and
                                    port.upgrade_prices[name]) or
               base_prices.upgrades[name]) * 0.85)
      if(not price) then
         return false, "This upgrade is not for sale."
      elseif(not utils.includes(ship.upgrade_names, name)) then
         return false, "You don't have this upgrade."
      else
         if(upgrades[name].sell) then upgrades[name].sell(ship) end
         lume.remove(ship.upgrade_names, name)
         ship:recalculate()
         ship.credits = ship.credits + price
         return price
      end
   end,

   list_upgrades = function(_, port)
      return port.upgrade_prices or {}
   end,

   upgrade_help = function(ship, _, upgrade_name)
      return ship.api.help.get(upgrade_name)
   end,

   fuel_price = function(_, port)
      return port.fuel_price or 0
   end,

   cargo_prices = function(_, port)
      return port.prices
   end,

   cargo_amounts = function(_, port)
      return port.cargo
   end,

   cargo_hold = function(ship, _)
      return ship.cargo
   end,

   port = function(ship, _, command)
      if(command ~= "fine") then
         return "Unknown command"
      elseif(ship.fine == 0) then
         return "No outstanding fines."
      elseif(ship.credits < ship.fine) then
         return "Insufficient credits. Fine balance: " .. ship.fine
      else
         ship.credits = ship.credits - ship.fine
         ship.fine = 0
         return "Fine paid: " .. ship.fine
      end
   end,

   fine = function(ship, _, amount)
      ship.fine = ship.fine + amount
   end,

   loan = function(ship, _, command, ...)
      if(command == "borrow") then return loan_borrow(ship, ...)
      elseif(command == "repay") then return loan_repay(ship, ...)
      elseif(command == "balance") then return loan_balance(ship, ...)
      end
   end,

   cargo_transfer = function(ship, port, direction, good, amount)
      assert(port.prices[good], port.name .. " does not trade in " .. good)
      local price = get_price(good, amount, port.prices, direction)
      if(ship.credits < price and direction == "buy") then
         return false, "Don't have " .. price .. " credits."
      elseif(not space_for(amount, ship, direction)) then
         return false, "No space for " .. amount .. " of " .. good .. "."
      elseif(not in_stock(port, ship, good, amount, direction)) then
         if(direction == "buy") then
            return false, "Sufficient " .. good .. " is not in stock."
         else
            return false, "You don't have enough " .. good .. "."
         end
      else
         if(direction == "sell") then
            port.cargo[good] = port.cargo[good] + amount
            ship:move_cargo(good, -amount)
            ship.credits = ship.credits + price
         else
            port.cargo[good] = port.cargo[good] - amount
            ship:move_cargo(good, amount)
            ship.credits = ship.credits - price
         end
         return price
      end
   end,

   no_trip_clearance = function(ship, portal)
      local to, is_interportal = portal.portal, portal.interportal
      for _, place in ipairs({clearances[ship.system_name .. ":" .. to] or {},
                              is_interportal and clearances["interportal"] or {},
                              clearances["any"],}) do
         for _, check in ipairs(place) do
            if(type(check) == "function") then
               local reason = check(ship)
               if(reason) then return reason end
            else
               for event, reason in pairs(check) do
                  if(type(check) == "string" and not ship.events[event]) then
                     return reason
                  end
               end
            end
         end
      end
      return false
   end,

   set_beams = function(ship, portal, n)
      portal.beam_count = ((n or 0) * 9) / ship.portal_time
      return ship.portal_range >= utils.distance(ship, portal)
   end,

   draw_power = function(ship, portal, power)
      if(ship.battery - power < 0 or power <= 0) then return false end
      ship.portal_target = portal
      ship.battery = ship.battery - power
      return true
   end,

   portal_activate = function(ship, portal)
      portal.beam_count = nil
      ship:enter(portal.portal, true)
   end,

   set_prompt = function(ship, _, prompt)
      ship.api.editor.with_current_buffer("*console*",
                                          ship.api.editor.set_prompt, prompt)
   end,

   get_prompt = function(ship, _)
      return ship.api.editor.with_current_buffer("*console*",
                                                 ship.api.editor.get_prompt)
   end,

   print_prompt = function(ship, _)
      ship.api.editor.with_current_buffer("*console*",
                                          ship.api.editor.print_prompt)
   end,

   time = function(ship, _)
      return utils.time(ship)
   end,

   ship_status = function(ship, _, field)
      return ship.api.status[field]
   end,

   distance = utils.distance,

   completions = function(ship, _, completions, entered)
      -- if the last command was not complete, then don't do anything
      if(ship.api.editor.last_command() ~=
         ship.api.editor.get("complete")) then return
      elseif(#completions == 1) then
         ship.api.editor.textinput(utf8.sub(completions[1], #entered + 1), true)
      elseif(#completions > 0) then
         local common = utils.longest_common_prefix(completions)
         if(common == entered) then
            ship.api.editor.echo(table.concat(completions, " "))
         else
            ship.api.editor.textinput(utf8.sub(common, #entered + 1), true)
         end
      end
   end,

   record_event = function(ship, _, event)
      mission.record_event(ship, event)
   end,

   split_editor = function(ship, _, buffer_name, mode)
      if(not love.window) then return end
      if(buffer_name and mode) then
         ship.api.editor.open_in_split(nil, buffer_name, mode)
         ship.api.editor.focus_next()
      else
         ship.api.editor.split(nil)
      end
   end,

   rover_state = function(ship, _, state)
      ship.api.rover_state = state
   end,

   subnet = {
      request = function(ship, input_string)
         local enc = function(x)
            local ok, val = pcall(bencode.encode, x)
            return (ok and val), (ok or val)
         end
         local fs = love.filesystem
         local ok, input = pcall(bencode.decode, input_string)
         if(not ok) then return nil, input end

         if(input.command == "groups") then
            local groups = fs.getDirectoryItems("data/subnet")
            groups = lume.filter(groups, function(g)
                                    return fs.isDirectory("data/subnet/" .. g)
            end)
            mission.record_event(ship, "subnet")
            return enc({status="ok", groups=groups})
         elseif(input.command == "list") then
            local posts = fs.getDirectoryItems("data/subnet/" .. input.group)
            return enc({status="ok", posts=posts, group=input.group})
         elseif(input.command == "get") then
            local text = fs.read("data/subnet/"..input.group.."/"..input.post)
            if(text) then
               return enc({status="ok", content=text, post=input.post})
            else
               return(enc({status="failed"}))
            end
         elseif(input.command == "help") then -- "d7:command4:helpe"
            return enc(love.filesystem.read("doc/subnet.md"))
         else
            return enc({status="unknown command",
                        commands={"groups","list","get","help"}})
         end
      end
   },
}
