local utils = require("utils")
local lume = require("lume")

local default_config = love.filesystem.read("ship/default_config.lua")
local fallback_config = love.filesystem.read("ship/fallback_config.lua")

local comm = require("ship.comm")
local help = require("ship.help")
local repl = require("ship.repl")
local upgrade = require("ship.upgrade")

local keymap = require("keymap")
local edit = require("edit")

-- for shuffling systems upon entry
local asteroid = require("asteroid")
local body = require("body")

local scale_min = 1

local sensor_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
}

local status_whitelist = {
   "fuel", "fuel_capacity", "battery", "battery_capacity", "mass", "in_range",
   "engine_on", "turning_right", "turning_left", "credits", "upgrade_names",
   "cargo", "cargo_capacity",
   "engine_strength", "turning_speed",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range", "scoop_range",
   "passponder_range", "passponder_power", "passponder_time"
}

local base_stats = {
   mass = 128,
   cargo_capacity = 64,
   fuel_capacity = 128,
   scoop_range = 0,
   comm_range = 2048 * 9999999,
   recharge_rate = 2,
   burn_rate = 16,
   engine_strength = 1024,
   turning_speed = 4,
   battery_capacity = 128,

   passponder_range = 0,
   passponder_time = 0,
   passponder_power = 0,
}

local sandbox_dofile = function(ship, filename)
   local contents = ship:find(filename)
   assert(type(contents) == "string", filename .. " is not a file.")
   local chunk = assert(loadstring(contents))
   setfenv(chunk, sandbox)
   chunk()
end

local sandbox = function(ship)
   return lume.merge(utils.sandbox,
                     {  help = help.message,
                        keymap = keymap,
                        default_config = default_config,
                        print = repl.print,
                        ship = ship.api,
                        dofile = lume.fn(sandbox_dofile, ship.api),
                        os = {time = lume.fn(utils.time, ship)},
                        scp = lume.fn(comm.scp, ship),
                        man = lume.fn(help.man, ship.api),
   })
end

local epoch_for = function(year)
   local years = year - 1970
   return years * 365 * 52 * 7 * 24 * 60 * 60
end

local ship = {
   -- ephemeral
   x=0, y=0, dx=0, dy=0, heading = math.pi,
   engine_on = false,
   turning_right = false,
   turning_left = false,
   comm_connected = false,
   target_number = 0,
   target = nil,
   mass = 128,
   battery = 128,
   upgrades = {},
   time_factor = 10,

   -- keep around
   fuel = 128,
   credits = 1024,
   time_offset = epoch_for(2431),
   system_name = "L 668-21",
   cargo = {["food"] = 2},
   upgrade_names = {"passponder"},
   events = {},
   visas = {},
   flag = "Tana",

   configure = function(ship, systems, ui)
      repl.initialize()
      edit.initialize()

      ship.api.ui = ui
      ship.systems = systems

      ship.sandbox = sandbox(ship)
      ship.api.repl.sandbox = ship.sandbox
   end,

   enter = function(ship, system_name, reseed)
      assert(ship.systems[system_name], system_name .. " not found.")
      ship.api.repl.display_line = "Entering the " .. system_name .. " system."

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      comm.logout_all(ship)
      ship:recalculate()

      if(reseed) then
         -- reset
         ship.x, ship.y = math.random(30000) + 10000, math.random(30000) + 10000
         ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
         ship.comm_connected, ship.target_number, ship.target = false, 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.seed_news(ship, b)
            body.seed_cargo(b)
            body.seed_pos(b, ship.bodies[1])
         end
      end
   end,

   update = function(ship, dt)
      ship.api.dt = dt

      -- activate controls
      if(keymap.current_mode == "flight") then
         for k,f in pairs(ship.api.controls) do
            f(love.keyboard.isDown(k))
         end
      end

      -- TODO: calculate oberth effect
      if(ship.engine_on and ship.fuel > 0) then
         local fx = (math.sin(ship.heading) * dt * ship.engine_strength)
         local fy = (math.cos(ship.heading) * dt * ship.engine_strength)
         ship.dx = ship.dx + fx / ship.mass
         ship.dy = ship.dy + fy / ship.mass
         ship.fuel = ship.fuel - (ship.burn_rate * dt)
      elseif(ship.fuel < ship.fuel_capacity) then
         ship.fuel = ship.fuel + (ship.recharge_rate * dt)
      end

      if(ship.battery < ship.battery_capacity) then
         local dist = utils.distance(ship.x, ship.y)
         ship.battery = ship.battery + (dt / math.log(dist*2)) * 30
      end

      for _,f in pairs(ship.api.updaters or {}) do
         f(ship.api, dt)
      end

      if(ship.turning_left) then
         ship.heading = ship.heading + (dt * ship.turning_speed)
      elseif(ship.turning_right) then
         ship.heading = ship.heading - (dt * ship.turning_speed)
      end

      ship:enforce_limits()
      for _,u in pairs(ship.upgrades) do
         if(u.update) then u.update(ship, dt) end
      end

      comm.flush()
   end,

   in_range = function(ship, b, range)
      return b and utils.distance(ship.x - b.x, ship.y - b.y) <
         (range or ship.comm_range)
   end,

   cargo_amount = function(ship)
      local amt = 0
      for _,v in pairs(ship.cargo) do amt = amt + v end
      return amt
   end,

   move_cargo = function(ship, good, amount, discard_remainder)
      assert((ship.cargo[good] or 0) >= -amount, "Not enough " .. good)
      local available_mass = ship.cargo_capacity - ship:cargo_mass()

      if(discard_remainder) then
         amount = math.min(amount, available_mass)
      else
         assert(amount <= available_mass, "Can't fit " .. amount .. " in hold.")
      end

      ship.cargo[good] = (ship.cargo[good] or 0) + amount
      ship:recalculate()
   end,

   recalculate = function(ship)
      ship.target = ship.bodies[ship.target_number]

      for k,v in pairs(base_stats) do
         ship[k] = v
      end

      for _,u in ipairs(ship.upgrade_names) do
         ship.upgrades[u] = assert(upgrade[u], u .. " not found.")
      end

      for name,u in pairs(ship.upgrades) do
         for k,v in pairs(u.stats) do
            ship[k] = (ship[k] or 0) + v
         end
         ship.api.actions[name] = u.action and lume.fn(u.action, ship)
      end

      ship.mass = ship.mass + ship:cargo_mass()
   end,

   cargo_mass = function(ship)
      local m = 0
      for _,c in pairs(ship.cargo) do m = m + c end
      return m
   end,

   enforce_limits = function(ship)
      if(ship.api.scale < scale_min) then ship.api.scale = scale_min end
   end,
}

-- everything in here is exposed to the sandbox. this table *is* `ship`, as far
-- as the in-game code is concerned.
ship.api = {
   repl = repl,
   edit = edit,

   -- data tables (read-only)
   sensors = utils.whitelist_table(ship, sensor_whitelist, "sensors"),
   status = utils.whitelist_table(ship, status_whitelist, "status"),

   -- upgrades can place functions in this table when loaded
   actions = {
      forward = function(down) ship.engine_on = down end,
      left = function(down) ship.turning_left = down end,
      right = function(down) ship.turning_right = down end,
      next_target = function()
         if(love.keyboard.isDown("lshift", "rshift")) then
            ship.target_number = ((ship.target_number - 1) %
                  (table.length(ship.api.sensors.bodies) + 1))
         else
            ship.target_number = ((ship.target_number + 1) %
                  (table.length(ship.api.sensors.bodies) + 1))
         end
         ship.target = ship.bodies[ship.target_number]
      end,
      closest_target = function()
         local min_distance = 1000000000000
         for i,b in ipairs(ship.api.sensors.bodies) do
            if(utils.distance(ship, b) < min_distance) then
               ship.target_number = i
               min_distance = utils.distance(ship, b)
            end
         end
         ship.target = ship.bodies[ship.target_number]
      end,
      login = lume.fn(comm.login, ship),
      anchor = function(s) s.cheat.dx, s.cheat.dy = 0, 0 end,
   },

   load = function(s, filename)
      filename = filename or "src.config"
      local content = assert(s:find(filename), "File not found: " .. filename)
      local chunk = assert(loadstring(content), "Failed to load " .. filename)
      setfenv(chunk, ship.sandbox)
      chunk()
   end,

   e = function(s, path)
      if(type(path) == "string") then
         keymap.change_mode("edit")
         s.repl.on(false)
         s.edit.open(s, path)
      end
   end,

   find = function(s, path)
      local parts = lume.split(path, ".")
      local target = s
      for _,p in ipairs(parts) do
         if(type(target) == "table") then
            target = target[p]
         else
            return false
         end
      end
      return target
   end,

   -- for user files
   src = {
      ["config"] = default_config,
      ["fallback_config"] = fallback_config,
   },
   docs = {},
   persist = {"persist", "scale", "trajectory", "src", "docs"},

   -- added by loading config
   controls = {},
   commands = {},
   helm = love.keyboard,

   -- you can adjust these to improve performance
   trajectory = 512,
   trajectory_step_size = 0.05,

   fuel_to_stop = function(s)
      -- no idea where this 20 factor comes from
      return utils.distance(s.sensors.dx, s.sensors.dy) *
         s.status.engine_strength * s.status.burn_rate / (s.status.mass * 20)
   end,

   scale = 1.9,

   cheat = ship,
}

return ship
