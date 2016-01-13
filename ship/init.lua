local utils = require("utils")
local lume = require("lume")

local default_config = love.filesystem.read("data/default_config.lua")
local fallback_config = love.filesystem.read("data/fallback_config.lua")

local comm = require("ship.comm")
local help = require("ship.help")
local console = require("ship.console")
local upgrade = require("ship.upgrade")
local ai = require("ship.ai")

local keymap = require("keymap")
local edit = require("edit")
local mission = require("mission")

-- for shuffling systems upon entry
local asteroid = require("asteroid")
local body = require("body")

local scale_min = 1

local status_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
   "fuel", "fuel_capacity", "battery", "battery_capacity", "mass", "in_range",
   "engine_on", "turning_right", "turning_left", "credits", "upgrade_names",
   "cargo", "cargo_capacity", "solar",
   "engine_strength", "turning_speed", "cpuinfo",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range", "scoop_range",
   "portal_range", "portal_time", "flag", "target", "comm_boost",
}

local base_stats = {
   mass = 128,
   cargo_capacity = 64,
   fuel_capacity = 192,
   scoop_range = 0,
   comm_range = 2048,
   recharge_rate = 4,
   burn_rate = 16,
   engine_strength = 1024,
   turning_speed = 4,
   battery_capacity = 128,
   solar = 30,

   portal_range = 1024,
   portal_time = 40, -- in-game seconds
}

local sandbox_dofile = function(ship, filename)
   local contents = ship.api:find(filename)
   assert(type(contents) == "string", filename .. " is not a file.")
   local chunk = assert(loadstring(contents))
   setfenv(chunk, ship.sandbox)
   return chunk()
end

local sandbox = function(ship)
   return lume.merge(utils.sandbox,
                     {  help = help.message,
                        keymap = keymap,
                        default_config = default_config,
                        print = console.print,
                        clear = console.clear_lines,
                        ship = ship.api,
                        dofile = lume.fn(sandbox_dofile, ship),
                        -- TODO: add require too; maybe loadstring
                        os = {time = lume.fn(utils.time, ship)},
                        scp = lume.fn(comm.scp, ship),
                        man = lume.fn(help.man, ship.api),
   })
end

local epoch_for = function(year)
   local years = year - 1970
   return years * 365 * 52 * 7 * 24 * 60 * 60
end

local target_dt = 0.03 -- about 33 frames per second

local trajectory_adjust = function(ship, dt)
   if(not ship.trajectory_adjust_progress) then
      ship.trajectory_adjust_progress = 5
      return
   end

   ship.trajectory_adjust_progress = ship.trajectory_adjust_progress - dt

   if((ship.trajectory_adjust_progress or 0) <= 0) then
      ship.updaters.trajectory_adjust = nil
      ship.trajectory_adjust_progress = nil
   elseif(dt > target_dt and ship.trajectory_adjust_progress < 4) then
      -- give it a second to stabilize, then reduce
      ship.trajectory = ship.trajectory * 0.8
      ship.trajectory_step_size = ship.trajectory_seconds / ship.trajectory
   end
end

local ship = {
   base_stats = base_stats,

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
   upgrade_names = {},
   active_missions={},
   events = {},
   flag = "Tana",

   cpuinfo = {processors=64, arch="arm128-ng", mhz=2800},
   configure = function(ship, systems, ui)
      console.initialize()
      edit.initialize()

      ship.api.ui = ui
      ship.systems = systems

      ship.sandbox = sandbox(ship)
      ship.api.console.sandbox = ship.sandbox
   end,

   enter = function(ship, system_name, reseed)
      local from = ship.system_name
      assert(ship.systems[system_name], system_name .. " not found.")
      ship.api.console.display_line = "Entering the " .. system_name .. " system."

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      comm.logout_all(ship)
      ship:recalculate()

      if(reseed) then
         ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
         ship.comm_connected, ship.target_number, ship.target = false, 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.seed_pos(b, ship.bodies[1])
            body.seed_news(ship, b)
            body.seed_cargo(b)
         end

         local portal = lume.match(ship.bodies, function(b) return b.portal == from end)
         if(portal) then
            ship.x, ship.y = portal.x, portal.y
            ship.dx, ship.dy = portal.dx, portal.dy
         else
            ship.x, ship.y = math.random(30000) + 10000, math.random(30000) + 10000
         end
         ai.seed(system_name, ship.bodies)
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

      -- this seems overcomplicated at first glance--why are we
      -- setting an engine_on bit in the ship.controls handler and
      -- then checking it later? the answer is that otherwise it would
      -- be a sandbox violation. if the ship.controls handler affected
      -- acceleration directly, you could write code that would make
      -- the engine arbitrarily powerful or use zero fuel or
      -- whatever. so these two steps must remain separate.
      if(ship.engine_on and ship.fuel > 0) then
         -- TODO: calculate oberth effect
         local fx = (math.sin(ship.heading) * dt * ship.engine_strength)
         local fy = (math.cos(ship.heading) * dt * ship.engine_strength)
         ship.dx = ship.dx + fx / ship.mass
         ship.dy = ship.dy + fy / ship.mass
         ship.fuel = ship.fuel - (ship.burn_rate * dt)
      elseif(ship.fuel < ship.fuel_capacity) then
         ship.fuel = ship.fuel + (ship.recharge_rate * dt)
      end

      if(ship.turning_left) then
         ship.heading = ship.heading + (dt * ship.turning_speed)
      elseif(ship.turning_right) then
         ship.heading = ship.heading - (dt * ship.turning_speed)
      end

      if(ship.battery < ship.battery_capacity) then
         local dist = utils.distance(ship.x, ship.y)
         ship.battery = ship.battery + (dt / math.log(dist*2)) * ship.solar
      end

      for _,f in pairs(ship.api.updaters or {}) do
         f(ship.api, dt)
      end

      for _,u in pairs(ship.upgrades) do
         if(u.update) then u.update(ship, dt) end
      end

      mission.update(ship, dt)
      ship:enforce_limits()
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

   -- run when cargo or upgrades change; always idempotent
   recalculate = function(ship)
      ship.target = ship.bodies[ship.target_number]
      -- this needs a bit more work; it shrinks the trajectory way too much
      -- if(ship.api.trajectory_auto) then
      --    ship.api.updaters.trajectory_adjust = trajectory_adjust
      -- end

      for k,v in pairs(base_stats) do
         ship[k] = v
      end

      for _,u in ipairs(ship.upgrade_names) do
         ship.upgrades[u] = assert(upgrade[u], u .. " not found.")
         if(upgrade[u].load) then upgrade[u].load(ship) end
         for k,v in pairs(upgrade[u].stats or {}) do
            ship[k] = (ship[k] or 0) + v
         end
         ship.api.actions[u] = upgrade[u].action and
            lume.fn(upgrade[u].action, ship)
      end

      for good,amt in pairs(ship.cargo) do
         if(amt == 0) then ship.cargo[good] = nil end
      end

      ship.mass = ship.mass + ship:cargo_mass()
      if(ship.events["jinod3"]) then
         for _,b in ipairs(ship.systems["Sol"].bodies) do
            if(b.name == "Mars") then
               if(not lume.find(b.upgrades, "comm_boost")) then
                  table.insert(b.upgrades, "comm_boost")
                  body.seed_cargo(b, true)
               end
            end
         end
      end
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
   console = console,
   repl = console, -- for backwards-compatibility
   edit = edit,
   help = help,

   mission = {
      list = lume.fn(mission.list, ship),
      abort = lume.fn(mission.abort, ship),
      hud = {x=-300, y=-100, type="text", format="Missions: %s",
             values = {lume.fn(mission.readout, ship)}}
   },
   -- data tables (read-only)
   sensors = utils.whitelist_table(ship, status_whitelist, "sensors"),
   status = utils.whitelist_table(ship, status_whitelist, "status"),

   -- upgrades can place functions in this table when loaded
   actions = {
      forward = function(down) ship.engine_on = down end,
      left = function(down) ship.turning_left = down end,
      right = function(down) ship.turning_right = down end,
      next_target = function()
         if(love.keyboard.isDown("lshift", "rshift")) then
            ship.target_number = ((ship.target_number - 1) %
                  (table.length(ship.bodies) + 1))
         else
            ship.target_number = ((ship.target_number + 1) %
                  (table.length(ship.bodies) + 1))
         end
         ship.target = ship.bodies[ship.target_number]
      end,
      closest_target = function()
         local min_distance = 1000000000000
         for i,b in ipairs(ship.bodies) do
            if(utils.distance(ship, b) < min_distance) then
               ship.target_number = i
               min_distance = utils.distance(ship, b)
            end
         end
         ship.target = ship.bodies[ship.target_number]
      end,
      login = lume.fn(comm.login, ship),
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
         s.console.on(false)
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
            return nil
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
   persist = {"persist", "scale", "src", "docs", "trajectory_seconds",
              "trajectory", "trajectory_step_size", "trajectory_auto"},

   -- added by loading config
   controls = {},
   commands = {},
   updaters = {},
   helm = love.keyboard,

   -- trajectory plotting is the single biggest perf drain by far
   -- these numbers will be changed if the frame rate is too low
   trajectory = 256,
   trajectory_step_size = 0.1,
   trajectory_seconds = 15, -- how far out the trajectory should go
   trajectory_auto = true, -- turn this off to disable auto-adjustment

   fuel_to_stop = function(s)
      -- no idea where this 20 factor comes from
      return utils.distance(s.status.dx, s.status.dy) *
         s.status.engine_strength * s.status.burn_rate / (s.status.mass * 20)
   end,

   scale = 1.9,

   cheat = ship,
}

return ship
