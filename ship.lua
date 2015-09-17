local utils = require("utils")
local comm = require("comm")
local help = require("help")
local asteroid = require("asteroid")
local body = require("body")

local lume = require("lume")
local keymap = require("keymap")
local repl = require("srepl")
local edit = require("edit")

local default_config = utils.read_file("default_config.lua")

local scale_min = 1

local sensor_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
}

local status_whitelist = {
   "fuel", "fuel_capacity", "mass", "in_range",
   "engine_on", "turning_right", "turning_left", "credits",
   "cargo", "cargo_capacity",
   "engine_strength", "turning_speed", "laser_power",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range", "scoop_range",
}

local sandbox = {
   pairs = pairs,
   ipairs = ipairs,
   unpack = unpack,
   print = repl.print,
   pp = lume.serialize,
   realprint = print, -- for debugging
   coroutine = { yield = coroutine.yield,
                 status = coroutine.status },
   tonumber = tonumber,
   tostring = tostring,
   math = math,
   type = type,
   table = { concat = table.concat,
             remove = table.remove,
             insert = table.insert,
   },
   help = help.message,
   man = help.man,
   keymap = keymap,
}

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

   -- keep around
   fuel = 128,
   credits = 1024,
   time_offset = 4383504000, -- roughly 139 years ahead
   system_name = "L 668-21",
   upgrades = {},
   cargo = {["food"] = 2},

   cargo_capacity = 64,
   fuel_capacity = 128,
   scoop_range = 512,
   comm_range = 1024,
   recharge_rate = 1,
   burn_rate = 12,
   base_mass = 128,
   laser_power = 1024,

   engine_strength = 16,
   turning_speed = 4,

   configure = function(ship, systems, ui)
      repl.initialize()
      edit.initialize()

      ship.api.ui = ui
      ship.systems = systems
      ship:enter(ship.system_name, true)
   end,

   enter = function(ship, system_name, reseed)
      ship.api.repl.last_result = "Entering the " .. system_name .. " system."

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      if(reseed) then
      -- reset
         ship.x, ship.y = math.random(30000) + 10000, math.random(30000) + 10000
         ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
         ship.comm_connected, ship.target_number, ship.target = false, 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.seed_cargo(b)
            body.seed_pos(b, ship.bodies[1])
         end
      end
   end,

   update = function(ship, dt)
      ship.api.dt = dt
      -- calculate movement
      ship.x = ship.x + (ship.dx * dt * 100)
      ship.y = ship.y + (ship.dy * dt * 100)

      -- activate controls TODO: move these to a keymap too?
      if(keymap.current_mode == "flight") then
         for k,f in pairs(ship.api.controls) do
            f(love.keyboard.isDown(k))
         end
      end

      -- TODO: engine strength is in terms of force, not accel
      -- TODO: calculate oberth effect
      if(ship.engine_on and ship.fuel > 0) then
         ship.dx = ship.dx + (math.sin(ship.heading) * dt *
                                 ship.engine_strength * ship.api.throttle)
         ship.dy = ship.dy + (math.cos(ship.heading) * dt *
                                 ship.engine_strength * ship.api.throttle)
         ship.fuel = ship.fuel - (ship.burn_rate * dt * ship.api.throttle)
      elseif(ship.fuel < ship.fuel_capacity) then
         ship.fuel = ship.fuel + (ship.recharge_rate * dt)
      end

      if(ship.turning_left) then
         ship.heading = ship.heading + (dt * ship.turning_speed)
      elseif(ship.turning_right) then
         ship.heading = ship.heading - (dt * ship.turning_speed)
      end

      for _,b in pairs(ship.bodies) do
         if(b.portal and ship:cleared_for(b) and ship:in_range(b, 75)) then
            ship:enter(b.portal, true)
         end
         local distance = utils.distance(ship.x - b.x, ship.y - b.y)
         if(ship.laser and b.asteroid and ship:laser_hits(b, distance)) then
            -- TODO: firing laser uses up power?
            b.strength = b.strength - dt * ship.laser_power / math.sqrt(distance)
            if(b.strength < 0) then b:split(ship) end
         end
      end

      ship:enforce_limits()

      comm.flush()
   end,

   cleared_for = function(_, b)
      return not b.requires_clearance
   end,

   in_range = function(ship, b, range)
      return utils.distance(ship.x - b.x, ship.y - b.y) <
         (range or ship.comm_range)
   end,

   laser_hits = function(ship, b, distance)
      -- assuming circular images
      local diameter = b.image:getWidth() / 2
      local theta = math.atan2(b.y - ship.y, b.x - ship.x)
      local angular_divergence = math.abs(ship.heading - theta)
      local divergence = math.abs(math.sin(angular_divergence) * distance)
      return divergence < diameter
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
      ship:recalculate_mass()
   end,

   recalculate_mass = function(ship)
      ship.mass = ship.base_mass + ship:cargo_mass()
   end,

   cargo_mass = function(ship)
      local m = 0
      for _,c in pairs(ship.cargo) do m = m + c end
      return m
   end,

   enforce_limits = function(ship)
      if(ship.api.throttle > 1) then ship.throttle.api = 1 end
      if(ship.api.throttle < 0) then ship.throttle.api = 0 end
      if(ship.api.scale < scale_min) then ship.api.scale = scale_min end
   end,
}

-- everything in here is exposed to the sandbox
ship.api = {
   -- components
   repl = repl,
   edit = edit,

   -- data tables (read-only)
   sensors = utils.whitelist_table(ship, sensor_whitelist, "sensors"),
   status = utils.whitelist_table(ship, status_whitelist, "status"),

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
      laser = function(down) ship.laser = down end,
      login = utils.partial(comm.login, ship),
      anchor = function(ship) ship.cheat.dx, ship.cheat.dy = 0, 0 end,
   },
   load_config = function(ship)
      ship.repl.sandbox = sandbox
      sandbox.ship = ship

      local chunk = assert(loadstring(ship["config.lua"]))
      setfenv(chunk, sandbox)
      chunk()
   end,
   e = function(ship, path)
      if(ship[path]) then
         keymap.change_mode("edit")
         ship.repl.on(false)
         ship.edit.open(ship, path)
      else
         print(path .. " not found.")
      end
   end,

   persist = {"throttle", "scale", "trajectory"},
   ["config.lua"] = default_config,

   -- added by loading config
   controls = {},
   commands = {},
   helm = love.keyboard,

   -- you can adjust these to improve performance
   trajectory = 256,
   trajectory_step_size = 0.05,

   throttle = 1,
   scale = 1,

   cheat = ship,
   teleport = function(self)
      local target = self.sensors.target
      local dist = self.sensors.target * 10
      if(not target) then return end
      self.cheat.x, self.cheat.y, self.cheat.dx, self.cheat.dy =
         target.x + dist, target.y + dist, target.dx, target.dy
   end,
}

return ship
