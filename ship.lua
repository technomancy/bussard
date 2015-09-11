local utils = require("utils")
local comm = require("comm")
local repl = require("repl")
local help = require("help")
local system = require("system")

local default_config = utils.read_file("default_config.lua")

local sensor_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "fuel", "mass", "in_range", "bodies",
   -- maybe these don't belong as sensors?
   -- ship status
   "engine_on", "turning_right", "turning_left",
   -- ship capabilities
   "engine_strength", "turning_speed",
   "recharge_rate", "burn_rate", "comm_range",
}

local sandbox = {
   pairs = pairs,
   ipairs = ipairs,
   unpack = unpack,
   print = repl.print,
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

   -- keep around
   fuel = 128,
   credits = 1024,
   time_offset = 4383504000, -- roughly 139 years ahead
   system_name = "L 668-21",
   upgrades = {},
   cargo = {},

   fuel_capacity = 128,
   recharge_rate = 1,
   burn_rate = 12,
   mass = 128,

   engine_strength = 16,
   turning_speed = 4,
   comm_range = 2048,

   config = default_config,

   configure = function(ship, systems, ui)
      repl.initialize()
      repl.font = love.graphics.getFont()
      repl.sandbox = sandbox

      ship.systems = systems
      sandbox.ship = ship.api
      sandbox.ui = ui
      sandbox.refuel = function() ship.fuel = ship.fuel_capacity end -- cheat

      local chunk = assert(loadstring(ship.config))
      setfenv(chunk, sandbox)
      chunk()

      ship:enter(systems, ship.system_name)
   end,

   enter = function(ship, systems, system_name)
      ship.api.repl.last_result = "Entering the " .. system_name .. " system."

      -- stuff these things in there to expose to in-ship APIs
      ship.system = systems[system_name]
      ship.bodies = ship.system.bodies

      -- reset
      ship.x, ship.y = math.random(30000) + 10000, math.random(30000) + 10000
      ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
      ship.comm_connected, ship.target_number, ship.target = false, 0, nil

      system.populate_asteroids(ship.system)
   end,

   update = function(ship, dt)
      ship.api.dt = dt
      -- calculate movement
      ship.x = ship.x + (ship.dx * dt * 100)
      ship.y = ship.y + (ship.dy * dt * 100)

      -- activate controls
      if(not ship.api.repl.toggled()) then
         for k,f in pairs(ship.api.controls) do
            f(love.keyboard.isDown(k))
         end
      end

      if(ship.engine_on and ship.fuel > 0) then
         ship.dx = ship.dx + (math.sin(ship.heading) * dt * ship.engine_strength)
         ship.dy = ship.dy + (math.cos(ship.heading) * dt * ship.engine_strength)
         ship.fuel = ship.fuel - (ship.burn_rate * dt)
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
            ship:enter(ship.systems, b.portal)
         end
         local distance = utils.calculate_distance(ship.x - b.x, ship.y - b.y)
         if(ship.laser and b.asteroid and ship:laser_hits(b, distance)) then
            print(b.name .. " hit, remaining: " .. b.strength)
            b.strength = b.strength - dt * ship.laser_power / distance
            if(b.strength < 0) then b:split() end
         end
      end

      comm.flush()
   end,

   cleared_for = function(ship, body)
      return not body.requires_clearance
   end,

   in_range = function(ship, body, range)
      return utils.calculate_distance(ship.x - body.x, ship.y - body.y) <
         (range or ship.comm_range)
   end,

   laser_hits = function(ship, body, distance)
      -- assuming circular images
      local diameter = body.image:getWidth() / 2
      local theta = math.atan2(body.y - ship.y, body.x - ship.x)
      local angular_divergence = math.abs(ship.heading - theta)
      local divergence = math.abs(math.sin(angular_divergence) * distance)
      return divergence < diameter
   end,
}

ship.api = {
   repl = repl,
   sensors = utils.whitelist_table(ship, sensor_whitelist, "sensors"),
   actions = {
      forward = function(down) ship.engine_on = down end,
      left = function(down) ship.turning_left = down end,
      right = function(down) ship.turning_right = down end,
      next_target = function()
         if(love.keyboard.isDown("lshift", "rshift")) then
            ship.target_number = ((ship.target_number - 1) %
                  (#ship.api.sensors.bodies + 1))
         else
            ship.target_number = ((ship.target_number + 1) %
                  (#ship.api.sensors.bodies + 1))
         end
         ship.target = ship.api.sensors.bodies[ship.target_number]
      end,
      laser = function(down) ship.laser = down end,
   },
   -- added by loading config
   controls = {},
   commands = {},
   comm = comm,
   helm = love.keyboard,
   trajectory = 256,
   step_size = 0.05,

   cheat = ship,
}

return ship
