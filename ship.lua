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
   engine_strength = 16,
   engine_on = false,
   turning_speed = 4,
   turning_right = false,
   turning_left = false,

   fuel = 128,
   fuel_capacity = 128,
   recharge_rate = 1,
   burn_rate = 4,
   mass = 128,

   comm_connected = false,
   comm_range = 2048,
   target_number = 0,
   target = nil,

   credits = 1024,
   time_offset = 4383504000, -- roughly 139 years ahead

   system_name = "L 668-21",

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
      ship.system = systems[system_name]
      ship.bodies = ship.system.bodies
      ship.x, ship.y = math.random(30000) + 10000, math.random(30000) + 10000
      ship.dx, ship.dy, ship.heading = 0, 0, math.pi
      ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
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
