local utils = require("utils")
local comm = require("comm")
local repl = require("repl")

local default_config_file = io.open("default_config.lua", "r")
local default_config = default_config_file:read("*all")
default_config_file:close()

repl.last_result = "Press control-` to open the repl or just start typing code."

local sensor_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "fuel", "mass", "bodies", "in_range",
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
}

local ship = {
   x = 30000, y = 30000,
   dx = 1, dy = 0,
   heading = math.pi,

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
   time = 157654,g

   config = default_config,

   configure = function(ship, bodies, ui)
      repl.initialize()
      repl.screenshot = false
      repl.font = love.graphics.getFont()
      repl.sandbox = sandbox
      local chunk = assert(loadstring(ship.config))
      ship.bodies = bodies
      sandbox.ship = ship.api
      sandbox.ui = ui
      sandbox.refuel = function() ship.fuel = ship.fuel_capacity end -- cheat
      setfenv(chunk, sandbox)
      chunk()
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

      comm.flush()
   end,

   in_range = function(ship, body)
      return utils.calculate_distance(ship.x - body.x, ship.y - body.y) <
         ship.comm_range
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
         ship.target_number = ((ship.target_number + 1) %
               (#ship.api.sensors.bodies + 1))
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
