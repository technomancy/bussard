local calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end
local default_config_file = io.open("default_config.lua", "r")
local default_config = default_config_file:read("*all")
default_config_file:close()

local ship = { x = -200, y = 0,
               dx = 0, dy = 0,
               heading = math.pi,

               engine = 3,
               engine_on = false,
               turning_speed = 3,
               turning_right = false,
               turning_left = false,

               fuel = 100,
               recharge_rate = 1,
               burn_rate = 5,
               mass = 1,

               landed = false,
               landing_speed_max = 10,
               target_number = 0,
               target = nil,

               config = default_config,

               configure = function(ship, bodies)
                  ship.api.sensors.bodies = function() return bodies end
                  local chunk = assert(loadstring(ship.config))
                  local box = { pairs = pairs,
                                ipairs = ipairs,
                                unpack = unpack,
                                print = print, -- TODO: route to messages
                                coroutine = { yield = coroutine.yield,
                                              status = coroutine.status },
                                io = { write = write, read = read },
                                tonumber = tonumber,
                                tostring = tostring,
                                math = math,
                                type = type,
                                table = { concat = table.concat,
                                          remove = table.remove,
                                          insert = table.insert,
                                },
                                ship = ship.api,
                  }
                  setfenv(chunk, box)
                  chunk()
               end,

               update = function(ship, dt)
                  -- calculate movement
                  ship.x = ship.x + (ship.dx * dt * 100)
                  ship.y = ship.y + (ship.dy * dt * 100)

                  -- activate controls
                  for k,f in pairs(ship.api.controls) do
                     f(love.keyboard.isDown(k))
                  end

                  if(ship.engine_on and ship.fuel > 0) then
                     ship.dx = ship.dx + (math.sin(ship.heading) * dt * ship.engine)
                     ship.dy = ship.dy + (math.cos(ship.heading) * dt * ship.engine)
                     ship.fuel = ship.fuel - (ship.burn_rate * dt)
                  elseif(ship.fuel < 100) then
                     ship.fuel = ship.fuel + (ship.recharge_rate * dt)
                  end

                  if(ship.turning_left) then
                     ship.heading = ship.heading + (dt * ship.turning_speed)
                  elseif(ship.turning_right) then
                     ship.heading = ship.heading - (dt * ship.turning_speed)
                  end
               end,

               can_land = function(ship, target)
                  local dist_max = target and target.image:getWidth() / 2
                  return(target and (not ship.landed) and target.description and
                         (calculate_distance(ship.dx - target.dx, ship.dy - target.dy))
                         < landing_speed_max and
                         (calculate_distance(ship.x - target.x, ship.y - target.y)) <
                         dist_max)
               end,
}

ship.api = {
   sensors = { x = function() return ship.x end,
               y = function() return ship.y end,
               dx = function() return ship.dx end,
               dy = function() return ship.dy end,
               heading = function() return ship.heading end,
               fuel = function() return ship.fuel end,
               landed = function() return ship.landed end,
   },
   controls = {},
   commands = {},
   actions = { forward = function(down) ship.engine_on = down end,
               left = function(down) ship.turning_left = down end,
               right = function(down) ship.turning_right = down end,
               land = function()
                  if(ship:can_land(ship.target)) then ship.landed = ship.target end
               end,
               next_target = function()
                  local bodies = ship.api.sensors.bodies()
                  ship.target_number = ((ship.target_number) % #bodies) + 1
                  ship.target = bodies[ship.target_number]
               end,
   },
}

return ship