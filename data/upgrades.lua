local utils = require("utils")
local lume = require("lume")
local asteroid = require("asteroid")

local laser_hits = function(ship, b, distance)
   -- assuming circular images
   if(b.image) then
      local diameter = b.image:getWidth() / 2
      local theta = math.atan2(b.y - ship.y, b.x - ship.x)
      local angular_divergence = math.abs(ship.heading - theta)
      local divergence = math.abs(math.sin(angular_divergence) * distance)
      return divergence < diameter
   end
end

local gov_colors = {
   ["Tana"] = {0xaf, 0xfe, 0xee},
   ["Sol"] = {0xff, 0xff, 0xff},
   ["Terran"] = {0x22, 0x8b, 0x22},
   ["Bohk"] = {0xff, 0xd7, 0x00},
   ["Katilay"] = {0x71, 0x1c, 0xae},
   ["Yueh"] = {0xcd, 0x00, 0x00},
}

local map_draw = function(ship)
   local em = love.graphics.getFont():getWidth('a')
   local w,h = love.graphics:getWidth(), love.graphics:getHeight()
   ship.api.map = ship.api.map or {x=0,y=0}
   love.graphics.setColor(0, 0, 0, 200)
   love.graphics.rectangle("fill", 0, 0, w, h)
   love.graphics.push()
   love.graphics.translate(w/2 + ship.api.map.x*-100, h/2 + ship.api.map.y*100)
   love.graphics.setColor(0, 0, 255)

   for _, data in pairs(ship.systems) do
      if(not data.unmapped) then
         for _, b in ipairs(data.bodies) do
            if(b.portal and not ship.systems[b.portal].unmapped) then
               local target = ship.systems[b.portal]
               love.graphics.line(data.x*100, data.y*-100,
                                  target.x*100, target.y*-100)
            end
         end
      end
   end

   for name, data in pairs(ship.systems) do
      if(not data.unmapped) then
         local x,y = data.x*100, data.y*-100
         local r = data.bodies[1].mass / 15000
         local label_width = name:len() * em
         local color = gov_colors[data.gov]
         love.graphics.setColor(unpack(color))
         love.graphics.circle("fill", x, y, r)
         love.graphics.print(name, x-label_width/2, y+r*1.3)
         if(name == ship.system_name) then
            love.graphics.circle("line", x, y, r*1.3)
         end
         love.graphics.setColor(77, 77, 77)
         love.graphics.circle("line", x, y, r)
      end
   end
   love.graphics.print("Copyright © 2430 Lonesome Planet Publishing" ..
                          ", All Rights Reserved.", -640, 480)
   love.graphics.pop()
end

local climate = { temp = 26, humidity = 0.4 }

local status_target = function(ship, body)
   local body_type = (body.station and "station") or (body.portal and "portal")
      or (body.fixed and "star") or (body.asteroid and "asteroid")
      or (body.ship and "ship") or (body.os and "planet") or "unknown"
   local t = {body.name, body_type, math.floor(utils.distance(ship, body))}
   if(ship.target == body) then table.insert(t, "selected") end
   return t
end

local jeejah_coro = nil
local status_msg = function(ship)
   return {
      id = math.random(999999999), op = "bussard/status",
      x = math.floor(ship.x), y = math.floor(ship.y),
      speed = math.floor(utils.distance(ship.dx, ship.dy)),
      credits = ship.credits, time = utils.format_time(utils.time(ship)),
      fuel = math.floor(ship.fuel / ship.fuel_capacity * 100),
      battery = math.floor(ship.battery / ship.battery_capacity * 100),
      targets = lume.map(ship.bodies, lume.fn(status_target, ship)),
   }
end

return {
   laser = {
      stats = {
         mass = 32,
         laser_power = 64,
         scoop_range = 512,
      },
      action = function(ship, down)
         if(down == "toggle") then
            ship.laser = not ship.laser
         elseif(ship.laser == "off") then
            ship.laser = false
         else
            ship.laser = ship.laser or down
         end
      end,
      update = function(ship, dt)
         if(ship.laser and ship.battery > 0) then
            ship.battery = ship.battery - dt * ship.laser_power * 0.1
            for _,b in pairs(ship.bodies) do
               local dist = utils.distance(ship.x - b.x, ship.y - b.y) / 4
               local power = ship.laser_power * 16
               if(b.asteroid and laser_hits(ship, b, dist)) then
                  b.strength = b.strength - (((dt * power * 32) / (dist*dist)))
                  if(b.strength < 0) then asteroid.split(b, ship) end
               end
            end
         end
      end,
      draw = function(ship)
         if(ship.laser) then
            love.graphics.push()
            love.graphics.rotate(math.pi - ship.heading)
            love.graphics.setLineWidth(3)
            love.graphics.line(0, 0, 0, -1000)
            love.graphics.pop()
            -- I know it's horrible to make state changes in draw, but if we
            -- don't do this we can't do ORing together in the action function
            -- which means you can't have two controls which can fire the laser
            ship.laser = false
         end
      end,
   },

   fuel_charger = {
      stats = { mass = 12 },
      action = function(ship, down)
         ship.api.fuel_charger_on = down
      end,
      update = function(ship, dt)
         if(ship.api.fuel_charger_on and
            ship.fuel < ship.fuel_capacity and ship.battery > 0) then
            ship.fuel = ship.fuel + (ship.recharge_rate * dt * 1/3)
            ship.battery = ship.battery - dt
         end
      end,
   },
   comm_boost = {
      stats = { mass = 16 },
      action = function(ship, on_off)
         if(on_off == "on") then
            ship.comm_boost = true
         elseif(on_off == "off") then
            ship.comm_boost = false
         else
            ship.comm_boost = not ship.comm_boost
         end
      end,
      update = function(ship, dt)
         local boost_multiplier, power_drain = 4, 8
         if(ship.comm_boost and ship.battery > (dt * power_drain)) then
            ship.battery = ship.battery - (dt * power_drain)
            ship.comm_range = ship.base_stats.comm_range * boost_multiplier
         elseif(ship.comm_boost) then
            ship.comm_boost = false
         else
            ship.comm_range = ship.base_stats.comm_range
         end
      end,
   },

   map = {
      stats = {},
      action = map_draw,
   },
   -- purely stat upgrades
   engine = { stats = { engine_strength = 64, burn_rate = 4, mass = 64, } },
   cargo_bay = { stats = { cargo_capacity = 64, mass = 12, } },
   fuel_tank = { stats = { fuel_capacity = 128, mass = 32, } },
   solar_panel = { stats = { solar = 30, mass = 32 }},
   life_support = { stats = { mass = 8 },
                    action = function(_, temp, humidity)
                       if(temp == nil and humidity == nil) then
                          return { temp = climate.temp,
                                   humidity = climate.humidity }
                       else
                          assert(temp < 30 and temp > 15,
                                 "Temperature unsafe for human life.")
                          assert(humidity > 0 and humidity < 1,
                                 "Invalid humidity parameter.")
                          climate.temp, climate.humidity = temp, humidity
                       end
                    end,
                    -- you can sell your life support system, but it's going to
                    -- make any humans you have onboard very annoyed!
                    sell = function(ship)
                       for name in pairs(ship.humans) do
                          ship:disembark(name)
                       end
                    end,
                  },
   jeejah = { stats = { mass = 2 },
              action = function(ship, port)
                 local jeejah = require("jeejah")
                 if(port == "stop") then
                    jeejah.stop(jeejah_coro)
                    ship.api.updaters.jeejah = nil
                    ship.api.long_updaters.jeejah_status = nil
                 elseif(jeejah_coro and
                        coroutine.status(jeejah_coro) == "suspended") then
                    print("Already started.")
                 else
                    jeejah_coro = jeejah.start(nil, port, {sandbox=ship.sandbox
                                                          ,debug=true})
                    ship.api.updaters.jeejah = lume.fn(coroutine.resume,
                                                       jeejah_coro)
                    ship.api.long_updaters.jeejah_status = function()
                       local status = status_msg(ship)
                       jeejah.broadcast(status)
                    end
                 end
              end
            },
   bencode_decoder = { stats = { mass = 0 },
                       action = function(_, data)
                          return require("bencode").decode(data)
                       end
                     },
   underclocker = { stats = { mass = 1 },
                    action = function(ship, data)
                       ship.time_factor = math.min(1e6,
                         math.max(data, ship.base_time_factor))
                    end
                  },
   passponder = {}, -- in order not to explode existing saves
}
