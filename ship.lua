local ship = { x = -200, y = 0,
               dx = 0, dy = 0,
               heading = math.pi,
               engine = 3,
               turning = 3,
               fuel = 100,
               mass = 1,
               landed = false,
               gravitate = true,
               target = 0,
               config = "ship.controls[\"up\"] = ship.actions.forward\nship.controls[\"left\"] = ship.actions.left\nship.controls[\"right\"] = ship.actions.right\nship.controls[\"enter\"] = ship.actions.land\nship.controls[\"=\"] = ship.actions.zoom\nship.controls[\"-\"] = ship.actions.unzoom"
}

ship.api = {
   sensors = { x = function() return ship.x end,
               y = function() return ship.y end,
               dx = function() return ship.dx end,
               dy = function() return ship.dy end,
               heading = function() return ship.heading end,
               fuel = function() return ship.fuel end,
               landed = function() return ship.landed end,
               bodies = function() return {} end, -- TODO
   },
   controls = {},
   actions = { forward = function() end,
               left = function() end,
               right = function() end,
               land = function() end,
               zoom = function() end,
               unzoom = function() end,
   },
   hud = {},
}

ship.configure = function(config)
   local chunk = assert(loadstring(config))
   local box = { pairs = pairs,
                 ipairs = ipairs,
                 unpack = unpack,
                 print = print, -- TODO: route to HUD
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
end

ship.update = function(dt)
end

return ship