local utils = require("utils")

local asteroid_image = love.graphics.newImage('assets/asteroid.png')
local asteroid = function(name)
   local x, y, dx, dy = math.random(200000)-100000, math.random(200000)-100000
   local dx, dy = math.random(32) - 16, math.random(32) - 16
   local mass = math.random(64)
   return { x = x, y = y, dx = dx, dy = dy, name = name,
            mass = mass, image = asteroid_image, asteroid = true }
end

return {
   load = function() return require("data/systems") end,

   populate_asteroids = function(system)
      if(not system.asteroids) then return end
      for _,b in pairs(system.bodies) do
         if(b.asteroid) then system.bodies[b.name] = nil end
      end
      for i = 0, system.asteroids do
         table.insert(system.bodies, asteroid("asteroid" .. i))
      end
      return bodies
   end,
}
