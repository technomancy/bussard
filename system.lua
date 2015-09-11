local asteroid_image = love.graphics.newImage('assets/asteroid.png')

local min_mass = 2

local function asteroid(name, mass_max, bodies, parent)
   local mass = math.random(mass_max)
   local split = function(self)
      print("Destroyed " .. name)
      bodies[name] = nil
      if(self.mass < min_mass) then
         print("TODO: collect minerals")
      else
         asteroid(name .. "-", self.mass / 2, bodies, self)
         asteroid(name .. "+", self.mass / 2, bodies, self)
      end
   end

   local a = { x = x, y = y, dx = dx, dy = dy, name = name,
               mass = mass, image = asteroid_image,
               asteroid = true, strength = mass, split = split,
   }

   if(parent) then
      a.x, a.y, a.dx, a.dy = parent.x, parent.y, parent.dx, parent.dy
   else
      a.x, a.y = math.random(200000)-100000, math.random(200000)-100000
      a.dx, a.dy = math.random(32) - 16, math.random(32) - 16
   end

   table.insert(bodies, a)
end

return {
   load = function() return require("data/systems") end,

   populate_asteroids = function(system)
      if(not system.asteroids) then return end
      for _,b in pairs(system.bodies) do
         if(b.asteroid) then system.bodies[b.name] = nil end
      end
      for i = 0, system.asteroids do
         asteroid("asteroid" .. i, 64, system.bodies)
      end
      return bodies
   end,
}
