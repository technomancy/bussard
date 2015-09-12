local utils = require("utils")

local asteroid_image = love.graphics.newImage('assets/asteroid.png')

local min_mass = 10

local function asteroid(name, mass_max, bodies, parent)
   local mass = math.random(mass_max)
   local split = function(self, ship)
      print("Destroyed " .. name)
      bodies[name] = nil
      if(ship.target == self) then ship.target = nil end
      if(self.mass < min_mass) then
         if(utils.distance(ship, self) <= ship.scoop_range) then
            ship.api.repl.print("Scooped up " .. self.name)
            ship:move_cargo("Ore", 10)
         else
            ship.api.repl.print("Destroyed " .. self.name ..
                                   " but out of scoop range.")
         end
      else
         asteroid(name .. "-", self.mass / 8, bodies, self)
         asteroid(name .. "+", self.mass / 8, bodies, self)
      end
   end

   local a = { x = x, y = y, dx = dx, dy = dy, name = name,
               mass = mass, image = asteroid_image,
               asteroid = true, strength = mass, split = split,
   }

   if(parent) then
      -- if two new asteroids spawn in exactly the same place, gravity bugs out
      local o = math.random(20) - 10
      a.x, a.y, a.dx, a.dy = parent.x + o, parent.y + o, parent.dx, parent.dy
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
