local utils = require("utils")
local body = require("body")

local asteroid_image = love.graphics.newImage('assets/asteroid.png')

local min_mass = 10

local max_asteroid_distance = 1000000

local retarget = function(a, ship)
   if(ship.target == a) then
      ship.target, ship.target_number = nil, 0
   end
end

local function asteroid(name, mass_max, bodies, parent)
   local mass = math.random(mass_max)
   local split = function(self, ship)
      for i,b in ipairs(bodies) do
         if(b == self) then table.remove(bodies, i) end
      end

      retarget(self, ship)
      if(self.mass < min_mass) then
         if(utils.distance(ship, self) <= ship.scoop_range) then
            ship.api.repl.print("Scooped up " .. self.name)
            ship:move_cargo("ore", 10, true)
         else
            ship.api.repl.print("Destroyed " .. self.name ..
                                   " but out of scoop range.")
         end
      else
         asteroid(name .. "-", self.mass / 2, bodies, self)
         asteroid(name .. "+", self.mass / 2, bodies, self)
      end
   end

   local a = { name = name, mass = mass, image = asteroid_image,
               scale = (mass / 64) + 0.5,
               asteroid = true, strength = mass, split = split,
   }

   if(parent) then
      -- if two new asteroids spawn in exactly the same place, gravity bugs out
      local o = math.random(20) - 10
      a.x, a.y, a.dx, a.dy = parent.x + o, parent.y + o, parent.dx, parent.dy
   else
      -- try to get them to spawn clustered near the sun with some outliers
      local r = utils.gaussian_random(30000)
      local theta = math.random(math.pi * 2)
      a.x, a.y = math.sin(theta) * r, math.cos(theta) * r
      a.dx, a.dy = math.random(32) - 16, math.random(32) - 16
   end

   table.insert(bodies, a)
end

return {
   -- if asteroids get too far from the player cycle them out and introduce more
   recycle = function(ship)
      local asteroid_count = 0
      for i,b in pairs(ship.bodies) do
         if(b.asteroid) then
            if(utils.distance(b, ship.bodies[1]) > max_asteroid_distance and
               utils.distance(b, ship) > max_asteroid_distance) then
               table.remove(ship.bodies, i)
               retarget(b, ship)
            else
               asteroid_count = asteroid_count + 1
            end
         end
      end
      if(asteroid_count < (ship.systems[ship.system_name].asteroids or 0)) then
         local i = 1
         while(body.find(ship.bodies, "Asteroid " .. i)) do i = i + 1 end
         asteroid("Asteroid " .. i, 64, ship.bodies)
      end
   end,

   populate = function(system)
      if(not system.asteroids) then return end
      for _,b in pairs(system.bodies) do
         if(b.asteroid) then system.bodies[b.name] = nil end
      end
      for i = 1, system.asteroids do
         asteroid("Asteroid " .. i, 64, system.bodies)
      end
   end,
}
