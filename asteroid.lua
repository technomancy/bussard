local utils = require("utils")
local body = require("body")

local asteroid_image = love.graphics.newImage('assets/asteroid.png')

local min_mass = 10

local max_asteroid_distance = 100000

local retarget = function(a, ship)
   if(ship.target == a) then
      ship.target, ship.target_number = nil, 0
   end
end

local function asteroid(name, mass_max, bodies, parent)
   local mass = math.random(mass_max)
   local split = function(self, ship)
      print("Destroyed " .. name)
      for i,b in ipairs(bodies) do
         if(b == self) then table.remove(bodies, i) end
      end

      retarget(self, ship)
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
               -- TODO: smaller image for smaller asteroids
               mass = mass, image = asteroid_image,
               asteroid = true, strength = mass, split = split,
   }

   if(parent) then
      -- if two new asteroids spawn in exactly the same place, gravity bugs out
      local o = math.random(20) - 10
      a.x, a.y, a.dx, a.dy = parent.x + o, parent.y + o, parent.dx, parent.dy
   else
      a.x = math.random(max_asteroid_distance * 2)-max_asteroid_distance
      a.y = math.random(max_asteroid_distance * 2)-max_asteroid_distance
      a.dx, a.dy = math.random(32) - 16, math.random(32) - 16
   end

   table.insert(bodies, a)
end

return {
   -- if asteroids get too far from the player cycle them out and introduce more
   recycle = function(ship)
      local asteroid_count = 0
      for i,b in pairs(ship.system.bodies) do
         if(b.asteroid) then
            if(utils.distance(b, ship.system.bodies[1]) > max_asteroid_distance) then
               table.remove(ship.system.bodies, i)
               retarget(b, ship)
            else
               asteroid_count = asteroid_count + 1
            end
         end
      end
      if(asteroid_count < ship.system.asteroids) then
         local i = 1
         while(body.find(ship.system.bodies, "asteroid" .. i)) do i = i + 1 end
         asteroid("asteroid" .. i, 64, ship.system.bodies)
      end
   end,

   populate = function(system)
      if(not system.asteroids) then return end
      for _,b in pairs(system.bodies) do
         if(b.asteroid) then system.bodies[b.name] = nil end
      end
      for i = 1, system.asteroids do
         asteroid("asteroid" .. i, 64, system.bodies)
      end
      return bodies
   end,
}
