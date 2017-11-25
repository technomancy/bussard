local lume = require("lume")
local utils = require("utils")
local body = require("body")

local max_asteroid_distance = 1000000

local function asteroid(name, mass_max, bodies)
   local mass = love.math.random(mass_max)

   local a = { name = name, mass = mass,
               image_name = "asteroid",
               scale = (mass / 64) + 0.5,
               asteroid = true, strength = mass
   }

   -- try to get them to spawn clustered near-ish the sun with some outliers
   local min_r = math.sqrt(bodies[1].mass*body.g/body.max_accel)*1.5
   local r = math.max(min_r, 30000)
   r = love.math.randomNormal(r, 2*r)
   local base_v = math.sqrt((body.g*bodies[1].mass)/math.abs(r))
   -- use the same orbit logic as planets, but with random eccentricity
   local eccentricity = love.math.random() / 2 + 0.5
   local v = base_v * eccentricity
   local theta = love.math.random() * math.pi * 2

   a.x, a.y = math.sin(theta) * r, math.cos(theta) * r
   a.dx = math.sin(theta + math.pi / 2) * v
   a.dy = math.cos(theta + math.pi / 2) * v

   table.insert(bodies, a)
end

return {
   -- if asteroids get too far from the player cycle them out and introduce more
   recycle = function(ship)
      local asteroid_count = 0
      for _,b in pairs(ship.bodies) do
         if(b.asteroid) then
            if(utils.distance(b, ship.bodies[1]) > max_asteroid_distance and
               utils.distance(b, ship) > max_asteroid_distance) then
               ship:remove_body(b)
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
      for i,b in lume.ripairs(system.bodies) do
         if(b.asteroid) then table.remove(system.bodies, i) end
      end
      for i = 1, system.asteroids do
         asteroid("Asteroid " .. i, 64, system.bodies)
      end
   end,
}
