local systems = require("data.systems")
local utils = require("utils")
local body = require("body")

-- TODO: rotation of this is wrong
local image = love.graphics.newImage("assets/ship.png")

local normalize = function(t)
   return math.mod(t + math.pi, math.pi * 2) - math.pi
end

local update = function(self, dt)
   if(not self.from) then self.from = body.find(self.bodies, self.from_name) end
   if(not self.target) then self.from = body.find(self.bodies, self.target_name) end
   local tx = self.target.x + self.target.dx * self.projection
   local ty = self.target.y + self.target.dy * self.projection
   local sx = self.x + self.dx * self.projection
   local sy = self.y + self.dy * self.projection
   local theta = normalize(-math.atan2(ty - sy, tx - sx) + math.pi/2)
   local dist = utils.distance({x=sx, y=sy}, {x=tx,y=ty})
   if(dist > self.target_range) then
      local theta_v = normalize(-math.atan2(self.dy, self.dx) + math.pi/2)
      local v = utils.distance(self.dx, self.dy)
      local dv = v - utils.distance(self.target.dx, self.target.dy)
      if(math.abs(theta_v - theta) < 1 and dv > self.speed_limit) then
      else
         self.rotation = theta
         local fx = (math.sin(self.rotation) * dt * self.engine_strength)
         local fy = (math.cos(self.rotation) * dt * self.engine_strength)
         self.dx = self.dx + fx / self.mass
         self.dy = self.dy + fy / self.mass
      end
      self.projection = self.projection + 0.001
   elseif(self.projection > 1) then
      self.projection = self.projection - 0.25
   end
end

local make_ship = function(targets, i)
   local target = targets[math.random(#targets)]
   local from = targets[math.random(#targets)]
   while target == from do from = targets[math.random(#targets)] end

   return {
      ship = true,
      update = update,
      image = image,
      scale = 10,

      x = from.x,
      y = from.y,
      dx=0, dy=0,
      mass = 128,
      name = "Ship " .. i,
      bodies = targets,
      engine_strength = 512,
      projection = 60,
      target_range = 1000,
      speed_limit = 10,

      target = target, target_name = target.name,
      from = from, from_name = from.name,
      progress = math.random(90)
   }
end

return {
   seed = function(system_name, bodies)
      local pops = {}
      for i,b in ipairs(bodies) do -- remove existing ships
         if(b.ship) then table.remove(bodies, i) end
         if(b.pop) then table.insert(pops, b.pop) end
      end
      local system = systems[system_name]
      local avg_pop = lume.reduce(pops, function(x, y) return x + y end)
      local ship_count = math.random(avg_pop / 2)
      local targets = lume.filter(bodies, function(b) return b.os end)
      for i = 1, ship_count do
         local ship = make_ship(targets, i)
         table.insert(bodies, ship)
         ship:update(0)
      end
   end,
}
