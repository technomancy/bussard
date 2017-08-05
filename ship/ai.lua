local utils = require("utils")
local body = require("body")

local names = lume.array(love.filesystem.lines("data/ships.txt"))

local normalize = function(t)
   return ((t + math.pi) % (math.pi * 2)) - math.pi
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
      if(not (math.abs(theta_v - theta) < 1 and dv > self.speed_limit)) then
         self.rotation = theta
         local fx = (math.sin(self.rotation) * dt * self.engine_strength)
         local fy = (math.cos(self.rotation) * dt * self.engine_strength)
         self.dx = self.dx + fx / self.mass
         self.dy = self.dy + fy / self.mass
      end
      self.projection = self.projection + 0.001
   elseif(self.projection > 1) then
      self.projection = self.projection - 0.25
   else -- we've been here a while now
      if(self.target.portal) then -- portal on out
         self:remove()
      else -- find a portal to target
         -- TODO: we should exchange some cargo with the station/planet
         local portals = lume.filter(self.bodies, "portal")
         self.target = portals[love.math.random(#portals)]
         self.target_name = self.target.name
         self.projection = 60
      end
   end
end

local draw = function(ship, sx, sy)
   love.graphics.push()
   love.graphics.setColor(50, 120, 50);
   love.graphics.translate(ship.x-sx, ship.y-sy)
   love.graphics.scale(ship.mass / 2)
   love.graphics.rotate(math.pi - (ship.rotation or 0))
   love.graphics.polygon("fill", 0, -6, -4, 10, 4, 10)
   if(ship.engine_on) then
      love.graphics.setColor(255, 255, 255);
      love.graphics.setLineWidth(1)
      love.graphics.line(-4, 11, 4, 11)
   end
   love.graphics.setColor(255,255,255);
   love.graphics.pop()
end

local make = function(ship, bodies, name, from_portal)
   local targets = lume.filter(bodies, "os")
   local target, from = targets[love.math.random(#targets)]

   if(from_portal) then
      local portals = lume.filter(bodies, "portal")
      from = portals[love.math.random(#portals)]
   else
      from = targets[love.math.random(#targets)]
   end

   while target == from do target = targets[love.math.random(#targets)] end

   return {
      ship = true,
      update = update,
      draw = draw,

      x = from.x,
      y = from.y,
      dx=0, dy=0,
      mass = love.math.randomNormal(24, 64),
      name = name,
      bodies = targets,
      engine_strength = 512,
      projection = 60,
      target_range = 1000,
      speed_limit = 5,

      target = target, target_name = target.name,
      from = from, from_name = from.name,
      progress = love.math.random(90),
      remove = lume.fn(ship.remove_body, ship),
   }
end

local insert_new = function(ship, bodies, from_portal)
   local s = make(ship, bodies, names[love.math.random(#names)], from_portal)
   table.insert(bodies, s)
end

local sys_pop = function(bodies)
   return lume.reduce(bodies, function(p, b) return p + (b.pop or 0) end, 0)
end

local update_counter = 0

-- for every 1 population in a system, there should be this many ships:
local ship_factor = 0.3

local ship_spawn_period = 160

return {
   seed = function(ship)
      for i,b in ipairs(ship.bodies) do -- remove existing ships
         if(b.ship) then table.remove(ship.bodies, i) end
      end
      local ship_count = love.math.random(math.ceil(sys_pop(ship.bodies)
                                                       * ship_factor))
      for _ = 1, ship_count do insert_new(ship, ship.bodies) end
   end,

   update = function(ship, bodies, dt)
      -- move this to long_update in ship update loop?
      if(update_counter < ship_spawn_period) then
         update_counter = update_counter + dt
      else
         update_counter = 0
         local ship_count = lume.count(bodies, "ship")
         if(ship_count < (love.math.random() * sys_pop(bodies) * ship_factor)) then
            insert_new(ship, bodies, true)
         end
      end
   end,

   make = make,
}
