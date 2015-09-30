local utils = require "utils"

local filesystem_overlays = require("data.filesystems")

local seed = function(os, body_name)
   local raw = os.fs.new_raw()
   local proxy = os.fs.proxy(raw, "root", raw)
   os.fs.seed(proxy, {guest = ""})

   for k,v in pairs(filesystem_overlays[body_name] or {}) do
      local dir,_ = os.fs.dirname(k)
      os.fs.mkdir(proxy, dir)
      proxy[k] = v
   end
   return raw
end

local filesystems = {}

local g = 1000

-- Without capping gravity accel, you run into a weird bug caused by
-- the fact that we calculate gravitation discretely instead of
-- continuously. If the point of calculation is one at which you are very close
-- to a star, you'll be given a very high velocity, and the next point of
-- calculation won't happen till you're further away, so gravity won't get
-- a chance to slow you back down. This is only noticeable when you pass over
-- what is essentially the heart of a star, but it's very annoying when you do.

local max_accel = 100

local gravitate = function(body, x, y)
   local dx = (x - body.x)
   local dy = (y - body.y)

   local distance = utils.distance(dx, dy)
   local theta = math.atan2(dx, dy) + math.pi

   local accel = math.min((body.mass * g) / (distance * distance), max_accel)
   return (accel * math.sin(theta)), (accel * math.cos(theta))
end

return {
   draw = function(body, x, y)
      body.image = body.image or love.graphics.newImage("assets/" ..
                                                           body.image_name .. ".png")
      local scale = body.scale or 1
      local bx = body.x - body.image:getWidth() * scale / 2
      local by = body.y - body.image:getHeight() * scale / 2
      love.graphics.draw(body.image, bx - x, by - y,
                         -- these are almost always nil
                         body.rotation, scale, scale)
   end,

   gravitate = gravitate,

   gravitate_all = function(bodies, ship, dt)
      ship.x = ship.x + (ship.dx * dt * 100)
      ship.y = ship.y + (ship.dy * dt * 100)
      for _, b in ipairs(bodies) do
         b.x = b.x + (b.dx * dt * 100)
         b.y = b.y + (b.dy * dt * 100)

         local ddx, ddy = gravitate(b, ship.x, ship.y)
         ship.dx = ship.dx + dt * ddx
         ship.dy = ship.dy + dt * ddy

         -- body-to-body
         for _, b2 in ipairs(bodies) do
            if(b ~= b2 and (not b2.star)) then
               local ddx2, ddy2 = gravitate(b, b2.x, b2.y)
               b2.dx = b2.dx + (dt * ddx2)
               b2.dy = b2.dy + (dt * ddy2)
            end
         end
      end
   end,

   -- currently you can log into any body that's not a star
   login = function(body, username, password)
      if((not body) or body.star) then return false end
      filesystems[body.name] = filesystems[body.name] or seed(body.os, body.name)
      return body.os.shell.auth(filesystems[body.name], username, password) and
         filesystems[body.name]
   end,

   schedule = function(bodies)
      for _,b in pairs(bodies) do
         local fs = filesystems[b.name]
         if b.os and fs then b.os.process.scheduler(fs) end
      end
   end,

   seed_cargo = function(b)
      if(not b.prices) then return end
      b.cargo = {}
      for name,info in pairs(b.prices) do
         b.cargo[name] = math.random(info.stock)
      end
   end,

   seed_pos = function(b, star)
      if(b.star or b.asteroid) then return end
      assert(star.star, star.name .. " is not a star.")

      -- Unclear why it's necessary to divide by ten here; standard orbital
      -- calculations do not include this factor, but without it we cannot
      -- achieve orbit.
      local v = math.sqrt((g*star.mass)/b.r) / 10
      local theta = math.random() * math.pi * 2

      b.x, b.y = math.sin(theta) * b.r, math.cos(theta) * b.r
      b.dx = math.sin(theta + math.pi / 2) * v
      b.dy = math.cos(theta + math.pi / 2) * v
   end,

   find = function(bodies,name)
      for _,b in pairs(bodies) do if(b.name == name) then return b end end
   end,

   filesystems = filesystems,
}
