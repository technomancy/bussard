local utils = require "utils"
local orb = require "orb"

local new = function(x, y, dx, dy, mass, image, name, description, star, os)
   return { x = x, y = y, dx = dx, dy = dy,
            mass = mass, image = image, name = name, description = description,
            star = star, ["os"] = os
   }
end

local seed = function(os)
   local raw = os.fs.new_raw()
   os.fs.seed(os.fs.proxy(raw, "root", raw), {guest = ""})
   return raw
end

local filesystems = {}

local g = 100000

return {
   load = function()
      return {new(5000, 0, 0, 5, 5000,
                  love.graphics.newImage('assets/planet-1.png'), "Earth",
                 "This is a pretty great planet.", false, orb),
              new(0, 0, 0, 0, 200000,
                  love.graphics.newImage('assets/sun.png'), "Sol", nil, true)}
   end,

   draw = function(body, x, y)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
   end,

   gravitate = function(body, x, y)
      local dx = (x - body.x)
      local dy = (y - body.y)

      local distance = utils.calculate_distance(dx, dy)
      local theta = math.atan2(dx, dy) + math.pi

      local f = (body.mass * g) / (distance * distance)
      return (f * math.sin(theta)), (f * math.cos(theta))
   end,

   -- currently you can log into any body that's not a star
   login = function(body, username, password)
      if((not body) or body.star) then return false end
      filesystems[body.name] = filesystems[body.name] or seed(body.os)
      return body.os.shell.auth(filesystems[body.name], username, password) and
         filesystems[body.name]
   end,

   schedule = function(bodies)
      for name,b in pairs(bodies) do
         local fs = filesystems[b.name]
         if b.os and fs then b.os.process.scheduler(fs) end
      end
   end,
}
