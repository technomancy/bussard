local utils = require "utils"
local orb = require "os/orb"

local new = function(x, y, dx, dy, mass, image, name, star, os)
   return { x = x, y = y, dx = dx, dy = dy,
            mass = mass, image = image, name = name, star = star, ["os"] = os
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
      return {new(0, 0, 0, 0, 200000,
                  love.graphics.newImage('assets/sun.png'), "Wolf 294", nil, true),
              new(30000, 27000, -5, 5, 100,
                  love.graphics.newImage('assets/station-1.png'),
                  "Trine station", false, orb)}
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

   escape_velocity = function(body, escapee)
      local distance = utils.calculate_distance(body.x - escapee.x,
                                                body.y - escapee.y)
      return math.sqrt(2*g*body.mass / distance)
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
