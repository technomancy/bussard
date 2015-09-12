local utils = require "utils"

local seed = function(os)
   local raw = os.fs.new_raw()
   os.fs.seed(os.fs.proxy(raw, "root", raw), {guest = ""})
   return raw
end

local filesystems = {}

local g = 1000

return {
   draw = function(body, x, y)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
   end,

   gravitate = function(body, x, y)
      local dx = (x - body.x)
      local dy = (y - body.y)

      local distance = utils.distance(dx, dy)
      local theta = math.atan2(dx, dy) + math.pi

      local f = (body.mass * g) / (distance * distance)
      return (f * math.sin(theta)), (f * math.cos(theta))
   end,

   escape_velocity = function(body, escapee)
      local distance = utils.distance(body.x - escapee.x,
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
}
