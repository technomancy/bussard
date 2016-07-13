local utils = require("utils")

local base_prices = require("data.prices")
local seed_users = require("data.seed_users")
local portal_motd = "Connected to portal, checking for clearance..."

local hostname = function(body_name)
   return body_name:lower():gsub(" ", "-")
end

local seed = function(os, body_name)
   local raw = os.fs.new_raw()
   local proxy = os.fs.proxy(raw, "root", raw)
   local users = {guest = ""}
   for _,u in pairs(seed_users[body_name] or {}) do
      users[u.username] = u.password
   end
   os.fs.seed(proxy, users)

   for _,user in pairs(seed_users[body_name] or {}) do
      for name,contents in pairs(user.files) do
         local dir,_ = os.fs.dirname(name)
         os.fs.mkdir(proxy, dir)
         proxy[name] = contents
      end
   end

   if(love.filesystem.isFile("data/motd/" .. body_name)) then
      proxy.etc.motd = love.filesystem.read("data/motd/" .. body_name)
   end
   return raw
end

local filesystems = {}

local g = 4196

-- Without capping gravity accel, you run into a weird bug caused by
-- the fact that we calculate gravitation discretely instead of
-- continuously. If the point of calculation is one at which you are very close
-- to a star, you'll be given a very high velocity, and the next point of
-- calculation won't happen till you're further away, so gravity won't get
-- a chance to slow you back down. This is only noticeable when you pass over
-- what is essentially the heart of a star, but it's very annoying when you do.

local max_accel = 10

local gravitate = function(body, x, y)
   local dx = (x - body.x)
   local dy = (y - body.y)

   local distance = utils.distance(dx, dy)
   local theta = math.atan2(dx, dy) + math.pi

   local accel = math.min((body.mass * g) / (distance * distance), max_accel)
   return (accel * math.sin(theta)), (accel * math.cos(theta))
end

local is_gravitated_by = function(from, to)
   if(from == to) then return false
   elseif(to.fixed) then return false
   elseif((to.world or to.portal) and not from.fixed) then return false
   else return true end
end

return {
   draw = function(body, x, y)
      body.image = body.image or love.graphics.newImage("assets/" ..
                                                           body.image_name .. ".png")
      local scale = body.scale or 1
      body.ox = body.ox or body.image:getWidth() * scale / 2
      body.oy = body.oy or body.image:getHeight() * scale / 2
      love.graphics.draw(body.image, body.x - x, body.y - y,
                         body.rotation, scale, scale, body.ox, body.oy)
   end,

   gravitate = gravitate,

   gravitate_all = function(bodies, ship, dt)
      ship.x = ship.x + (ship.dx * dt)
      ship.y = ship.y + (ship.dy * dt)
      for _, b in ipairs(bodies) do
         b.x = b.x + (b.dx * dt)
         b.y = b.y + (b.dy * dt)

         local ddx, ddy = gravitate(b, ship.x, ship.y)
         ship.dx = ship.dx + dt * ddx
         ship.dy = ship.dy + dt * ddy

         -- apply b's gravity to other bodies
         for _, b2 in ipairs(bodies) do
            if(is_gravitated_by(b, b2)) then
               local ddx2, ddy2 = gravitate(b, b2.x, b2.y)
               b2.dx = b2.dx + (dt * ddx2)
               b2.dy = b2.dy + (dt * ddy2)
            end
         end
      end
   end,

   login = function(_, body, username, password)
      if((not body) or not body.os) then return false end
      if(not filesystems[body.name] and body.os.fs and body.os.fs.seed) then
         filesystems[body.name] = seed(body.os, body.name, body.portal)
      end

      if(body.portal) then
         filesystems[body.name].etc.motd = portal_motd
      end

      return body.os.shell.auth(filesystems[body.name], username, password) and
         filesystems[body.name]
   end,

   update = function(bodies, dt)
      for _,b in pairs(bodies) do
         if(b.update) then
            b:update(dt)
         elseif(b.os) then
            local fs = filesystems[b.name]
            if fs then b.os.process.scheduler(fs) end
         end
      end
   end,

   seed_cargo = function(b, no_cargo)
      if(not b.os or b.os.name ~= "orb") then return end
      local equipment_factor = (math.log(b.remote / 2) + 4) *
         (3 / (b.industry + b.tech)) + 0.5

      b.fuel_price = math.ceil(base_prices.fuel *
                                  (math.log(b.remote / 2) + 1) * (5 / b.industry))
      b.account_price = math.floor(base_prices.account *
                                      (math.log(math.max(b.remote, 2) * 0.5) + 1))
      b.upgrade_prices = { life_support = 512 } -- everyone sells this

      for _,u in ipairs(b.upgrades) do
         b.upgrade_prices[u] = math.floor(base_prices.upgrades[u] *
                                             equipment_factor)
      end

      if(no_cargo) then return end

      b.prices = {}
      local price_difference = 1.2 -- should be dynamic?
      local price = function(good, base)
         b.prices[good] = { buy = base, sell = math.ceil(base * price_difference) }
      end
      price("ore", math.floor(base_prices.ore * (math.log(10 - b.mineral) +1)))
      price("food", math.floor(base_prices.food * (math.log(10 - b.agri) +1)))
      price("medicine", math.floor(base_prices.medicine *
                                      (math.log(b.pop / 4 + b.remote + 2) + 1)))
      price("equipment", math.floor(base_prices.equipment * equipment_factor))

      b.cargo = {}
      for _,name in ipairs({"ore", "food", "equipment", "medicine"}) do
         b.cargo[name] = love.math.random(20)
      end

      -- print("\n" .. b.name, equipment_factor)
      -- for k,v in pairs(b.prices) do print(k,v) end
      -- print("fuel", b.fuel_price)
      -- print("account", b.account_price)
      -- for k,v in pairs(b.upgrade_prices) do print(k,v) end
   end,

   seed_pos = function(b, star)
      if(b.fixed) then
         b.x, b.y = 0, 0
         return
      elseif(not b.r) then
         return
      else
         assert(star.fixed, star.name .. " is not a star.")

         local v = math.sqrt((g*star.mass)/b.r)
         local theta = love.math.random() * math.pi * 2

         b.x, b.y = math.sin(theta) * b.r, math.cos(theta) * b.r
         b.dx = math.sin(theta + math.pi / 2) * v
         b.dy = math.cos(theta + math.pi / 2) * v
      end
   end,

   find = function(bodies,name)
      for _,b in pairs(bodies) do if(b.name == name) then return b end end
   end,

   filesystems = filesystems,

   load = function(systems)
      for system_name,system in pairs(systems) do
         for _,body in ipairs(system.bodies) do
            body.system, body.gov = system_name, system.gov
            if(body.portal) then assert(body.os, "OS-less portal") end
            if(body.fixed) then assert(not body.os, "OS on a fixed body") end
         end
      end
   end,

   base_prices = base_prices,

   hostname = hostname,

   g = g,
}
