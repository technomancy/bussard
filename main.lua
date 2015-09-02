local body = require "body"
local starfield = require "starfield"
local hud = require "hud"
local ship = require "ship"

local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local bodies = bodies or {}

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local scale = scale or 0.5
local paused = paused or false

calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end

local landing_speed_max = 10

local can_land = function(ship)
   local target = bodies[ship.target]
   local dist_max = target and target.image:getWidth() / 2
   return(target and (not ship.landed) and target.description and
          (calculate_distance(ship.dx - target.dx, ship.dy - target.dy))
          < landing_speed_max and
          (calculate_distance(ship.x - target.x, ship.y - target.y)) <
          dist_max)
end

love.load = function()
  if arg[#arg] == "-debug" then require("mobdebug").start() end
   local font = love.graphics.newFont("jura-demibold.ttf", 20)
   love.graphics.setFont(font)
   bodies = body.load()
   ship.configure(ship.config)
end

love.update = function(dt)
   if(love.keyboard.isDown("=")) then
      scale = scale + (dt / 2)
   elseif(love.keyboard.isDown("-") and scale > dt * 0.5) then
      scale = scale - (dt / 2)
   end

   if(paused or ship.landed) then return end

   if(love.keyboard.isDown("up") and ship.fuel > 0) then
      ship.dx = ship.dx + (math.sin(ship.heading) * dt * ship.engine)
      ship.dy = ship.dy + (math.cos(ship.heading) * dt * ship.engine)
      ship.fuel = ship.fuel - 0.5
   elseif(ship.fuel < 100) then
      ship.fuel = ship.fuel + 0.05
   end

   if(love.keyboard.isDown("left")) then
      ship.heading = ship.heading + (dt * ship.turning)
   elseif(love.keyboard.isDown("right")) then
      ship.heading = ship.heading - (dt * ship.turning)
   end

   -- calculate movement
   ship.x = ship.x + (ship.dx * dt * 100)
   ship.y = ship.y + (ship.dy * dt * 100)

   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 50)
      b.y = b.y + (b.dy * dt * 50)
      if(ship.gravitate) then
         local ddx, ddy = body.gravitate(b, ship.x, ship.y, ship.mass)
         ship.dx = ship.dx + ddx
         ship.dy = ship.dy + ddy
      end
      for _, b2 in ipairs(bodies) do
         local ddx, ddy = body.gravitate(b, b2.x, b2.y, b2.mass)
         b2.theta_v = theta
         b2.dx = b2.dx + ddx
         b2.dy = b2.dy + ddy
      end
   end
end

-- for commands that don't need repeat
love.keypressed = function(key, is_repeat)
   if(key == "return" and can_land(ship)) then
      ship.landed = bodies[ship.target]
   elseif(ship.landed and key == "escape") then ship.landed = false
   elseif(key == "escape") then love.event.push("quit")
   elseif(key == "p") then paused = not paused
   elseif(key == "tab") then
      ship.target = ship.target + 1
      if(ship.target > #bodies) then ship.target = 0 end
   end
end

love.draw = function()
   starfield.render(star1, ship.x, ship.y)
   starfield.render(star2, ship.x, ship.y)
   starfield.render(star3, ship.x, ship.y)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.scale(scale*scale)

   if(bodies[ship.target]) then -- directional target indicator
      love.graphics.setLineWidth(scale*scale*5) -- TODO: scale linearly
      local px, py = bodies[ship.target].x, bodies[ship.target].y
      local dx, dy = px - ship.x, py - ship.y
      love.graphics.setColor(10, 100, 10)
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   love.graphics.setColor(255, 255, 255)
   for i, b in ipairs(bodies) do
      body.draw(b, ship.x, ship.y, i == ship.target)
   end

   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - ship.heading)
   love.graphics.polygon("fill", 0, -30, -20, 50, 20, 50)

   love.graphics.pop()
   love.graphics.setLineWidth(1)

   love.graphics.setColor(255, 255, 255);
   hud.render(ship, bodies[ship.target])
   hud.vector(ship.dx, ship.dy, w - 10 - hud.vector_size, 10)
   if(bodies[ship.target]) then
      local body = bodies[ship.target]
      hud.vector(body.dx, body.dy, w - 10 - hud.vector_size, 70)
   end

   if(ship.landed) then
      love.graphics.setColor(0,0,0, 200);
      love.graphics.rectangle("fill", 100, 100, 400, 300)
      love.graphics.setColor(255, 255, 255);
      love.graphics.rectangle("line", 100, 100, 400, 300)
      love.graphics.print("You landed on " .. ship.landed.name .. "\n\n" ..
                             ship.landed.description, 150, 150)
   end
end
