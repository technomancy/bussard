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

local gravitate = function(bodies, ship, dt)
   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 50)
      b.y = b.y + (b.dy * dt * 50)
      local ddx, ddy = body.gravitate(b, ship.x, ship.y)

      ship.dx = ship.dx + ddx * ship.mass
      ship.dy = ship.dy + ddy * ship.mass

      for _, b2 in ipairs(bodies) do
         if(not b == b2) then
            local ddx, ddy = body.gravitate(b, b2.x, b2.y)
            b2.theta_v = theta
            b2.dx = b2.dx + ddx
            b2.dy = b2.dy + ddy
         end
      end
   end
end

love.load = function()
   if arg[#arg] == "-debug" then require("mobdebug").start() end
   local font = love.graphics.newFont("jura-demibold.ttf", 20)
   love.graphics.setFont(font)
   bodies = body.load()
   ship:configure(bodies)
end

love.update = function(dt)
   -- debug
   if(love.keyboard.isDown("w")) then
      ship.y = ship.y - (dt*1000)
   elseif(love.keyboard.isDown("a")) then
      ship.x = ship.x - (dt*1000)
   elseif(love.keyboard.isDown("s")) then
      ship.y = ship.y + (dt*1000)
   elseif(love.keyboard.isDown("d")) then
      ship.x = ship.x - (dt*1000)
   elseif(love.keyboard.isDown("0")) then
      ship.x, ship.y = 0, 0
   end

   -- zoom
   if(love.keyboard.isDown("=")) then
      scale = scale + (dt / 2)
   elseif(love.keyboard.isDown("-") and scale > dt * 0.5) then
      scale = scale - (dt / 2)
   end

   if(paused) then return end

   ship:update(dt)

   gravitate(bodies, ship, dt)
end

-- for commands that don't need repeat
love.keypressed = function(key, is_repeat)
   if(ship.api.commands[key]) then ship.api.commands[key]()
   elseif(key == "p") then paused = not paused
   end
end

love.draw = function()
   starfield.render(star1, ship.x, ship.y)
   starfield.render(star2, ship.x, ship.y)
   starfield.render(star3, ship.x, ship.y)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.scale(scale*scale)

   if(ship.target) then -- directional target indicator
      love.graphics.setLineWidth(scale*scale*5) -- TODO: scale linearly
      local px, py = ship.target.x, ship.target.y
      local dx, dy = px - ship.x, py - ship.y
      love.graphics.setColor(10, 100, 10)
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   love.graphics.setColor(255, 255, 255)
   for _,b in pairs(bodies) do
      body.draw(b, ship.x, ship.y, b == ship.target)
   end

   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - ship.heading)
   love.graphics.polygon("fill", 0, -30, -20, 50, 20, 50)

   love.graphics.pop()
   love.graphics.setLineWidth(1)

   love.graphics.setColor(255, 255, 255);
   -- TODO: data-driven hud
   hud.render(ship, ship.target)
   hud.vector(ship.dx, ship.dy, w - 10 - hud.vector_size, 10)
   if(ship.target) then
      hud.vector(ship.target.dx, ship.target.dy, w - 10 - hud.vector_size, 70)
   end
end
