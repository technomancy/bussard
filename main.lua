local body = require "body"
local starfield = require "starfield"
local hud = require "hud"
local ship = require "ship"

local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local bodies = bodies or {}

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local ui = { quit = function() love.event.push("quit") end,
             scale = 0.5,
             paused = false,
             keyboard = love.keyboard,
           }

local gravitate = function(bodies, ship, dt)
   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 100)
      b.y = b.y + (b.dy * dt * 100)

      local ddx, ddy = body.gravitate(b, ship.x, ship.y)
      ship.dx = ship.dx + dt * ddx / ship.mass
      ship.dy = ship.dy + dt * ddy / ship.mass

      -- body-to-body
      for _, b2 in ipairs(bodies) do
         if(b ~= b2 and (not b2.star)) then
            local ddx2, ddy2 = body.gravitate(b, b2.x, b2.y)
            b2.theta_v = theta
            b2.dx = b2.dx + (dt * ddx2 / b2.mass)
            b2.dy = b2.dy + (dt * ddy2 / b2.mass)
         end
      end
   end
end

love.load = function()
   if arg[#arg] == "-debug" then require("mobdebug").start() end
   -- love.graphics.setDefaultFilter('nearest', 'nearest')
   local font = love.graphics.newFont("mensch.ttf", 14)
   love.graphics.setFont(font)
   bodies = body.load()
   ship:configure(bodies, ui)
end

love.update = function(dt)
   if(ui.paused) then return end
   ship:update(dt)
   body.schedule(bodies)
   gravitate(bodies, ship, dt)
end

-- for commands that don't need repeat
love.keypressed = function(key, is_repeat)
   if(key == "pageup") then ship.dx, ship.dy = 0, 0 end
   if(ship.api.commands[key]) then
      ship.api.commands[key]()
   elseif(not ship.api.controls[key]) then
      ship.api.repl.keypressed(key, is_repeat)
   end
end

love.textinput = function(t)
   if(ship.api.repl.toggled() or (not ship.api.controls[t])) then
      ship.api.repl.textinput(t)
   end
end

love.draw = function()
   starfield.render(star1, ship.x, ship.y)
   starfield.render(star2, ship.x, ship.y)
   starfield.render(star3, ship.x, ship.y)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.push()
   love.graphics.scale(ui.scale*ui.scale)

   if(ship.target) then -- directional target indicator
      if(ship:in_range(ship.target)) then
         love.graphics.setColor(10, 100, 10)
      else
         love.graphics.setColor(100, 100, 100)
      end
      love.graphics.setLineWidth(ui.scale*ui.scale*5)
      local px, py = ship.target.x, ship.target.y
      local dx, dy = px - ship.x, py - ship.y
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   love.graphics.setColor(255, 255, 255)
   for _,b in pairs(bodies) do
      body.draw(b, ship.x, ship.y, b == ship.target)
   end

   love.graphics.pop()

   -- the ship itself
   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - ship.heading)
   love.graphics.polygon("fill", 0, -6, -4, 10, 4, 10)
   if(ship.engine_on) then
      love.graphics.setColor(255, 255, 255);
      love.graphics.setLineWidth(1)
      love.graphics.line(-4, 11, 4, 11)
   end

   love.graphics.pop()

   -- TODO: data-driven hud
   hud.render(ship, ship.target)
   hud.vector(ship.dx, ship.dy, w - 10 - hud.vector_size, 10)
   if(ship.target) then
      -- target velocity
      hud.vector(ship.target.dx, ship.target.dy, w - 10 - hud.vector_size, 70)
      -- target gravitation on ship
      dx, dy = body.gravitate(ship.target, ship.x, ship.y)
      hud.vector(dx, dy, w - 10 - hud.vector_size, 130)
   end

   ship.api.repl.draw()
end
