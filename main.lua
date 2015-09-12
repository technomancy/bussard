local starfield = require "starfield"
local body = require "body"
local hud = require "hud"
local ship = require "ship"
local asteroid = require "asteroid"
local systems = require "data/systems"
local save = require "save"

local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local ui = {
   -- TODO: restart function
   quit = function(ui)
      save.save(ship, ui)
      love.event.push("quit")
   end,
   -- this stuff should be moved to a subtable of ship
   scale = 1, scale_min = 0.1,
   paused = false,
}

local time_factor = 1

local gravitate = function(bodies, s, dt)
   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 100)
      b.y = b.y + (b.dy * dt * 100)

      local ddx, ddy = body.gravitate(b, s.x, s.y)
      s.dx = s.dx + dt * ddx
      s.dy = s.dy + dt * ddy

      -- body-to-body
      for _, b2 in ipairs(bodies) do
         if(b ~= b2 and (not b2.star)) then
            local ddx2, ddy2 = body.gravitate(b, b2.x, b2.y)
            b2.dx = b2.dx + (dt * ddx2)
            b2.dy = b2.dy + (dt * ddy2)
         end
      end
   end
end

love.load = function()
   if arg[#arg] == "-debug" then require("mobdebug").start() end
   -- love.graphics.setDefaultFilter('nearest', 'nearest')
   local font = love.graphics.newFont("mensch.ttf", 14)
   love.graphics.setFont(font)
   ship:configure(systems, ui)
   save.load_into(ship, ui)
   ship.api.repl.last_result =
      "Press control-` to open the repl or just start typing code."
end

love.update = function(dt)
   if(ui.paused) then return end
   ship:update(dt * time_factor)
   body.schedule(ship.bodies)
   asteroid.recycle(ship)
   gravitate(ship.bodies, ship, dt * time_factor)
end

-- for commands that don't need repeat
love.keypressed = function(key, is_repeat)
   if(key == "pageup" and (not ship.api.repl.toggled())) then
      ship.dx, ship.dy = 0, 0 -- cheat
   elseif(ship.api.commands[key]) then
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

   if(ship.laser) then
      love.graphics.push()
      love.graphics.rotate(math.pi - ship.heading)
      love.graphics.setLineWidth(3)
      love.graphics.line(0, 0, 0, -1000)
      love.graphics.pop()
   end

   if(ui.scale < 1) then ui.scale = 1 end
   local scale = math.pow(1/ui.scale, 8)
   love.graphics.scale(scale)

   if(ship.target) then -- directional target indicator
      if(ship:in_range(ship.target) and ship.target.os) then
         love.graphics.setColor(10, 100, 10)
      elseif(ship:in_range(ship.target, ship.scoop_range) and ship.target.asteroid) then
         love.graphics.setColor(100, 10, 10)
      else
         love.graphics.setColor(100, 100, 100)
      end
      love.graphics.setLineWidth(5*scale)
      local px, py = ship.target.x, ship.target.y
      local dx, dy = px - ship.x, py - ship.y
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   hud.trajectory(ship, ship.bodies, ship.api.trajectory)

   love.graphics.setColor(255, 255, 255)
   for _,b in pairs(ship.bodies) do
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
   hud.render(ship, ship.target, ui.scale)
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

return ship -- for headless.lua
