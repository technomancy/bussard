local starfield = require "starfield"
local body = require "body"
local hud = require "hud"
local ship = require "ship"
local asteroid = require "asteroid"
local systems = require "data/systems"

local save = require "save"
local keymap = require "keymap"

local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local ui = {
   quit = function(ui)
      save.save(ship, ui)
      love.event.quit()
   end,
   abort = function(confirm)
      if(not confirm) then
         return("Aborting will wipe your in-process game. Call " ..
                   "abort(true) to confirm.")
      end
      save.abort(ship)
      love.event.quit()
   end,
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
   local font = love.graphics.newFont("mensch.ttf", 14)
   love.graphics.setFont(font)
   love.keyboard.setKeyRepeat(true)
   ship:configure(systems, ui)
   save.load_into(ship)
   ship.api.load(ship.api)
   ship.api.repl.last_result = ship.api.repl.last_result or
      "Press control-enter to open the repl and `man()` for more help."
end

love.update = function(dt)
   if(ship.api.paused) then return end
   ship:update(dt * time_factor)
   body.schedule(ship.bodies)
   asteroid.recycle(ship)
   gravitate(ship.bodies, ship, dt * time_factor)
end

-- for commands that don't need repeat
love.keypressed = keymap.handle

love.textinput = keymap.textinput

love.draw = function(dt)
   starfield.render(star1, ship.x, ship.y)
   starfield.render(star2, ship.x, ship.y)
   starfield.render(star3, ship.x, ship.y)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.push()

   for _,u in pairs(ship.upgrades) do
      if(u.draw) then u.draw(ship, dt) end
   end

   local scale = math.pow(1/ship.api.scale, 8)
   love.graphics.scale(scale)

   if(ship.target) then -- directional target indicator
      if(ship:in_range(ship.target) and ship.target.os) then
         love.graphics.setColor(10, 100, 10)
      elseif(ship:in_range(ship.target, ship.scoop_range) and
             ship.target.asteroid) then
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

   hud.trajectory(ship, ship.bodies, ship.api.trajectory,
                  ship.api.trajectory_step_size, {150, 150, 255})

   if(ship.target) then
      love.graphics.push()
      love.graphics.translate(ship.target.x - ship.x, ship.target.y - ship.y)
      hud.trajectory(ship.target, ship.bodies, ship.api.trajectory,
                     ship.api.trajectory_step_size, {90, 90, 155})
      love.graphics.pop()
   end

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
   ship.api.edit.draw()
end

return ship -- for headless.lua
