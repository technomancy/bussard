-- need to fix pairs before loading lume
_, lume = require("metatable_monkey"), require("lume")

local starfield = require "starfield"
local body = require "body"
local hud = require "ship.hud"
local ship = require "ship"
local asteroid = require "asteroid"
local save = require "save"
local keymap = require "keymap"

local w, h = love.graphics:getWidth(), love.graphics:getHeight()
local systems = require("data.systems")

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local ui = {
   version = "alpha-5-pre",

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

love.load = function()
   if arg[#arg] == "-debug" then require("mobdebug").start() end
   local font = love.graphics.newFont("assets/mensch.ttf", 14)
   love.graphics.setFont(font)
   love.keyboard.setKeyRepeat(true)
   ship:configure(systems, ui)
   if arg[#arg] == "-abort" then save.abort(ship) end
   save.load_into(ship)
   body.load(systems)

   ship.api.repl.display_line =
      "Press ~ to open the repl, and run man() for more help."
   xpcall(function() ship.api:load("src.config") end,
      function(e)
         print("Initial load failed:", e)
         s.repl.print(e)
         s.repl.print(debug.traceback())
         s.repl.display_line = "Error loading config; falling back to " ..
            "ship.src.fallback_config."
         local chunk = assert(loadstring("src.fallback_config"))
         setfenv(chunk, ship.api.repl.sandbox)
         local success, msg = pcall(chunk)
         if(not success) then
            s.repl.print(msg)
         end
   end)
end

love.update = function(dt)
   if(ship.api.paused) then return end
   local real_time_factor = ship.time_factor * 0.1 * dt
   ship:update(real_time_factor)
   body.schedule(ship.bodies)
   asteroid.recycle(ship)
   body.gravitate_all(ship.bodies, ship, real_time_factor)
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

   local scale = math.pow(1/ship.api.scale, 8)
   love.graphics.scale(scale)

   for _,u in pairs(ship.upgrades) do
      if(u.draw) then u.draw(ship, dt) end
   end

   if(ship.target) then -- directional target indicator
      -- you can log into portals, but this isn't obvious at first
      if(ship:in_range(ship.target) and ship.target.os and
         not ship.target.portal) then
         love.graphics.setColor(10, 100, 10)
      elseif(ship.target.asteroid and
             ship:in_range(ship.target, ship.scoop_range)) then
         love.graphics.setColor(100, 10, 10)
      elseif(ship.target.portal and
             ship:in_range(ship.target, ship.passponder_range)) then
         love.graphics.setColor(10, 10, 100)
      else
         love.graphics.setColor(100, 100, 100)
      end
      love.graphics.setLineWidth(5*scale)
      local dx, dy = ship.target.x - ship.x, ship.target.y - ship.y
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   -- TODO: how can we move this to hud?
   hud.trajectory(ship, ship.bodies, ship.api.trajectory,
                  ship.api.trajectory_step_size, {190, 190, 255}, {99, 99, 168})

   if(ship.target and not ship.target.star) then
      love.graphics.push()
      love.graphics.translate(ship.target.x - ship.x, ship.target.y - ship.y)
      hud.trajectory(ship.target, ship.bodies, ship.api.trajectory,
                     ship.api.trajectory_step_size,
                     {90, 90, 155}, {60, 60, 102})
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

   hud.render(ship)
   ship.api.repl.draw()
   ship.api.edit.draw()
end

return ship -- for headless.lua
