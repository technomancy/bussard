-- need to fix pairs before loading lume
_, lume = require("metatable_monkey"), require("lume")

local starfield = require "starfield"
local body = require "body"
local hud = require "ship.hud"
local ai = require "ship.ai"
local ship = require "ship"
local asteroid = require "asteroid"
local save = require "save"
local pause = require "pause"
local systems = require("data.systems")

local play, stars

local quit = function()
   save.save(ship)
   love.event.quit()
end

local ui = {
   version = "beta-1-prerelease",

   quit = quit,
   abort = function(confirm)
      if(not confirm) then
         return("Aborting will wipe your in-process game. Call " ..
                   "abort(true) to confirm.")
      end
      save.abort(ship)
      love.event.quit()
   end,
   config_reset = lume.fn(save.config_reset, ship),

   pause = function() pause(play, quit) end,

   get_fps = love.timer.getFPS,
}

local safely = function(f)
   return function(...)
      -- if true then return f(...) end
      local traceback, err
      local ok, ret = xpcall(lume.fn(f, ...), function(e)
                                traceback, err = debug.traceback(), e
      end)
      if(ok) then return ret end

      print(traceback:gsub("\n[^\n]+$", ""))
      love.draw = function()
         local w, h = love.graphics:getWidth(), love.graphics:getHeight()
         love.graphics.setColor(50, 50, 200)
         love.graphics.rectangle("fill", 0, 0, w, h)
         love.graphics.setColor(255, 255, 255)
         love.graphics.print("Error: " .. err, 100, 100)
         love.graphics.print("Press Enter to save and quit.", 100, 200)
         love.graphics.print("Press Esc to quit without saving.", 100, 300)
         love.graphics.print("Press Ctrl-R to revert to stock config.", 100, 400)
         love.graphics.print("Press Ctrl-Q to wipe your save game.", 100, 500)
         love.graphics.print("Please consider reporting this crash to " ..
                                "help improve the game!", 200, 600)
      end
      love.keypressed = function(key)
         if(key == "return") then ui.quit(ui)
         elseif(key == "escape") then love.event.quit()
         elseif(key == "q" and love.keyboard.isDown("lctrl", "rctrl")) then
            save.abort(ship)
            love.event.quit()
         elseif(key == "r" and love.keyboard.isDown("lctrl", "rctrl")) then
            save.revert(ship)
         end
      end
      love.update = function() end
   end
end

love.load = function()
   local dw, dh = love.window.getDesktopDimensions()

   if(dh < 800) then
      love.window.setMode(dw, dh, {fullscreen=true, fullscreentype="desktop"})
   else
      love.window.setMode(dw*0.9, dh*0.9)
   end

   love.graphics.setFont(love.graphics.newFont("assets/mensch.ttf", 14))

   stars = { starfield.new(10, 0.01, 100),
             starfield.new(10, 0.05, 175),
             starfield.new(10, 0.1, 255), }

   if(arg[#arg] == "-debug") then require("mobdebug").start() end
   love.keyboard.setKeyRepeat(true)
   ship:configure(systems, ui)
   if arg[#arg] == "-abort" then save.abort(ship) end
   save.load_into(ship)
   body.load(systems)
   ship.api.editor.initialize()

   if(love.filesystem.isFile("localhacks.lua")) then
      require("localhacks")(ship)
   end

   ship:dofile("src.config")
end

local update = safely(function(dt)
      if(ship.api.paused) then return end
      local real_time_factor = ship.time_factor * 0.001 * dt
      ship:update(real_time_factor)
      body.update(ship.bodies, dt)
      body.gravitate_all(ship.bodies, ship, real_time_factor)
      asteroid.recycle(ship)
      ai.update(ship.bodies, dt)
end)

-- for commands that don't need repeat
local keypressed = safely(lume.fn(ship.handle_key, ship))

local textinput = safely(lume.fn(ship.textinput, ship))

-- drawing

local portal_offsets = {
   {0, -200}, {-141, -141}, {-200, 0}, {-141, 141},
   {0, 200}, {141, 141}, {200, 0}, {141, -141},
}

local draw = safely(function(dt)
      for _,s in pairs(stars) do starfield.render(s, ship.x, ship.y) end

      love.graphics.push()
      love.graphics.translate(love.graphics.getWidth() / 2,
                              love.graphics.getHeight() / 2)
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
            love.graphics.setColor(10, 200, 10)
         elseif(ship.target.asteroid and
                ship:in_range(ship.target, ship.scoop_range)) then
            love.graphics.setColor(100, 10, 10)
         elseif(ship.target.portal and
                ship:in_range(ship.target, ship.portal_range)) then
            love.graphics.setColor(10, 10, 100)
         elseif(ship.target.os) then
            love.graphics.setColor(80, 120, 80)
         else
            love.graphics.setColor(100, 100, 100)
         end
         love.graphics.setLineWidth(5*scale)
         local dx, dy = ship.target.x - ship.x, ship.target.y - ship.y
         love.graphics.line(0, 0, dx, dy)
         love.graphics.setLineWidth(1)
      end

      hud.trajectory(ship, ship.bodies, ship.api.trajectory,
                     ship.api.trajectory_step_size,
                     {190, 190, 255}, {99, 99, 168}, {90, 90, 155}, {60, 60, 102})

      love.graphics.setColor(255, 255, 255)
      for _,b in pairs(ship.bodies) do
         body.draw(b, ship.x, ship.y, b == ship.target)
      end

      if(ship.target and ship.target.beam_count) then
         love.graphics.setLineWidth(10)
         for i = 1,8 do
            if(ship.target.beam_count > i) then
               love.graphics.line(0,0,
                                  ship.portal_target.x - ship.x +
                                     portal_offsets[i][1],
                                  ship.portal_target.y - ship.y +
                                     portal_offsets[i][2])
            end
         end
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
      local mode = ship.api:mode()
      local draw = (mode and mode.draw) or ship.api.editor.draw
      draw(ship)
end)

play = function(first_time)
   -- first time through love.graphics hasn't loaded yet; this will segfault
   if(not first_time) then
      love.graphics.setFont(love.graphics.newFont("assets/mensch.ttf", 14))
   end
   love.update,love.keypressed,love.textinput,love.draw =
      update, keypressed, textinput, draw
end

play(true)

return ship -- for headless.lua
