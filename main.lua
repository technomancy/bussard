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

local stars

local quit = function()
   save.save(ship)
   love.event.quit()
end

love.quit = function()
   save.save(ship)
   return false
end

local font_path = "assets/fonts/inconsolata.ttf"

local resize = function()
   local dw, dh = love.window.getDesktopDimensions()
   local w, h = dw*0.9, dh*0.9

   if(love.filesystem.isFile("window")) then
      local wh = lume.split(love.filesystem.read("window"), " ")
      w, h = tonumber(wh[1]), tonumber(wh[2])
   end

   if(love.filesystem.isFile("fullscreen")) then
      if(love.filesystem.read("fullscreen") == "true") then
         love.window.setMode(dw, dh, {fullscreen=true,
                                      fullscreentype="desktop"})
      else
         love.window.setMode(w, h, {resizable=true})
      end
   else
      if(dh < 800) then
         love.window.setMode(dw, dh, {fullscreen=true, fullscreentype="desktop"})
      else
         love.window.setMode(w, h, {resizable=true})
      end
   end
end

local ui = {
   version = "beta-2-pre",

   quit = quit,
   abort = function(confirm)
      if(confirm ~= true) then
         return("Aborting will wipe your in-process game. Call " ..
                   "abort(true) to confirm.")
      end
      save.abort()
      love.event.quit()
   end,
   config_reset = lume.fn(save.config_reset, ship),

   get_fps = love.timer.getFPS,
   powersave = love.system.getPowerInfo() == "battery",
}

ui.pause = function() pause(ui.play, quit, resize, font_path) end
ui.set_font = function(path, size)
   -- if size is nil, assume path is size
   local font
   if(size == nil) then
      font = love.graphics.newFont(font_path, path)
   else
      font_path = path
      font = love.graphics.newFont(path, size)
   end
   love.graphics.setFont(font)

   -- love 0.9.x doesn't support this
   local noto = love.graphics.newFont("assets/fonts/noto-thai.ttf", 16)
   if(font.setFallbacks) then font:setFallbacks(noto) end
end

local safely = function(f)
   return function(...)
      -- if true then return f(...) end
      local traceback, err
      local ok, ret = xpcall(lume.fn(f, ...), function(e)
                                traceback, err = debug.traceback(), e
      end)
      if(ok) then return ret end

      print(err)
      print(traceback:gsub("\n[^\n]+$", ""))
      love.draw = function()
         local w, h = love.graphics:getWidth(), love.graphics:getHeight()
         love.graphics.setColor(50, 50, 200)
         love.graphics.rectangle("fill", 0, 0, w, h)
         love.graphics.setColor(255, 255, 255)
         love.graphics.print("Error: " .. err, 100, 100)
         love.graphics.print("Press Enter to save and quit.", 100, 150)
         love.graphics.print("Press Esc to quit without saving.", 100, 200)
         love.graphics.print("Press Ctrl-R to revert to stock config.", 100, 250)
         love.graphics.print("Press Ctrl-W to wipe your save game.", 100, 300)
         love.graphics.print("Please consider reporting this crash to " ..
                                "help improve the game: ", 100, 400)
         love.graphics.print("https://gitlab.com/technomancy/bussard", 100, 450)
      end
      love.keypressed = function(key)
         if(key == "return") then ui.quit(ui)
         elseif(key == "escape") then love.event.quit()
         elseif(key == "w" and love.keyboard.isDown("lctrl", "rctrl")) then
            save.abort(ship)
            love.event.quit()
         elseif(key == "r" and love.keyboard.isDown("lctrl", "rctrl")) then
            save.config_reset(ship)
         end
      end
      love.update = function() end
   end
end

love.load = function()
   if(arg[#arg] == "--test") then save.abort(ship) end

   ship:configure(systems, ui)
   save.load_into(ship)
   ship:dofile("src.config")
   if(love.filesystem.isFile("localhacks.lua")) then
      require("localhacks")(ship)
   end

   if(arg[#arg] == "-debug") then require("mobdebug").start() end
   if(arg[#arg] == "--wipe") then save.abort(ship) love.event.quit() end
   if(arg[#arg] == "--test") then return require("tests") end
   if(arg[#arg] == "--fuzz") then return require("tests.fuzz") end

   resize()
   ui.set_font(16)

   stars = { starfield.new(10, 0.01, 100),
             starfield.new(10, 0.05, 175),
             starfield.new(10, 0.1, 255), }
   love.keyboard.setKeyRepeat(true)

   ui.play()
end

love.resize = function(w,h) love.filesystem.write("window", w .. " " .. h) end

local update = safely(function(dt)
      local real_time_factor = ship.time_factor * dt
      ship:update(real_time_factor)
      body.update(ship.bodies, dt)
      body.gravitate_all(ship.bodies, ship, real_time_factor)
      asteroid.recycle(ship)
      ai.update(ship.bodies, dt)
end)

-- for commands that don't need repeat
local keypressed = safely(function(key)
      -- need hard-coded reset for recovering from bad config bugs
      if(key == "f1" and love.keyboard.isDown("lctrl", "rctrl")) then
         ship.api.ui.config_reset()
      else
         ship.api.editor.handle_key(key)
      end
end)

local wheelmoved = safely(ship.api.editor.handle_wheel)

local textinput = safely(ship.api.editor.handle_textinput)

-- drawing

local portal_offsets = {
   {0, -200}, {-141, -141}, {-200, 0}, {-141, 141},
   {0, 200}, {141, 141}, {200, 0}, {141, -141},
}

local draw = safely(function(dt)
      local w,h = love.window.getMode()
      for _,s in pairs(stars) do starfield.render(s, ship.x, ship.y, w, h) end

      love.graphics.push()
      love.graphics.translate(w/2, h/2)
      love.graphics.push()

      local scale = math.pow(2/ship.api.scale, 8)
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

      -- save battery by disabling trajectory when not in flight mode
      if(ship.api:mode().name == "flight") then
        hud.trajectory(ship, ship.bodies, ship.api.trajectory,
                       ship.api.trajectory_step_size,
                       {190, 190, 255}, {99, 99, 168},
                       {90, 90, 155}, {60, 60, 102})
      end

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

      if(ship.target and ship.target.beam_count and
         ship.target.beam_count > 8) then -- portal flash
         local flash = (ship.target.beam_count - 8) * 255
         love.graphics.setColor(255,255,255, flash)
         love.graphics.rectangle("fill", -w, -h, w*2, h*2)
      end

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

      local ok, err = pcall(function() hud.render(ship) end)
      if(not ok) then
         ship.api.print("HUD rendering error: " .. err)
         ship.api.print("Resetting HUD back to stock.")
         -- of course this is not perfect; other files can modify ship.hud
         ship.api.src.bak = ship.api.src.bak or {}
         ship.api.src.bak.hud = ship.api.src.hud
         ship.api.src.hud = love.filesystem.read("data/src/hud")
         ship.api.dofile("src.hud")
      end

      ship.api.editor.draw(ship)
end)

ui.play = function()
   ui.set_font(16)
   love.update,love.keypressed,love.wheelmoved,love.textinput,love.draw =
      update, keypressed, wheelmoved, textinput, draw
end

return ship -- for headless.lua
