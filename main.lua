-- need to fix pairs before loading lume
_, lume = require("metatable_monkey"), require("lume")

local body = require("body")
local ai = require("ship.ai")
local ship = require("ship")
local save = require("save")
local pause = require("pause")
local flight_draw = require("draw")
local systems = require("data.systems")
local os_client = require("os.client")

local quit = function()
   ship.api.editor.set_prompt("> ")
   ship.api.editor.newline()
   ship.api.editor.print_prompt()
   save.save(ship)
   love.event.quit()
end

local is_ctrl = function()
   return (love.keyboard.isScancodeDown or love.keyboard.isDown)("lctrl", "rctrl", "capslock")
end

love.quit = function()
   save.save(ship)
   for _,b in ipairs(ship.bodies) do body.stop(b) end
   return false
end

local font_path, font_size = "assets/fonts/inconsolata.ttf", 16

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
                                      fullscreentype="desktop",
                                      highdpi=true,})
      else
         love.window.setMode(w, h, {resizable=true, highdpi=true,})
      end
   else
      if(dh < 800) then
         love.window.setMode(dw, dh, {fullscreen=true,
                                      fullscreentype="desktop",
                                      highdpi=true,})
      else
         love.window.setMode(w, h, {resizable=true, highdpi=true,})
      end
   end
end

local ui = {
   version = "beta-3",

   quit = quit,
   abort = function(confirm)
      if(confirm ~= true) then
         return("Aborting will wipe your in-process game. Call " ..
                   "abort(true) to confirm.")
      end
      save.abort(ship)
      love.event.quit()
   end,
   config_reset = lume.fn(save.config_reset, ship),

   get_fps = love.timer.getFPS,
   get_screen_size = love.window and love.window.getMode,
   powersave = love.system.getPowerInfo() == "battery",
}

ui.pause = function() pause(ui.play, quit, resize, font_path) end
ui.set_font = function(path, size)
   -- if size is nil, assume path is size
   local font
   if(size == nil) then
      font_size = math.max(6, path)
      font = love.graphics.newFont(font_path, path)
   else
      font_path, font_size = path, math.max(6, size)
      font = love.graphics.newFont(path, size)
   end
   love.graphics.setFont(font)

   -- love 0.9.x doesn't support this
   local noto = love.graphics.newFont("assets/fonts/noto-thai.ttf", 16)
   local djvu = love.graphics.newFont("assets/fonts/DejaVuSansMono.ttf", 16)
   if(font.setFallbacks) then font:setFallbacks(noto,djvu) end
end

ui.adjust_font = function(diff) ui.set_font(font_size + diff) end

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
         elseif(key == "w" and is_ctrl()) then
            save.abort(ship)
            love.event.quit()
         elseif(key == "r" and is_ctrl()) then
            save.config_reset(ship)
         end
      end
      love.update = function() end
   end
end

love.load = function()
   local options = {}
   local i = 2
   while (i <= #arg) do
     local current_argument = arg[i]
     if(current_argument == "--test") then
       options.test = true
     elseif(current_argument == "--data") then
        i=i+1 -- this arg must be handled earlier in conf.lua
     elseif(current_argument == "--backup") then
       options.backup = arg[i+1]
       i=i+1
     elseif(current_argument == "--wipe") then
       options.wipe = true
     elseif(current_argument == "-debug") then
       options.debug = true
     elseif(current_argument == "--debug") then
       options.debug = true
     elseif(current_argument == "--fuzz") then
       options.fuzz = true
     elseif(current_argument == "--cheat") then
       options.cheat = true
     elseif(current_argument == "--no-cheat") then
       options.nocheat = true
     else
       print("Unknown argument: " .. current_argument)
     end
     i = i+1
   end

   if(options.backup) then save.backup(options.backup) end
   if(options.test) then save.abort(ship) end

   love.keyboard.setKeyRepeat(true)
   love.keyboard.setTextInput(true) -- needed for mobile
   ship:configure(systems, ui)

   if(options.wipe) then
     save.abort(ship)
     love.event.quit()
   end

   save.load_into(ship)
   if(ship.api.host.autoload) then ship:dofile("host.autoload") end

   if(love.filesystem.isFile("localhacks.lua")) then
      require("localhacks")(ship)
   end

   if(options.debug) then require("mobdebug").start() end
   if(options.test) then return require("tests") end
   if(options.fuzz) then return require("tests.fuzz") end

   if(options.cheat) then
      ship.api.cheat = ship
   elseif(options.nocheat) then
      ship.api.cheat = nil
   end

   if(love.graphics) then
      resize()
      font_size = love.graphics.getWidth() / 90
      love.graphics.half_hyperbola = require("conics")
      ui.play()
   end
end

love.resize = function(w,h) love.filesystem.write("window", w .. " " .. h) end

local timestep, elapsed = 0.01, 0

local update = safely(function(dt)
      elapsed = elapsed + dt
      while(elapsed > timestep) do -- fixed timestep
         elapsed = elapsed - timestep
         local adjusted_dt = ship.time_factor * timestep
         ship:update(adjusted_dt, dt * ship.base_time_factor)
         body.update(ship.bodies, adjusted_dt)
         body.gravitate_all(ship.bodies, ship, adjusted_dt)
         ai.update(ship, ship.bodies, adjusted_dt)
         os_client.update(ship, dt)
         ship.api.editor.update(dt)
      end
end)

-- for commands that don't need repeat
local keypressed = safely(function(key)
      -- need hard-coded reset for recovering from bad config bugs
      if(key == "f1" and is_ctrl()) then
         ship.api.ui.config_reset()
      else
         ship.api.editor.keypressed(key)
      end
end)

local keyreleased = safely(ship.api.editor.keyreleased)
local wheelmoved = safely(ship.api.editor.wheelmoved)
local mousepressed = safely(ship.api.editor.mousepressed)
local mousereleased = safely(ship.api.editor.mousereleased)
local textinput = safely(ship.api.editor.textinput)

local draw = function(dt)
   if(ship.api.editor.get_prop("transparency") ~= false) then
      local w, h = love.window.getMode()
      flight_draw()
      love.graphics.setColor(ship.api.editor.colors.background)
      love.graphics.rectangle("fill", 0, 0, w, h)
   end
   ship.api.editor.draw(dt)
end

ui.play = function()
   ui.set_font(font_size)
   love.update, love.keypressed, love.keyreleased, love.wheelmoved,
   love.mousepressed, love.mousereleased, love.textinput,love.draw =
      update, keypressed, keyreleased, wheelmoved,
   mousepressed, mousereleased, textinput, safely(draw)
end

local major, minor, patch = love.getVersion()
if(major == 0 and not (minor > 10 or minor == 10 and patch >= 2)) then
   print("Need LÖVE 0.10.2+")
end
