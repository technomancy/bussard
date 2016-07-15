local lume = require "lume"

local starfield = require "starfield"
local stars = {}

local title, choices_font, text_font, font_height, resize

local text, line = {}, 1
local scroll = 0

local files = {"main.lua","main.lua","main.lua","main.lua","main.lua",
               "main.lua","main.lua","main.lua","main.lua","main.lua",
               "main.lua","main.lua","main.lua","main.lua","main.lua",
               "asteroid.lua","body.lua","mission.lua", "save.lua",
               "services.lua","pause.lua",
}

local buttons = {"resume", "credits", "license", "toggle fullscreen", "quit"}
local actions = {resume=function() end,
                 credits=function()
                    text, line = lume.split(love.filesystem.read("credits.md"), "\n"), 1
                 end,
                 license=function()
                    text, line = lume.split(love.filesystem.read("LICENSE"), "\n"), 1
                 end,
                 ["toggle fullscreen"]=function()
                    local _,_,f = love.window.getMode()
                    if(f.fullscreen) then
                       love.filesystem.write("fullscreen", "false")
                    else
                       love.filesystem.write("fullscreen", "true")
                    end
                    resize()
                 end,
                 quit = love.event.quit,}
local selected = 1

local x,dx,y,dy = 0,0,0,0

local update = function(dt)
   dx, dy = dx + love.math.random(32) - 16, dy + love.math.random(32) - 16
   x, y = x + dx, y + dy
   if(dx > 128 or dx < -128) then dx = dx * 0.9 end
   if(dy > 128 or dy < -128) then dy = dy * 0.9 end
   scroll = scroll + dt
   if(love.keyboard.isDown(" ")) then line = line + 1
   elseif(scroll > 1 and line <= #text) then line, scroll = line + 1,0
   elseif(scroll > 1 and line == #text) then line, scroll = 1,0 end
end

local input = ""

local keypressed = function(key)
   if(key == "up") then
      selected = selected - 1
      if(selected == 0) then selected = #buttons end
   elseif(key == "down") then
      selected = (selected % #buttons) + 1
   elseif(key == "return") then
      if(love.filesystem.isFile(input)) then
         text, line = lume.split(love.filesystem.read(input), "\n"), 1
      else
         actions[buttons[selected]]()
      end
      input = ""
   elseif(key == "escape") then
      input = ""
   elseif(key == "q" and love.keyboard.isDown("lctrl", "rctrl")) then
      love.event.quit()
   elseif(#key == 1) then
      input = input .. (love.keyboard.isDown("lshift") and key:upper() or key)
   end
end

local draw = function()
   local w,h = love.window.getMode()
   for _,s in pairs(stars) do starfield.render(s, x, y, w, h) end

   love.graphics.draw(title, 30, 30)

   love.graphics.setFont(choices_font)
   for i,name in ipairs(buttons) do
      love.graphics.setColor(125,125,125)
      if i == selected then love.graphics.setColor(200,200,200) end
      love.graphics.print(name, 80, 100 + i*40)
   end

   love.graphics.setColor(0,200,0)
   love.graphics.setFont(text_font)
   for i=1, math.floor((love.graphics.getHeight()-100) / font_height) do
      if(text[line+i-1]) then
         love.graphics.print(text[line+i-1], 300, 100+i*font_height)
      end
   end
end

local random_choice = function(t) return t[love.math.random(#t)] end

return function(resume, quit, resize_fn, font_path)
   title = love.graphics.newImage("assets/title.png") -- jura demibold
   resize = resize_fn
   choices_font = love.graphics.newFont(font_path, 20)
   text_font = love.graphics.newFont(font_path, 14)
   font_height = text_font:getHeight()
   stars = { starfield.new(10, 0.005, 75),
             starfield.new(10, 0.01, 100),
             starfield.new(10, 0.05, 175),
             starfield.new(10, 0.1, 255), }
   actions.resume, actions.quit = resume, quit
   love.update,love.keypressed,love.draw,love.textinput=update,keypressed,draw,nil
   text, line = lume.split(love.filesystem.read(random_choice(files)), "\n"), 1
end
