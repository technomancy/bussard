local lume = require "lume"
local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local starfield = require "starfield"
local star1 = starfield.new(10, w, h, 0.01, 100)
local star2 = starfield.new(10, w, h, 0.05, 175)
local star3 = starfield.new(10, w, h, 0.1, 255)

local title = love.graphics.newImage("assets/title.png") -- jura demibold
local main_font = love.graphics.newFont("assets/mensch.ttf", 14)
local font_height = main_font:getHeight()

local text, line = {}, 1
local scroll = 0

local files = {"main.lua","main.lua","main.lua","main.lua","main.lua",
               "main.lua","main.lua","main.lua","main.lua","main.lua",
               "main.lua","main.lua","main.lua","main.lua","main.lua",
               "asteroid.lua","body.lua","mission.lua", "save.lua",
               "services.lua","splash.lua",
}

local buttons = {"play", "credits", "license", "quit"}
local actions = {play=function() end,
                 credits=function()
                    text, line = lume.split(love.filesystem.read("credits.md"), "\n"), 1
                 end,
                 license=function()
                    text, line = lume.split(love.filesystem.read("LICENSE"), "\n"), 1
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
   if(key == "up" and selected > 1) then
      selected = selected - 1
   elseif(key == "down" and selected < #buttons) then
      selected = selected + 1
   elseif(key == "return") then
      if(love.filesystem.isFile(input)) then
         text, line = lume.split(love.filesystem.read(input), "\n"), 1
      else
         actions[buttons[selected]]()
      end
      input = ""
   elseif(key == "escape") then
      input = ""
   elseif(#key == 1) then
      input = input .. (love.keyboard.isDown("lshift") and key:upper() or key)
   end
end

local draw = function()
   starfield.render(star1, x, y)
   starfield.render(star2, x, y)
   starfield.render(star3, x, y)

   love.graphics.draw(title, 30, 30)

   love.graphics.setFont(main_font)
   for i,name in ipairs(buttons) do
      love.graphics.setColor(125,125,125)
      if i == selected then love.graphics.setColor(200,200,200) end
      love.graphics.print(name, 100, 100 + i*30)
   end

   love.graphics.setColor(0,200,0)
   for i=1, math.floor((h-100) / font_height) do
      if(text[line+i-1]) then
         love.graphics.print(text[line+i-1], 260, 100+i*font_height)
      end
   end
end

local random_choice = function(t) return t[love.math.random(#t)] end

return function(play, quit, resume)
   if(resume) then buttons[1], actions.resume = "resume", play end
   actions.play, actions.quit = play, quit
   love.update,love.keypressed,love.draw,love.textinput=update,keypressed,draw
   text, line = lume.split(love.filesystem.read(random_choice(files)), "\n"), 1
end

