local w,h = love.graphics:getWidth(), love.graphics:getHeight()

player = { x = 0, y = 0,
           dx = 0, dy = -2,
           heading = math.pi,
           engine = 3,
           turning = 3,
           target = 0 }

local starfield = require "starfield"
local star1 = starfield.new(10, w, h, 0.01, 100)
local star2 = starfield.new(10, w, h, 0.05, 175)
local star3 = starfield.new(10, w, h, 0.1, 255)

local body = require "body"
local bodies = body.load()

local hud = require "hud"

local scale = 50
local paused = false

local font = love.graphics.newFont("jura-demibold.ttf", 20)
love.graphics.setFont(font)

repl = require "love-repl"

repl.font = font
love.load = repl.initialize

love.update = function(dt)
   if(love.keyboard.isDown("-")) then
      scale = scale + dt
   elseif(love.keyboard.isDown("=") and scale > dt * 0.5) then
      scale = scale - dt
   end

   if(paused or repl.toggled()) then return end

   if(love.keyboard.isDown("up")) then
      player.dx = player.dx + (math.sin(player.heading) * dt * player.engine)
      player.dy = player.dy + (math.cos(player.heading) * dt * player.engine)
   end

   if(love.keyboard.isDown("left")) then
      player.heading = player.heading + (dt * player.turning)
   elseif(love.keyboard.isDown("right")) then
      player.heading = player.heading - (dt * player.turning)
   end

   player.x = player.x + (player.dx * dt * 100)
   player.y = player.y + (player.dy * dt * 100)

   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 50)
      b.y = b.y + (b.dy * dt * 50)
      local ddx, ddy = body.gravitate(b, player.x, player.y)
      player.dx = player.dx + ddx
      player.dy = player.dy + ddy
      for _, b2 in ipairs(bodies) do
         local ddx, ddy = body.gravitate(b, b2.x, b2.y)
         b2.dx = b2.dx + ddx
         b2.dy = b2.dy + ddy
      end
   end
end

love.keypressed = function(key)
   if(repl.toggled() and key:len() == 1) then repl.textinput(key)
   elseif(repl.toggled() and key:len() > 1) then repl.keypressed(key)
   elseif(key == "escape") then love.event.push('quit')
   elseif(key == "p") then paused = not paused
   elseif(key == "tab") then
      player.target = player.target + 1
      if(player.target > #bodies) then player.target = 0 end
   elseif(key == "f2") then repl.toggle()
   end
end

love.draw = function()
   if(repl.toggled()) then
      repl.draw()
      return
   end

   starfield.render(star1, player.x, player.y)
   starfield.render(star2, player.x, player.y)
   starfield.render(star3, player.x, player.y)

   -- momentum indicator
   love.graphics.circle("fill", player.dx * 20 + w/2, h/2 + player.dy * 20, 1)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.scale(1/scale, 1/scale)

   for i, b in ipairs(bodies) do
      body.draw(b, player.x, player.y, i == player.target)
   end

   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - player.heading)
   love.graphics.triangle("fill", 0, -30, -20, 50, 20, 50)

   love.graphics.pop()
   love.graphics.setColor(255, 255, 255);
   hud.render(player, bodies[player.target])
end
