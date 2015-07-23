local breen = {w = love.graphics:getWidth(),
               h = love.graphics:getHeight()}

local starfield = require "starfield"
local body = require "body"

local bodies = body.load()

local player = {x = 0, y = 0,
                dx = 0, dy = 0,
                heading = math.pi, }

local scale = 0.2

love.load = function()
   breen.star1 = starfield.new(10, breen.w, breen.h, 1, 100)
   breen.star2 = starfield.new(10, breen.w, breen.h, 5, 175)
   breen.star3 = starfield.new(10, breen.w, breen.h, 10, 255)
end

love.update = function(dt)
   if(love.keyboard.isDown("escape")) then love.event.push('quit') end

   player.x = player.x + player.dx
   player.y = player.y + player.dy

   if(love.keyboard.isDown("up")) then
      player.dx = player.dx + (math.sin(player.heading) * dt)
      player.dy = player.dy + (math.cos(player.heading) * dt)
   end

   if(love.keyboard.isDown("=")) then
      scale = scale + (dt * 0.5)
   elseif(love.keyboard.isDown("-")) then
      scale = scale - (dt * 0.5)
   end

   if(love.keyboard.isDown("left")) then
      player.heading = player.heading + dt
   elseif(love.keyboard.isDown("right")) then
      player.heading = player.heading - dt
   end
end

love.draw = function()
   starfield.render(breen.star1, player.x, player.y)
   starfield.render(breen.star2, player.x, player.y)
   starfield.render(breen.star3, player.x, player.y)

   love.graphics.translate(breen.w / 2, breen.h / 2)
   love.graphics.scale(scale, scale)

   for _, b in ipairs(bodies) do
      body.draw(b, player.x, player.y)
   end

   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - player.heading)
   love.graphics.triangle("fill", 0, -30, -20, 50, 20, 50)
end
