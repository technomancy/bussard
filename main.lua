local breen = {w = love.graphics:getWidth(),
               h = love.graphics:getHeight()}

local starfield = require "starfield"

local player = {x = breen.w / 2,
                y = breen.h / 2,
                dx = 0,
                dy = 0,
                heading = math.pi, }

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

   if(love.keyboard.isDown("down")) then
      local reverse = math.atan(player.dx / player.dy)
      if(reverse - player.heading < 0) then
         player.heading = player.heading - dt
      else
         player.heading = player.heading + dt
      end
   elseif(love.keyboard.isDown("left")) then
      player.heading = player.heading + dt
   elseif(love.keyboard.isDown("right")) then
      player.heading = player.heading - dt
   end
   if(player.heading > math.pi) then -- TODO: got to be a better way for this
      player.heading = player.heading - (2 * math.pi)
   elseif(player.heading < -math.pi) then
      player.heading = player.heading + (2 * math.pi)
   end
end

love.draw = function()
   starfield.render(breen.star1, player.x, player.y)
   starfield.render(breen.star2, player.x, player.y)
   starfield.render(breen.star3, player.x, player.y)

   love.graphics.setColor(255, 50, 50);
   love.graphics.translate(breen.w / 2, breen.h / 2)
   love.graphics.rotate(math.pi - player.heading)
   love.graphics.triangle("fill", 0, -10, -3, 10, 3, 10)
end
