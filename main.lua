local body = require "body"
local starfield = require "starfield"
local hud = require "hud"
repl = require "love-repl"

local w,h = love.graphics:getWidth(), love.graphics:getHeight()

bodies = bodies or {}

player = player or { x = -200, y = 0,
           dx = 0, dy = 0,
           heading = math.pi,
           engine = 3,
           turning = 3,
           target = 0,
           fuel = 100,
           mass = 1,
           landed = false,
           gravitate = true,
}

local star1 = star1 or starfield.new(10, w, h, 0.01, 100)
local star2 = star2 or starfield.new(10, w, h, 0.05, 175)
local star3 = star3 or starfield.new(10, w, h, 0.1, 255)

local scale = scale or 0.5
local paused = paused or false

calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end

local landing_speed_max = 10

local can_land = function(player)
   local target = bodies[player.target]
   local dist_max = target and target.image:getWidth() / 2
   return(target and (not player.landed) and target.description and
             (calculate_distance(player.dx - target.dx, player.dy - target.dy))
             < landing_speed_max and
             (calculate_distance(player.x - target.x, player.y - target.y)) <
             dist_max)
end

love.load = function()
  if arg[#arg] == "-debug" then require("mobdebug").start() end
   local font = love.graphics.newFont("jura-demibold.ttf", 20)
   love.graphics.setFont(font)
   repl.font = font
   repl.initialize()
   bodies = body.load()
end

love.update = function(dt)
   if(love.keyboard.isDown("=")) then
      scale = scale + (dt / 2)
   elseif(love.keyboard.isDown("-") and scale > dt * 0.5) then
      scale = scale - (dt / 2)
   end

   if(paused or repl.toggled() or player.landed) then return end

   if(love.keyboard.isDown("up") and player.fuel > 0) then
      player.dx = player.dx + (math.sin(player.heading) * dt * player.engine)
      player.dy = player.dy + (math.cos(player.heading) * dt * player.engine)
      player.fuel = player.fuel - 0.5
   elseif(player.fuel < 100) then
      player.fuel = player.fuel + 0.05
   end

   if(love.keyboard.isDown("left")) then
      player.heading = player.heading + (dt * player.turning)
   elseif(love.keyboard.isDown("right")) then
      player.heading = player.heading - (dt * player.turning)
   end

   -- calculate movement
   player.x = player.x + (player.dx * dt * 100)
   player.y = player.y + (player.dy * dt * 100)

   for _, b in ipairs(bodies) do
      b.x = b.x + (b.dx * dt * 50)
      b.y = b.y + (b.dy * dt * 50)
      if(player.gravitate) then
         local ddx, ddy = body.gravitate(b, player.x, player.y, player.mass)
         player.dx = player.dx + ddx
         player.dy = player.dy + ddy
      end
      for _, b2 in ipairs(bodies) do
         local ddx, ddy = body.gravitate(b, b2.x, b2.y, b2.mass)
         b2.theta_v = theta
         b2.dx = b2.dx + ddx
         b2.dy = b2.dy + ddy
      end
   end
end

love.textinput = function(text)
   if(repl.toggled()) then repl.textinput(text) end
end

-- for commands that don't need repeat
love.keypressed = function(key, is_repeat)
   if(repl.toggled() and key == "escape") then repl.toggle()
   elseif(repl.toggled() and key:len() > 1) then repl.keypressed(key)
   elseif(key == "return" and can_land(player)) then
      player.landed = bodies[player.target]
   elseif(player.landed and key == "escape") then player.landed = false
   elseif(key == "escape") then love.event.push("quit")
   elseif(key == "p") then paused = not paused
   elseif(key == "tab") then
      player.target = player.target + 1
      if(player.target > #bodies) then player.target = 0 end
   elseif(key == "`" and love.keyboard.isDown("lshift")) then repl.toggle()
   end
end

love.draw = function()
   if(repl.toggled()) then repl.draw() return end

   starfield.render(star1, player.x, player.y)
   starfield.render(star2, player.x, player.y)
   starfield.render(star3, player.x, player.y)

   love.graphics.push()
   love.graphics.translate(w / 2, h / 2)
   love.graphics.scale(scale*scale)

   if(bodies[player.target]) then -- directional target indicator
      love.graphics.setLineWidth(scale*scale*5) -- TODO: scale linearly
      local px, py = bodies[player.target].x, bodies[player.target].y
      local dx, dy = px - player.x, py - player.y
      love.graphics.setColor(10, 100, 10)
      love.graphics.line(0, 0, dx, dy)
      love.graphics.setLineWidth(1)
   end

   love.graphics.setColor(255, 255, 255)
   for i, b in ipairs(bodies) do
      body.draw(b, player.x, player.y, i == player.target)
   end

   love.graphics.setColor(255, 50, 50);
   love.graphics.rotate(math.pi - player.heading)
   love.graphics.polygon("fill", 0, -30, -20, 50, 20, 50)

   love.graphics.pop()
   love.graphics.setLineWidth(1)

   love.graphics.setColor(255, 255, 255);
   hud.render(player, bodies[player.target])
   hud.vector(player.dx, player.dy, w - 10 - hud.vector_size, 10)
   -- TODO: show velocity of target body

   if(player.landed) then
      love.graphics.setColor(0,0,0, 200);
      love.graphics.rectangle("fill", 100, 100, 400, 300)
      love.graphics.setColor(255, 255, 255);
      love.graphics.rectangle("line", 100, 100, 400, 300)
      love.graphics.print("You landed on " .. player.landed.name .. "\n\n" ..
                             player.landed.description, 150, 150)
   end
end
