local ship = require("ship")
local body = require("body")
local hud = require("ship.hud")
local utils = require("utils")
local starfield = require("starfield")

local stars = { starfield.new(10, 0.01, 100),
                starfield.new(10, 0.05, 175),
                starfield.new(10, 0.1, 255), }

local portal_offsets = {
   {0, -200}, {-141, -141}, {-200, 0}, {-141, 141},
   {0, 200}, {141, 141}, {200, 0}, {141, -141},
}

return function(dt)
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

   -- the navigation ui helpers in the system coordinates
   utils.run_handlers(ship.api, "navigation_ui_helpers",
                      "broken_navigation_ui_helpers", {dt}, ship.api.editor.print)

   love.graphics.pop()

   if(ship.target and ship.target.beam_count and
      ship.target.beam_count > 8) then -- portal flash
      local flash = (ship.target.beam_count - 8) * 255
      love.graphics.setColor(255,255,255, flash)
      love.graphics.rectangle("fill", -w, -h, w*2, h*2)
   end

   -- the navigation ui helpers in the ship-drawing coordinates
   utils.run_handlers(ship.api, "ui_helpers", "broken_ui_helpers",
                      {dt}, ship.api.editor.print)

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

   local y = (love.graphics:getHeight() -
                 love.graphics.getFont():getHeight() * 2)
   local get_line = function() return ship.api.editor.get_line(-1) or "" end
   local line = ship.api.editor.with_current_buffer("*console*", get_line)
   love.graphics.setColor(ship.api.editor.colors.flight_text)
   love.graphics.print(line, 20, y)

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
end
