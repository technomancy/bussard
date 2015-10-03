local utils = require("utils")
local lume = require("lume")
local body = require("body")

local vector_size = 50
local w, h = love.graphics:getWidth(), love.graphics:getHeight()

local get_pos = function(pos_str)
   local x, y = unpack(lume.map(lume.split(pos_str, ":"), tonumber))
   if(x < 0) then x = (w+x)-vector_size end
   if(y < 0) then y = (h+y)-vector_size end
   return x, y
end

local get_data = function(ship, field)
   if(type(field) == "function") then
      return field(ship)
   elseif(type(field) == "string") then
      local t = ship
      for _,f in ipairs(lume.split(field, ".")) do
         t = t and t[f]
      end
      return t
   else
      error("Unknown field type " .. type(field))
   end
end

local render_vector = function(at_x, at_y, values)
   if(not values or not values[1]) then return end
   local half = vector_size / 2
   local x, y = unpack(values)

   love.graphics.line(at_x + half, at_y + half,
                      at_x + half + x, at_y + half + y)
   love.graphics.setLineWidth(1)
   love.graphics.setColor(255, 255, 255);
   love.graphics.rectangle("line", at_x, at_y, vector_size, vector_size)
end

local render_bar = function(x, y, values)
   local level, max, secondary = unpack(values)
   love.graphics.rectangle("fill", x, y, math.min(level, max), 10)
   love.graphics.setColor(255, 200, 200)
   love.graphics.rectangle("line", x, y, max, 10)
   if(secondary) then
      love.graphics.setColor(0,0,0,100)
      love.graphics.rectangle("fill", x, y, secondary, 5)
   end
end

local render_text = function(x, y, format, values)
   if(values and values[1]) then
      love.graphics.print(string.format(format, unpack(values)), x, y)
   end
end

local trajectory = function(ship, bodies, steps, step_size, color1, color2)
   local last_x, last_y
   local sim_ship = {x = ship.x, y = ship.y, dx = ship.dx, dy = ship.dy,
                     mass = ship.mass}
   local sim_bodies = {}
   for _, b in pairs(bodies) do
      if(b ~= ship) then
         sim_bodies[#sim_bodies+1] = {x = b.x, y = b.y, dx = b.dx, dy = b.dy,
                                      mass = b.mass}
      end
   end

   love.graphics.setLineWidth(5)
   for i=0, steps do
      if i % 10 == 0 then
         love.graphics.setColor(color1)
         color1, color2 = color2, color1
      end
      last_x, last_y = sim_ship.x, sim_ship.y
      body.gravitate_all(sim_bodies, sim_ship, step_size)
      love.graphics.line(last_x - ship.x, last_y - ship.y,
                         sim_ship.x - ship.x, sim_ship.y - ship.y)
   end
end

return {
   render = function(ship)
      for pos,data in pairs(ship.api.hud or {}) do
         love.graphics.setColor(unpack(data.color or {255, 255, 255, 150}))
         love.graphics.setLineWidth(data.width or 1)
         local values = lume.map(data.values, lume.fn(get_data, ship.api))
         local x,y = get_pos(pos)
         if(data.type == "text") then render_text(x, y, data.format, values)
         elseif(data.type == "bar") then render_bar(x, y, values)
         elseif(data.type == "vector") then render_vector(x, y, values)
            -- TODO: side slider?
         else error("Unknown hud type " .. data.type) end
      end
   end,

   trajectory = trajectory,
}
