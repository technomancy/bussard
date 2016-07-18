local utf8 = require("utf8.init")
local lume = require("lume")
local body = require("body")

local vector_size, font_width = 50, nil
local w, h

local get_pos = function(value)
   local x, y = value.x, value.y
   local offset = (value.type == "vector" and vector_size) or 0
   if(x < 0) then x = (w+x)-offset end
   if(y < 0) then y = (h+y)-offset end
   return x, y
end

local get_data = function(ship, field)
   if(type(field) == "function") then
      return field(ship)
   elseif(type(field) == "string") then
      return ship:find(field)
   elseif(type(field) == "number") then
      return field
   else
      error("Unknown field type " .. type(field))
   end
end

local render_vector = function(at_x, at_y, values)
   if(not values or not values[1]) then return end
   local half = vector_size / 2
   local x, y, scale = unpack(values)

   if(type(x) == "number" and type(y) == "number") then
      if(scale) then x, y = x * scale, y * scale end
      love.graphics.line(at_x + half, at_y + half,
                         at_x + half + x, at_y + half + y)
      love.graphics.setLineWidth(1)
      love.graphics.setColor(255, 255, 255);
      love.graphics.rectangle("line", at_x, at_y, vector_size, vector_size)
   end
end

local render_bar = function(x, y, values, data)
   local level, max, secondary = unpack(values)
   local height = data.h or 10
   if(type(level) == "number" and type(max) == "number") then
      love.graphics.rectangle("fill", x, y, math.min(level, max), height)
      love.graphics.setColor(255, 200, 200)
      love.graphics.rectangle("line", x, y, max, height)
      if(type(secondary) == "number") then
         love.graphics.setColor(0,0,0,100)
         love.graphics.rectangle("fill", x, y, secondary, 5)
      end
   end
end

local render_text = function(x, y, format, values, data)
   if(values and values[1]) then
      local text = utf8.format(format, unpack(values))
      local limit = data.limit or utf8.len(text) * font_width
      if(data.align == "right") then x = x - limit end
      love.graphics.printf(text, x, y, limit, data.align)
   end
end

local trajectory = function(ship, bodies, steps, step_size,
                     color1, color2, color3, color4)
   local last_x, last_y
   local sim_ship = {x = ship.x, y = ship.y, dx = ship.dx, dy = ship.dy,
                     mass = ship.mass}
   local sim_bodies, sim_target, target_points = {}, nil, {}
   for _, b in pairs(bodies) do
      if(b ~= ship) then
         local sim = lume.pick(b, "x", "y", "dx", "dy", "mass")
         table.insert(sim_bodies, sim)
         if(b == ship.target and not b.fixed) then sim_target = sim end
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
      if(sim_target) then
         table.insert(target_points, {x=sim_target.x - ship.x,
                                      y=sim_target.y - ship.y})
      end
   end

   if(sim_target) then
      color1, color2 = color3, color4
      for i=1, (steps-1) do
         if i % 10 == 0 then
            love.graphics.setColor(color1)
            color1, color2 = color2, color1
         end
         love.graphics.line(target_points[i+1].x, target_points[i+1].y,
                            target_points[i].x, target_points[i].y)
      end
   end
end

return {
   render = function(ship)
      w, h = love.graphics:getWidth(), love.graphics:getHeight()
      font_width = love.graphics.getFont():getWidth('a')
      for _,data in ipairs(ship.api.hud or {}) do
         love.graphics.setColor(unpack(data.color or {255, 255, 255, 150}))
         love.graphics.setLineWidth(data.width or 1)
         local values = lume.map(data.values, lume.fn(get_data, ship.api))
         local x,y = get_pos(data)
         if(data.type == "text") then render_text(x, y, data.format, values, data)
         elseif(data.type == "bar") then render_bar(x, y, values, data)
         elseif(data.type == "vector") then render_vector(x, y, values, data)
         else error("Unknown hud type " .. data.type) end
      end
      -- hard-code this for now
      local scale_y = math.log(ship.api.scale) * h
      love.graphics.line(w - 5, scale_y, w, scale_y)
   end,

   trajectory = trajectory,
}
