local utf8 = require("utf8.init")
local lume = require("lume")

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
      local scale_y = math.log(ship.api.scale / 2) * h
      love.graphics.line(w - 5, scale_y, w, scale_y)
   end,
}
