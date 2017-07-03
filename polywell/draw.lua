local row_height, scroll_rows, em, w, h
local padding, buffer_padding, scroll_point = 10, 0, 0.8

local render_line = function(ln2, y)
   if(ln2 == "\f\n" or ln2 == "\f") then
      love.graphics.line(0, y + 0.5 * row_height, w, y + 0.5 * row_height)
   else
      love.graphics.print(ln2, buffer_padding, y)
   end
end

local render_buffer = function(b, colors, x, y, bw, bh, focused)
   love.graphics.push()
   love.graphics.translate(x, y)
   love.graphics.setScissor(x, y, bw, bh)
   local display_rows = math.floor(bh / row_height)
   local edge = math.ceil(display_rows * scroll_point)
   local offset = (b.point_line < edge and 0) or (b.point_line - edge)
   if(focused or not scroll_rows) then scroll_rows = display_rows end
   for i,line in ipairs(b.props.render_lines or b.lines) do
      if(i >= offset) then
         local row_y = row_height * (i - offset)
         if(row_y >= h - row_height) then break end

         if(i == b.mark_line) then -- mark
            love.graphics.setColor(colors.mark)
            love.graphics.rectangle("line", b.mark*em, row_y, em, row_height)
         end
         if(i == b.point_line) then -- point and point line
            love.graphics.setColor(colors.point_line)
            love.graphics.rectangle("fill", 0, row_y, w, row_height)
            love.graphics.setColor(colors.point)
            love.graphics.rectangle(focused and "fill" or "line",
                                    buffer_padding+b.point*em, row_y,
                                    em, row_height)
         end

         if(b.props.render_lines) then -- fancy colors get ANDed w base colors
            love.graphics.setColor(255, 255, 255)
         else
            love.graphics.setColor(colors.text)
         end
         render_line(line, row_y)
      end
   end
   love.graphics.pop()
   love.graphics.setScissor()
end

local draw_scroll_bar = function(b, colors)
   -- this only gives you an estimate since it uses the amount of
   -- lines entered rather than the lines drawn, but close enough

   -- height is percentage of the possible lines
   local bar_height = math.min(100, (scroll_rows * 100) / #b.lines)
   -- convert to pixels (percentage of screen height, minus 10px padding)
   local bar_height_pixels = (bar_height * (h - 10)) / 100

   local sx = w - 5
   love.graphics.setColor(colors.scroll_bar)
   -- Handle the case where there are less actual lines than display rows
   if bar_height_pixels >= h - 10 then
      love.graphics.line(sx, 5, sx, h - 5)
   else
      -- now determine location on the screen by taking the offset in
      -- history and converting it first to a percentage of total
      -- lines and then a pixel offset on the screen
      local bar_end = (b.point_line * 100) / #b.lines
      bar_end = ((h - 10) * bar_end) / 100

      local bar_begin = bar_end - bar_height_pixels
      -- Handle overflows
      if bar_begin < 5 then
         love.graphics.line(sx, 5, sx, bar_height_pixels)
      elseif bar_end > h - 5 then
         love.graphics.line(sx, h - 5 - bar_height_pixels, sx, h - 5)
      else
         love.graphics.line(sx, bar_begin, sx, bar_end)
      end
   end
end

return function(b, buffers_where, echo_message, colors)
   row_height = love.graphics.getFont():getHeight()
   em = love.graphics.getFont():getWidth('a')
   w, h = love.window.getMode()

   -- Draw background
   if(#buffers_where > 0) then
      love.graphics.setColor(colors.background)
      love.graphics.rectangle("fill", 0, 0, w, h)
   end

   for pos,buf in pairs(buffers_where) do
      local x,y,bw,bh = unpack(pos)
      render_buffer(buf, colors, x, y, bw, bh, buf == b)
   end

   love.graphics.setColor(colors.minibuffer_bg)
   love.graphics.rectangle("fill", 0, h - row_height, w, row_height)
   love.graphics.setColor(colors.minibuffer_fg)

   if(b.path == "minibuffer") then
      love.graphics.print(b:render(), padding, h - row_height)
      love.graphics.setColor(colors.point)
      love.graphics.rectangle("fill", padding+b.point*em,
                              h - row_height, em, row_height)
   elseif(echo_message) then
      love.graphics.print(echo_message, padding, h - row_height)
   else
      love.graphics.print(b:modeline(), padding, h - row_height)
   end

   if(b.path ~= "minibuffer") then draw_scroll_bar(b, colors) end
end
