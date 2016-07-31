-- Take a table of lines and turn it into a table of {color, string,
-- color2, string2} which can be used by love.graphics.print.

-- this is https://github.com/Stepets/utf8.lua, *not* the utf8.lua from love
-- which lacks a bunch of basic string functions (find, sub, etc)
local utf8 = require("utf8.init")

local keywords = {"and", "break", "do", "else", "elseif", "end", "false",
                  "for", "function", "if", "in", "local", "nil", "not", "or",
                  "repeat", "return", "then", "true", "until", "while", }

local colors, comment_match

local function colorize_keyword(l, n)
   -- hoo boy, not having access to | in lua patterns is a pain!
   -- if this code makes you cringe at the performance implications, just
   -- remember that luajit is faster than you could possibly hope for.
   if(n and n > #keywords) then return {colors.text, l} end
   local s,e = utf8.find(l, keywords[n or 1], nil, true)
   if(s and utf8.find(utf8.sub(l,s-1,s-1), "[%w_]") or
      (e and utf8.find(utf8.sub(l,e+1,e+1), "[%w_]"))) then
      -- if it's inside a larger word, no match!
      return colorize_keyword(l, (n or 1) + 1)
   elseif(s == 1) then
      return {colors.keyword, utf8.sub(l,1,e),
              unpack(colorize_keyword(utf8.sub(l, e+1)))}
   elseif(s) then
      return {colors.text, utf8.sub(l,1, s-1), colors.keyword,
              utf8.sub(l,s,e), unpack(colorize_keyword(utf8.sub(l,e+1))) }
   else
      return colorize_keyword(l, (n or 1) + 1)
   end
end

local function colorize_number(l)
   -- TODO: scientific notation, hex
   local s,e = utf8.find(l, "[\\.0-9]+")
   if(s and utf8.find(utf8.sub(l,s-1,s-1), "[%w_]")) then
      return colorize_keyword(l) -- numbers at the end of identifiers: nope
   elseif(s == 1) then
      return {colors.number, utf8.sub(l,1,e),
              unpack(colorize_number(utf8.sub(l, e+1)))}
   elseif(s) then
      local line = colorize_keyword(utf8.sub(l, 1, s-1))
      return lume.concat(line, {colors.number, utf8.sub(l,s,e),
                                unpack(colorize_number(utf8.sub(l,e+1))) })
   else
      return colorize_keyword(l)
   end
end

local colorize_comment = function(l)
   comment_match = utf8.find(l, "[-][-]")
   if(comment_match == 1) then
      return {colors.comment, l}
   elseif(comment_match) then
      local line = colorize_number(utf8.sub(l, 1,comment_match-1))
      table.insert(line, colors.comment)
      table.insert(line, utf8.sub(l,comment_match))
      return line
   else
      return colorize_number(l)
   end
end

local function colorize_string(l)
   local s,e = utf8.find(l, "\"[^\"]*\"")
   if(s == 1) then
      return {colors.str, utf8.sub(l,1,e),
              unpack(colorize_comment(utf8.sub(l,e+1)))}
   elseif(s) then
      local pre = colorize_comment(utf8.sub(l, 1,s-1))
      if(comment_match) then
         table.insert(pre, colors.comment)
         table.insert(pre, utf8.sub(l,s))
         return pre
      else
         local post = colorize_string(utf8.sub(l,e+1))
         return lume.concat(pre, {colors.str, utf8.sub(l, s, e)}, post)
      end
   else
      return colorize_comment(l)
   end
end

return function(lines, color_table)
   colors = color_table
   local t = {}
   for _,l in ipairs(lines) do table.insert(t, colorize_string(l)) end
   return t
end
