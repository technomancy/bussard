local lume = require("polywell.lume")

local function colorize_keyword(keywords, colors, l, n, offset)
   -- hoo boy, not having access to | in lua patterns is a pain!
   -- if this code makes you cringe at the performance implications, just
   -- remember that luajit is faster than you could possibly hope for.
   -- (the longest file in the codebase, 1200 lines, colorizes in 0.3 seconds
   -- on a 2009-era core 2 duo thinkpad)
   if(n and n > #keywords) then return {colors.text, l} end
   local s,e = string.find(l, keywords[n or 1], offset, true)
   if(s and string.find(string.sub(l,s-1,s-1), "[%w_]") or
      (e and string.find(string.sub(l,e+1,e+1), "[%w_]"))) then
      -- if it's inside a larger word, no match!
      return colorize_keyword(keywords, colors, l, n, e+1)
   elseif(s == 1) then
      return {colors.keyword, string.sub(l,1,e),
              unpack(colorize_keyword(keywords, colors, string.sub(l, e+1)))}
   elseif(s) then
      local pre = colorize_keyword(keywords, colors, string.sub(l,1, s-1))
      return lume.concat(pre, {colors.keyword, string.sub(l,s,e),
                               unpack(colorize_keyword(keywords, colors,
                                                       string.sub(l,e+1))) })
   else
      return colorize_keyword(keywords, colors, l, (n or 1) + 1)
   end
end

local function colorize_number(keywords, colors, l, offset)
   local s,e = string.find(l, "[\\.0-9]+", offset)
   if(s and string.find(string.sub(l,s-1,s-1), "[%w_]")) then
       -- no numbers at the end of identifiers
      return colorize_number(keywords, colors, l, e+1)
   elseif(s == 1) then
      return {colors.number, string.sub(l,1,e),
              unpack(colorize_number(keywords, colors, string.sub(l, e+1)))}
   elseif(s) then
      local line = colorize_keyword(keywords, colors, string.sub(l, 1, s-1))
      return lume.concat(line, {colors.number, string.sub(l,s,e),
                                unpack(colorize_number(keywords, colors,
                                                       string.sub(l,e+1))) })
   else
      return colorize_keyword(keywords, colors, l)
   end
end

local comment_match

local colorize_comment = function(keywords, colors, l)
   comment_match = string.find(l, keywords.comment_pattern)
   if(comment_match == 1) then
      return {colors.comment, l}
   elseif(comment_match) then
      local n = string.sub(l, 1,comment_match-1)
      local line = colorize_number(keywords, colors, n)
      table.insert(line, colors.comment)
      table.insert(line, string.sub(l,comment_match))
      return line
   else
      return colorize_number(keywords, colors, l)
   end
end

local function colorize_string(keywords, colors, l)
   local s,e = string.find(l, "\"[^\"]*\"")
   if(s == 1) then
      return {colors.str, string.sub(l,1,e),
              unpack(colorize_comment(keywords, colors, string.sub(l,e+1)))}
   elseif(s) then
      local pre = colorize_comment(keywords, colors, string.sub(l, 1,s-1))
      if(comment_match) then
         table.insert(pre, colors.comment)
         table.insert(pre, string.sub(l,s))
         return pre
      else
         local post = colorize_string(keywords, colors, string.sub(l,e+1))
         return lume.concat(pre, {colors.str, string.sub(l, s, e)}, post)
      end
   else
      return colorize_comment(keywords, colors, l)
   end
end

return function(keywords, colors, lines)
   return lume.map(lines, lume.fn(colorize_string, keywords, colors))
end
