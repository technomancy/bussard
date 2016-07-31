local keywords = {"and", "break", "do", "else", "elseif", "end", "false",
                  "for", "function", "if", "in", "local", "nil", "not", "or",
                  "repeat", "return", "then", "true", "until", "while", }

local colors, comment_match

local function colorize_keyword(l, n)
   if(n and n > #keywords) then return {colors.text, l} end
   local s,e = l:find(keywords[n or 1], nil, true)
   if(s and l:sub(s-1,s-1):find("%w") or (e and l:sub(e+1,e+1):find("%w"))) then
      -- if it's inside a larger word, no match!
      return colorize_keyword(l, (n or 1) + 1)
   elseif(s == 1) then
      return {colors.keyword, l:sub(1,e), unpack(colorize_keyword(l:sub(e+1)))}
   elseif(s) then
      return {colors.text, l:sub(1, s-1), colors.keyword, l:sub(s,e),
              unpack(colorize_keyword(l:sub(e+1))) }
   else
      return colorize_keyword(l, (n or 1) + 1)
   end
end

local function colorize_number(l)
   -- TODO: 3   3.0   3.1416   314.16e-2   0.31416E1   0xff   0x56
   local s,e = l:find("[\\.0-9]+")
   if(s == 1) then
      return {colors.number, l:sub(1,e), unpack(colorize_number(l:sub(e+1)))}
   elseif(s) then
      local line = colorize_keyword(l:sub(1, s-1))
      return lume.concat(line, {colors.number, l:sub(s,e),
                                unpack(colorize_number(l:sub(e+1))) })
   else
      return colorize_keyword(l)
   end
end

local colorize_comment = function(l)
   comment_match = l:find("-[-]")
   if(comment_match == 1) then
      return {colors.comment, l}
   elseif(comment_match) then
      local line = colorize_number(l:sub(1,comment_match-1))
      table.insert(line, colors.comment)
      table.insert(line, l:sub(comment_match))
      return line
   else
      return colorize_number(l)
   end
end

local function colorize_string(l)
   local s,e = l:find("\"[^\"]*\"")
   if(s == 1) then
      return {colors.str, l:sub(1,e), unpack(colorize_comment(l:sub(e+1)))}
   elseif(s) then
      local pre = colorize_comment(l:sub(1,s-1))
      if(comment_match) then
         return pre
      else
         local post = colorize_string(l:sub(e+1))
         return lume.concat(pre, {colors.str, l:sub(s, e)}, post)
      end
   else
      return colorize_comment(l)
   end
end

local function colorize(l)
   return colorize_string(l)
end

return function(editor)
   colors = editor.colors
   -- 0.9.x doesn't have multi-colored print
   if(love._version_major > 0 or love._version_minor < 10) then return end
   editor.set_prop("render_lines", lume.map(editor.get_lines(), colorize))
end
