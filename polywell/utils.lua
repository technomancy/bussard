local utf8 = require("polywell.utf8")
local lume = require("polywell.lume")

local find_common = function(a, b)
   for i=math.min(#a, #b),1,-1 do
      if(utf8.sub(a,1,i) == utf8.sub(b,1,i)) then
         return utf8.sub(a,1,i)
      end
   end
end

local longest_common_prefix = function(strings)
   local common = strings[1]
   for _,s in pairs(strings) do
      common = find_common(common, s)
      if(not common) then return "" end
   end
   return common
end

-- in Lua 5.2+, the regular pairs function just works with metamethods, but
-- Love is compiled without 5.2-compat, so we have to check manually. ugh.
local mtpairs = function(t)
   local mt = getmetatable(t)
   return (mt and mt.__pairs) or pairs
end

local function completions_for(input, context, separator, prefixes)
   if(type(context) ~= "table") then return {} end
   prefixes = prefixes or {}
   local input_parts = separator and lume.split(input, separator) or {input}
   if(#input_parts == 1) then
      local matches = {}
      -- if we have an array, use it as-is.
      -- if we have a k/v table or proxied table, use keys.
      if(#context == 0 or #context ~= lume.count(context)) then
         local iter, dir = mtpairs(context)(context)
         context = {}
         for key in iter, dir do table.insert(context, key) end
      end
      for _,v in mtpairs(context)(context) do
         if(type(v) == "string" and utf8.sub(v, 1, #input) == input) then
            local parts = lume.clone(prefixes)
            table.insert(parts, v)
            table.insert(matches, table.concat(parts, separator))
         end
      end
      return matches
   else
      local first_part = table.remove(input_parts, 1)
      table.insert(prefixes, first_part)
      return completions_for(table.concat(input_parts, separator),
                             context[first_part], separator, prefixes)
   end
end

return {
   completions_for = completions_for,
   longest_common_prefix = longest_common_prefix,

   with_traceback = function(print2, f, ...)
      local args = {...}
      -- TODO: sandboxed traceback which trims out irrelevant layers
      return xpcall(function() return f(unpack(args)) end, function(e)
            print(debug.traceback(nil, 1))
            print(e)
            if(print2) then
               print2(debug.traceback())
               print2(e)
            end
      end)
   end,
}
