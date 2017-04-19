-- Versions of Lua prior to 5.1 could not properly support iterating over proxy
-- tables with their built-in iterators. This module fixes that problem.

local original_pairs, original_ipairs = pairs, ipairs
local mtpairs = function(tab)
   local mt = getmetatable(tab)
   if(mt and mt.__pairs) then
      return mt.__pairs(tab)
   else
      return original_pairs(tab)
   end
end

local mtipairs = function(tab)
   local mt = getmetatable(tab)
   if(mt and mt.__ipairs) then
      return mt.__ipairs(tab)
   else
      return original_ipairs(tab)
   end
end

local patched, ipatched, t = false, false, {}
setmetatable(t, {__pairs = function() return next,{1},nil end})

for _ in pairs(t) do patched = true end
if(not patched) then pairs = mtpairs end
for _ in ipairs(t) do ipatched = true end
if(not ipatched) then ipairs = mtipairs end

return {original_pairs, original_ipairs}
