require "metatable_monkey"

local keys = function(t)
   local ks = {}
   for k,_ in pairs(t) do table.insert(ks, k) end
   return ks
end

local vals = function(t)
   local vs = {}
   for _,v in pairs(t) do table.insert(vs, v) end
   return vs
end

local shallow_copy = function(orig)
   local orig_type = type(orig)
   local copy
   if orig_type == 'table' then
      copy = {}
      for orig_key, orig_value in pairs(orig) do
         copy[orig_key] = orig_value
      end
   else -- number, string, boolean, etc
      copy = orig
   end
   return copy
end

local includes = function(tab, val)
   for _,x in pairs(tab) do
      if(x == val) then return true end
   end
   return false
end

local function distance(x, y)
   if(type(x) == "number" and type(y) == "number") then
      return math.sqrt(x*x+y*y)
   else -- accept tables
      return distance(x.x - y.x, x.y - y.y)
   end
end

local pairs_for = function(raw, wrap)
   return function(_)
      local t = {}
      for k,v in pairs(raw) do
         if(type(v) == "table") then
            t[k] = wrap(v)
         else
            t[k] = v
         end
      end
      return next,t,nil
   end
end

local ipairs_for = function(raw, wrap)
   return function(_)
      local t = {}
      for k,v in ipairs(raw) do
         if(type(v) == "table") then
            t[k] = wrap(v)
         else
            t[k] = v
         end
      end
      return next,t,nil
   end
end

local function readonly_proxy(source, table_name)
   table_name = table_name or "table"
   local t = {}
   local mt = {
      __index = function(_, key)
         if(type(source[key]) == "table") then
            return readonly_proxy(source[key])
         else
            return source[key]
         end
      end,
      __pairs = pairs_for(source, readonly_proxy),
      __ipairs = ipairs_for(source, readonly_proxy),
      __newindex = function(_, _, _) error(table_name .. " are read-only") end
   }
   setmetatable(t, mt)
   return t
end

return {
   shallow_copy = shallow_copy,

   keys = keys,

   vals = vals,

   includes = includes,

   split = function(str,div)
      if(div=='') then return {str} end
      if(div==str) then return {} end
      local pos,res = 0,{}
      for st,sp in function() return str:find(div,pos) end do
         local str2 = string.sub(str,pos,st-1)
         table.insert(res,str2)
         pos = sp + 1
      end
      table.insert(res,string.sub(str,pos))
      return res
   end,

   starts_with = function(s, start)
      return string.sub(s,1,string.len(start))==start
   end,

   concat = function(t1_orig, t2)
      local t1 = shallow_copy(t1_orig)
      for i=1,table.length(t2) do
         t1[table.length(t1)+1] = t2[i]
      end
      return t1
   end,

   take = function(n, t)
      local new = {}
      for i=1,n do
         table.insert(new, t[i])
      end
      return new
   end,

   drop = function(n, t)
      local new = {}
      for i=n+1,table.length(t) do
         table.insert(new, t[i])
      end
      return new
   end,

   whitelist_table = function(source, whitelist, table_name)
      table_name = table_name or "table"
      local t = {}
      local mt = {
         __index = function(_, key)
            if(includes(whitelist, key)) then
               if(type(source[key]) == "table") then
                  return readonly_proxy(source[key])
               else
                  return source[key]
               end
            end
         end,
         __pairs = pairs_for(source, readonly_proxy),
         __ipairs = ipairs_for(source, readonly_proxy),
         __newindex = function(_, _, _) error(table_name .. " are read-only") end
      }
      setmetatable(t, mt)
      return t
   end,

   readonly_proxy = readonly_proxy,

   mtlength = mtlength,

   distance = distance,

   format_seconds = function(s)
      local formatted, k = tostring(s)
      while k ~= 0 do
         formatted, k = string.gsub(formatted, "^(-?%d+)(%d%d%d)", '%1:%2')
      end
      return formatted
   end,

   gaussian_random = function(r)
      local x = 1 + math.sqrt(-2 * math.log(math.random()))
      local y = math.cos(2 * math.pi * math.random()) / 2
      return x * y * r
   end,
}
