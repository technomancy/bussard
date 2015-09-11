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

local make_readonly = function(t, table_name)
   table_name = table_name or "table"
   local mt = {
      __newindex = function(_, _, _) error(table_name .. " is read-only") end
   }
   setmetatable(t, mt)
   return t
end

return {
   shallow_copy = shallow_copy,

   partial = function(f, ...)
      local partial_args = {...}
      return function(...)
         local new_args = shallow_copy(partial_args)
         local inner_args = {...}
         for _,v in ipairs(inner_args) do table.insert(new_args, v) end
         return f(unpack(new_args))
      end
   end,

   keys = function(t)
      local ks = {}
      for k,_ in pairs(t) do table.insert(ks, k) end
      return ks
   end,

   vals = function(t)
      local vs = {}
      for _,v in pairs(t) do table.insert(vs, v) end
      return vs
   end,

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
      for i=1,#t2 do
         t1[#t1+1] = t2[i]
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
      for i=n+1,#t do
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
                  return make_readonly(shallow_copy(source[key]))
               else
                  return source[key]
               end
            end
         end,
         __newindex = function(_, _, _) error(table_name .. " are read-only") end
      }
      setmetatable(t, mt)
      return t
   end,

   readonly_proxy = function(source, table_name)
      table_name = table_name or "table"
      local t = {}
      local mt = {
         __index = function(_, key)
            if(type(source[key]) == "table") then
               return make_readonly(shallow_copy(source[key]))
            else
               return source[key]
            end
         end,
         __newindex = function(_, _, _) error(table_name .. " are read-only") end
      }
      setmetatable(t, mt)
      return t
   end,

   calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end,

   format_seconds = function(s)
      local formatted, k = tostring(s)
      while k ~= 0 do
         formatted, k = string.gsub(formatted, "^(-?%d+)(%d%d%d)", '%1:%2')
      end
      return formatted
   end,

   read_file = function(filename)
      local f = io.open(filename, "r")
      local content = f:read("*all")
      f:close()
      return content
   end,
}
