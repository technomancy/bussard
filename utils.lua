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

return {
   shallow_copy = shallow_copy,

   partial = function(f, ...)
      local partial_args = {...}
      return function(...)
         local new_args = orb.utils.shallow_copy(partial_args)
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

   includes = function(tab, val)
      for _,x in orb.utils.mtpairs(tab) do
         if(x == val) then return true end
      end
      return false
   end,

   split = function(str,div)
      if(div=='') then return {str} end
      if(div==str) then return {} end
      local pos,res = 0,{}
      for st,sp in function() return str:find(div,pos) end do
         local str = string.sub(str,pos,st-1)
         if(str ~= "") then table.insert(res,str) end
         pos = sp + 1
      end
      table.insert(res,string.sub(str,pos))
      return res
   end,

   concat = function(t1, t2)
      local t1 = shallow_copy(t1)
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
}
