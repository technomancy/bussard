-- utils

-- mostly functions which are inexplicable omissions from the lua standard lib

orb.utils = {
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

   shallow_copy = function(orig)
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
   end,

   interp = function(s, tab)
      return (s:gsub('($%b{})', function(w) return tab[w:sub(3, -2)] or w end))
   end,

   includes = function(tab, val)
      for _,x in orb.utils.mtpairs(tab) do
         if(x == val) then return true end
      end
      return false
   end,

   mtpairs = function(tab)
      local mt = getmetatable(tab)
      if(mt) then
         return mt.__iterator(tab)
      else
         return pairs(tab)
      end
   end,

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

   size = function(t)
      local n = 0
      for _,_ in pairs(t) do n = n + 1 end
      return n
   end,

   -- for hashing passwords when minetest is absent
   exec_capture = function(cmd)
      local f = assert(io.popen(cmd, 'r'))
      local s = assert(f:read('*a'))
      f:close()
      return s
   end,

   sha1sum = function(string)
      local sum = orb.utils.exec_capture("echo " .. string .. "| sha1sum")
      return sum:gsub("[^0-9a-z]", "")
   end,

   get_password_hash = function(u, p)
      if(minetest) then
         return minetest.get_password_hash(u, p)
      else
         return orb.utils.sha1sum(u .. ":" .. p)
      end
   end,
}
