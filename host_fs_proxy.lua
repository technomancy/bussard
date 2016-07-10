local host_fs_proxy_entry = function(name, readonly)
   if love.filesystem.isDirectory(name) then
      return host_fs_proxy(name, readonly)
   elseif love.filesystem.isFile(name) then
      return love.filesystem.read(name)
   end
end

local host_fs_proxy_pairs = function(prefix, readonly)
   return function(self)
      local t = {}
      for _, name in pairs(love.filesystem.getDirectoryItems(prefix)) do
         t[name] = host_fs_proxy_entry(prefix .. "/" .. name, readonly)
      end
      return next, t, nil
   end
end

local host_fs_proxy_set = function(prefix)
   return function(self, key, content)
      -- Setting the same value as currently stored is a NOP even for
      -- readonly proxies
      if(content==self[key]) then return end
      if(readonly) then
         error("This host FS proxy is readonly")
      else
         local name = prefix .. "/" .. key
         if(type(content)=="string") then
            if(love.filesystem.isDirectory(name)) then
               error("The path " .. name ..
                  " is a directory, cannot replace with a file")
            else
               assert(love.filesystem.write(name, content), "Could not write.")
            end
         elseif(type(content)=="table") then
            if(love.filesystem.exists(name)) then
               error("The path " .. name ..
                  "already exists, can't create a directory")
            else
               love.filesystem.createDirectory(name)
               local subproxy = host_fs_proxy(name, false)
               for k,v in pairs(content) do
                  subproxy[k]=v
               end
            end
         else
            error("Don't know how to use type " .. type(content))
         end
      end
   end
end

local host_fs_proxy_mt = function(prefix, readonly)
   return {
      __index = function(self, key)
         local name = prefix .. "/" .. key
         return host_fs_proxy_entry(name, readonly)
      end,
      __pairs = host_fs_proxy_pairs(prefix,readonly),
      __newindex = host_fs_proxy_set(prefix),
   }
end

local host_fs_proxy = function (prefix, readonly)
   local t = {}
   love.filesystem.createDirectory(prefix)
  setmetatable(t, host_fs_proxy_mt(prefix, readonly))
  return t
end

return {
   create = host_fs_proxy,
}
