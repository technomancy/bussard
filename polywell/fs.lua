local lume = require("polywell.lume")
local has_lfs, lfs = pcall(require, "lfs") -- optional dependency

local function fs_for(base_path)
   local get_path = function(path)
      if(path:find("/") == 1) then return path end
      return base_path .. "/" .. path
   end

   local fs = {}
   local mt = {
      __separator = "/",
      __index = function(_, path)
         local f = io.open(path, "r")
         if(not f) then return nil end
         local _,_,code = f:read(1)
         f:close()
         if(code == 21) then -- directory
            return fs_for(get_path(path))
         else
            return table.concat(lume.array(io.lines(get_path(path))), "\n")
         end
      end,
      __newindex = function(_, path, contents)
         if(contents) then
            local f = io.open(get_path(path), "w")
            f:write(contents)
            f:close()
         end
      end,
      -- without LFS, it will be unable to provide file open completion
      __pairs = has_lfs and function()
         return lfs.dir(base_path)
      end,
   }
   setmetatable(fs, mt)
   return fs
end

return fs_for
