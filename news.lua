local lume = require("lume")

local include = function(ship, b, meta)
   if(meta.fn) then return(meta.fn(ship, b, meta))
   elseif(meta.chance and math.random(100) < meta.chance) then return false
   elseif(meta.systems and not lume.find(meta.systems, ship.system_name)) then return false
   elseif(meta.worlds and not lume.find(meta.worlds, b.name)) then return false
   elseif(meta.govs and not lume.find(meta.govs, b.gov)) then return false
   else return true end
end

return {
   seed = function(ship, b, fs)
      local groups = love.filesystem.getDirectoryItems("data/news")
      b.os.fs.mkdir(fs, "/usr/news")
      for _,group in ipairs(groups) do
         fs.usr.news[group] = nil
         local msgs = love.filesystem.getDirectoryItems("data/news/" .. group)
         for _,basename in ipairs(msgs) do
            local _,_,name = basename:find("(.*)\\.msg")
            if(name) then
               local filename = "data/news/" .. group .. "/" .. basename
               local metaname = "data/news/" .. group .. "/" .. name .. ".lua"
               local meta = dofile(metaname)
               if(include(ship, b, meta)) then
                  b.os.fs.mkdir(fs, "/usr/news/" .. group)
                  fs.usr.news[group][name] = love.filesystem.read(filename)
               end
            end
         end
      end
   end
}
