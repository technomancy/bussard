local lume = require("lume")

local dofile_if_exists = function(filename)
   if(love.filesystem.isFile(filename)) then
      return love.filesystem.load(filename)()
   end
end

local include = function(ship, b, m)
   local happened = function(event) return ship.events[event] end
   if(m.fn) then return(m.fn(ship, b, m))
   elseif(m.chance and math.random(100) < m.chance) then return false
   elseif(m.systems and not lume.find(m.systems, ship.system_name)) then return false
   elseif(m.worlds and not lume.find(m.worlds, b.name)) then return false
   elseif(m.govs and not lume.find(m.govs, b.gov)) then return false
   elseif(m.prereqs and not lume.all(m.prereqs, happened)) then return false
   elseif(m.restrictions and lume.any(m.restrictions, happened)) then return false
   elseif(m.mission_id and not lume.find(ship.active_missions, m.mission_id)) then return false
   elseif(m.prereq_fn and not m.prereq_fn(ship)) then return false
   else return true end
end

return {
   seed = function(ship, b, fs)
      if(b.os.name ~= "orb") then return end
      local groups = love.filesystem.getDirectoryItems("data/news")
      b.os.fs.mkdir(fs, "/usr/news")
      for _,group in ipairs(groups) do
         fs.usr.news[group] = nil
         local msgs = love.filesystem.getDirectoryItems("data/news/" .. group)
         for _,basename in ipairs(msgs) do
            local _,_,name = basename:find("(.*).msg")
            local group_meta = dofile_if_exists("data/news/" .. group .. ".lua")
            if(name) then
               local filename = "data/news/" .. group .. "/" .. basename
               local metaname = "data/news/" .. group .. "/" .. name .. ".lua"
               local meta = lume.merge(group_meta or {},
                                       dofile_if_exists(metaname) or {})
               if(include(ship, b, meta)) then
                  b.os.fs.mkdir(fs, "/usr/news/" .. group)
                  fs.usr.news[group][name] = love.filesystem.read(filename)
               end
            end
         end
      end
   end
}
