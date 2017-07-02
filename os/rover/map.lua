local lume = require("lume")

local threads = {}

local find_pos = function(state, target)
   for y,line in ipairs(state) do
      for x,tile in ipairs(line) do
         if(tile == target) then
            return x, y
         end
      end
   end
   return nil, "Not found: " .. target
end

local is_open = function(state, x, y)
   return state[y][x] == " "
end

local dirs = {[0]="^", [1]=">", [2]="v", [3]="<"}
local dir_for = function(dir)
   dir = math.mod(dir+math.pi*2, math.pi*2)
   return dirs[math.floor(dir/(math.pi/2)+0.5)] or "^"
end

return {
   load = function(name)
      local chunk = assert(love.filesystem.load("data/maps/" .. name .. ".lua"))
      local state = chunk()
      for i,line in ipairs(state) do
         state[i] = {}
         for j=1,string.len(line) do state[i][j] = line:sub(j,j) end
      end

      for pos,host in pairs(state.hosts) do
         local t = {}
         t.input, t.output = love.thread.newChannel(), love.thread.newChannel()
         t.thread = love.thread.newThread("os/server.lua")
         t.thread:start(t.input, t.output, host.os, host.name)
         threads[pos] = t
      end

      state.dir = 0
      return state
   end,

   move = function(state, item, dx, dy)
      local x, y = assert(find_pos(state, item))
      if(is_open(state, x+dx, y+dy)) then
         state[y][x] = " "
         state[y+dy][x+dx] = item
         return true, x+dx, y+dy
      else
         return false, "obstructed at " .. x+dx .. "x" .. y+dy
      end
   end,

   tostring = function(state)
      local x, y = assert(find_pos(state, "r"))
      state[y][x] = dir_for(state.dir) or "x"
      local lines = lume.map(state, table.concat)
      state[y][x] = "r"
      return table.concat(lines, "\n")
   end,

   get_channels = function(pos)
      if(threads[pos]) then
         return threads[pos].input, threads[pos].output
      end
   end,

   find_pos = find_pos,
}
