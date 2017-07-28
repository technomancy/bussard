local utils = require("utils")
local threads = {}

local can_move_to = function(state, x, y)
   for _,rect in pairs(state.rects or {}) do
      local rw, rh = state.rover[3], state.rover[4]
      local rover = {x-rw/2, y-rh/2, rw, rh}
      if(utils.rect_overlap(rect, rover)) then return false end
   end
   return true
end

return {
   load = function(name)
      local chunk = assert(love.filesystem.load("data/maps/" .. name .. ".lua"))
      local state = chunk()

      for _,host in pairs(state.hosts or {}) do
         local t = {}
         t.input, t.output = love.thread.newChannel(), love.thread.newChannel()
         t.thread = love.thread.newThread("os/server.lua")
         t.thread:start(t.input, t.output, host.os, host.name)
         threads[host] = t
      end

      state.dir, state.login_range = state.dir or 0, state.login_range or 5
      return state
   end,

   move = function(state, dx, dy)
      local new_x, new_y = state.rover[1]+dx, state.rover[2]+dy
      if(can_move_to(state, new_x, new_y)) then
         state.rover[1], state.rover[2] = new_x, new_y
         return true
      else
         return false, "obstructed at " .. new_x .. "x" .. new_y
      end
   end,

   get_in_range = function(state, field, range)
      range = range or 0
      local x, y = state.rover[1] - range, state.rover[2] - range
      local w, h = state.rover[3] + range, state.rover[4] + range
      for _,target in pairs(state[field] or {}) do
         if(utils.rect_overlap(target, {x,y,w,h})) then
            return target
         end
      end
   end,

   get_channels = function(host)
      if(threads[host]) then
         return threads[host].input, threads[host].output
      end
   end,
}
