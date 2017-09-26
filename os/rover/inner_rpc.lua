-- There are two kinds of RPCs involving rovers; the main RPC is when rover
-- functions call functions in the main thread to change ship state.
-- However, the player can log into the rover and from there log into other
-- hosts, and those hosts can make RPC calls back to the rover to set state
-- there; for example opening doors.

return {
   door = function(state, door_num, action)
      door_num = assert(tonumber(door_num), "No door specified")
      action = action or "toggle"
      for _,rect in ipairs(state.rects) do
         if(door_num == rect.door) then
            if(action == "close" or
               (rect.open and action == "toggle")) then
               rect.open = false
            elseif(action == "open" or
                   (not rect.open and action == "toggle")) then
               rect.open = true
            end
            return
         end
      end
      error("Door " .. door_num .. " not found.")
   end,
}
