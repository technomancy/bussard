local lume = require("lume")
local timed_msgs = require("data.msgs.timed")
local utils = require("utils")

-- Types of deliveries
-- [x] time-based
-- [ ] event-based
-- [ ] random?

local read = function(msg)
   return love.filesystem.read("data/msgs/" .. msg)
end

local folder_for = function(msg)
   local to = msg:match("To: ([^\n]+)")
   if(to == "jobs@news.local") then
      return "jobs"
   elseif(to == "captain@adahn.local") then
      return "inbox"
   else
      return to
   end
end

local add_date = function(msg, date)
   local parts = lume.split(msg, "\n\n")
   return parts[1] .. "\nDate: " .. utils.format_time(date) .. "\n\n" .. parts[2]
end

local deliver_message = function(ship, msg_name)
   local msg = read(msg_name)
   msg_name = msg_name:gsub(".msg$", "")
   local folder = folder_for(msg)
   if(not ship.mail_delivered[msg_name]) then
      print(add_date(msg, utils.time(ship)))
      ship.api.docs.mail[folder][msg_name] = add_date(msg, utils.time(ship))
      ship.api.docs.mail[folder]._unread[msg_name] = true
      ship.mail_delivered[msg_name] = true
   end
end

return {
   deliver = function(ship, _system_name)
      local offset = utils.time(ship) - utils.game_start
      for when, name in pairs(timed_msgs) do
         if(offset > when) then
            deliver_message(ship, name)
         end
      end
   end
}
