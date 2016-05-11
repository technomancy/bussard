local lume = require("lume")
local timed_msgs = require("data.msgs.timed")
local event_msgs = require("data.msgs.events")
local utils = require("utils")
local mission = require("mission")

-- TODO: adapt messages in data/news to new mail system

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
   parts[1] = parts[1] .. "\nDate: " .. utils.format_time(date)
   return table.concat(parts, "\n\n")
end

local deliver_msg = function(ship, msg_name)
   local msg = read(msg_name)
   msg_name = msg_name:gsub(".msg$", "")
   if(msg and not ship.mail_delivered[msg_name]) then
      local folder = folder_for(msg)
      ship.api.docs.mail[folder][msg_name] = add_date(msg, utils.time(ship))
      ship.api.docs.mail[folder]._unread[msg_name] = true
      ship.mail_delivered[msg_name] = true
      return true
   else
      return false, "No message."
   end
end

return {
   deliver = function(ship, _system_name)
      local offset = utils.time(ship) - utils.game_start
      for when, name in pairs(timed_msgs) do
         if(offset > when) then
            deliver_msg(ship, name)
         end
      end
   end,

   deliver_msg = deliver_msg,

   reply = function(ship, msg_id)
      if(event_msgs[msg_id]) then
         mission.record_event(ship, event_msgs[msg_id])
         return true
      elseif(mission.find(msg_id)) then
         return mission.accept(ship, msg_id)
      elseif(love.filesystem.isFile("data/msgs/" .. msg_id)) then
         return deliver_msg(ship, msg_id)
      end
   end,

   replyable = function(msg_id)
      if(event_msgs[msg_id] or mission.find(msg_id) or
         love.filesystem.isFile("data/msgs/" .. msg_id)) then
         return true
      end
   end,
}
