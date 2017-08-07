local lume = require("lume")
local utf8 = require("polywell.utf8")
local timed_msgs = require("data.msgs.timed")
local event_msgs = require("data.msgs.events")
local utils = require("utils")
local mission = require("mission")

local read = function(msg)
   return love.filesystem.read("data/msgs/" .. msg)
end

local folder_for = function(msg)
   local to = msg:match("To: ([^\n]+)")
   if(to == "captain@adahn.local" or
      msg:match("Cc: ([^\n]+)") == "captain@adahn.local") then
      return "inbox"
   else
      return utf8.gsub(to, "@news.local", "")
   end
end

local add_header = function(msg, header, value)
   local parts = lume.split(msg, "\n\n")
   if(not value or parts[1]:match("\n" .. header .. ": ")) then return msg end
   parts[1] = parts[1] .. "\n" .. header .. ": " .. tostring(value)
   return table.concat(parts, "\n\n")
end

local deliver_msg = function(ship, msg_name, allow_multiple, skip_notify)
   local msg = read(msg_name)
   msg_name = msg_name:gsub(".msg$", "")
   if(allow_multiple) then msg_name = lume.uuid() end
   if(msg and not ship.mail_delivered[msg_name]) then
      local folder = folder_for(msg)
      msg = add_header(msg, "Date", utils.format_time(utils.time(ship)))
      msg = add_header(msg, "Message-Id", allow_multiple and msg_name)
      ship.api.docs.mail[folder][msg_name] = msg
      ship.api.docs.mail[folder]._unread[msg_name] = true
      ship.mail_delivered[msg_name] = true
      if(not skip_notify) then
         ship.api.editor.print("Received mail; press f3 to view.\n")
      end
      return true
   else
      return false, "No message."
   end
end

return {
   -- check to see if any messages need delivering
   deliver = function(ship)
      local offset = utils.time(ship) - utils.game_start
      for when, name in pairs(timed_msgs) do
         if(type(when) == "number" and offset > when) then
            deliver_msg(ship, name)
         elseif(type(when) == "table" and ship.events[when[1]]) then
            -- can be timed for N seconds after an event, too
            local event_time = ship.events[when[1]] + when[2]
            if(utils.time(ship) > event_time) then
               deliver_msg(ship, name)
            end
         end
      end
   end,

   add_header = add_header,
   deliver_msg = deliver_msg,

   reply = function(ship, msg_id)
      if(event_msgs[msg_id]) then
         mission.record_event(ship, event_msgs[msg_id])
         return true
      elseif(mission.find(nil, msg_id)) then
         return mission.accept(ship, msg_id)
      elseif(love.filesystem.isFile("data/msgs/" .. msg_id)) then
         return deliver_msg(ship, msg_id)
      end
   end,

   -- three kinds of replyable messages; some accept missions, some trigger
   -- events, some trigger delivery
   replyable = function(msg_id)
      if(event_msgs[msg_id] or mission.find(nil, msg_id) or
         msg_id and love.filesystem.isFile("data/msgs/" .. msg_id)) then
         return true
      end
   end,
}
