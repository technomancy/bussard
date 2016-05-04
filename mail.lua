local lume = require("lume")
local meta = require("data.msgs.meta")
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
   return to == "captain@adahn.local" and "inbox" or to
end

local deliver_message = function(ship, msg_name)
   local msg = read(msg_name)
   msg_name = msg_name:gsub(".msg$", "")
   local folder = folder_for(msg)
   if(not ship.mail_delivered[msg_name]) then
      ship.api.docs.mail[folder][msg_name] = msg
      ship.api.docs.mail[folder]._unread[msg_name] = true
      ship.mail_delivered[msg_name] = true
   end
end

return {
   deliver = function(ship, system_name)
      local offset = utils.time(ship) - utils.game_start
      for when, name in pairs(meta) do
         if(offset > when) then
            deliver_message(ship, name)
         end
      end
   end
}
