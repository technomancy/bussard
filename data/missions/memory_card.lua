local mission = require("mission")
local mail = require("mail")

-- local decoded = function() end -- TODO: implement

local coro = coroutine.create(function(ship)
      while(ship.system_name ~= "Wolf 294") do coroutine.yield() end
      mail.deliver_msg(ship, "dex19-memory-2.msg")
      mission.wait_for(ship, "memory_card_delivered")
      -- mail.deliver_msg(ship, "nari-decode-1.msg")
      -- mission.wait_for(ship, decoded)
end)

return {
   init = function(ship) table.insert(ship.updaters, coro) end,
   name = "Access memory card",
   description = "Take the memory card to Nari on Solotogo (Wolf 294 system) to" ..
      "find out what it says.",

   objectives = {"decode"},
   -- on_success = function(ship) mail.deliver_msg(ship, "dex19-decode-3.msg") end,
}
