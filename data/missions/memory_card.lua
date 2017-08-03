local mail = require("mail")

local coro = coroutine.create(function(ship)
      while(ship.system_name ~= "Wolf 294") do coroutine.yield() end
      mail.deliver_msg(ship, "dex19-memory-2.msg")
end)

return {
   init = function(ship) table.insert(ship.updaters, coro) end,
   name = "Access memory card",
   description = "Take the memory card to Nari on Solotogo (Wolf 294 system)" ..
      " to find out what it says.",

   objectives={"memory_card_delivered"},
   on_success = function(ship) mail.deliver_msg(ship, "nari-memory-01.msg") end,
}
