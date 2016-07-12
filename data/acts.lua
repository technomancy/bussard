local mail = require("mail")

return {
   [1] = function(ship)
      local events = { "passenger2", "try_interportal",
                       "background_check", "invite_nari", }
      local msgs = {"nari-a-01.msg", "nari-a-02.msg", "nari-a-03.msg",
                    "nari-a-04.msg", "nari-a-05.msg", "nari-a-06.msg",
                    "nari-a-07.msg", "nari-a-08.msg", "nari-a-09.msg" }
      for _,e in ipairs(events) do ship.events[e] = ship.sandbox.os.time() end
      for _,m in ipairs(msgs) do mail.deliver_msg(ship, m) end
      table.insert(ship.upgrade_names, "life_support")
      ship.humans.nari = "companion"
   end,
   [2] = function(ship)
      local events = {"rot13-decrypt", "subnet"}
      local msgs = {"nari-decrypt-02.msg", "nari-decrypt-03.msg",
                    "nari-decrypted.msg",
                    "subnet.msg", "subnet2.msg", "subnet3.msg"}
      for _,e in ipairs(events) do ship.events[e] = ship.sandbox.os.time() end
      for _,m in ipairs(msgs) do mail.deliver_msg(ship, m) end
   end,
}
