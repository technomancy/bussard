-- for a new game
local lume = require("lume")
local mail = require("mail")
local client = require("os.client")

local statuses = {
   " Press ctrl-enter to toggle flight mode\n\n" ..
      "Stellar proximity alert. Leaving hibernation mode.\n\n" ..
      "System diagnostics...\n" ..
      "Main engine...             " .. "[ FAIL ]\n" ..
      "RCS thrusters...           " .. "[  ok  ]\n" ..
      "Memory banks...            " .. "[  ok  ]\n" ..
      "Storage array...           " .. "[ WARN ]\n" ..
      "Backup storage system...   " .. "[  ok  ]\n" ..
      "Communication system...    " .. "[  ok  ]\n" ..
      "IPv8 subsystem...          " .. "[  ok  ]\n" ..
      "Periodic scheduler...      " .. "[ FAIL ]\n" ..
      "Heads-up display...        " .. "[  ok  ]\n" ..
      "Sensor array...            " .. "[  ok  ]\n" ..
      "Helm controls...           " .. "[ WARN ]\n" ..
      "Life support...            " .. "[ WARN ]\n" ..
      "Cargo loader...            " .. "[  ok  ]\n" ..
      "Auxiliary expansion bay... " .. "[  ok  ]\n", -- this is you!
   "Automated repair...        ", "[ FAIL ]\n",
   "Solar panel...             ", "[  ok  ]\n",
   "Reaction mass collector... ", "[  ok  ]\n",
   "Main battery...            ", "[ FAIL ]\n",
   "Auxiliary battery...       ", "[ WARN ]\n",
   "\nRepair systems offline; engaging emergency maintenance protocols.\n",
   "Received mail; press ctrl-m to view.\n"
}

return function(ship)
   ship.system_name = "L 668-21"
   ship.flag = "Katilay"
   ship.name = "Adahn"
   ship.api.docs = ship.api.docs or {}
   ship.api.docs.mail = ship.api.docs.mail or
      { inbox = { _unread = {},
                  manual = love.filesystem.read("data/msgs/manual.msg")},
        jobs = { _unread = {} },
        archive = { _unread = {} },
      }
   for _,v in pairs(love.filesystem.getDirectoryItems("data/src")) do
      ship.api.src[v] = ship.api.src[v] or love.filesystem.read("data/src/"..v)
   end

   local function deliver_on_login()
      if(client.is_connected(ship, "Merdeka Station")) then
         mail.deliver_msg(ship, "dex19-3.msg")
         lume.remove(ship.api.updaters, deliver_on_login)
      end
   end

   local function engine_disabled()
      ship.engine_on = false
      if(ship.events.engine_restart) then
         mail.deliver_msg(ship, "dex19-2.msg")
         lume.remove(ship.api.updaters, engine_disabled)
         table.insert(ship.api.updaters, deliver_on_login)
      end
   end

   local t = -1
   local function print_statuses(_, dt)
      t = t + dt
      if(t > love.math.random(1, 10)) then
         ship.api.editor.with_current_buffer("*console*",
                                             ship.api.editor.write,
                                             table.remove(statuses, 1))
         if(#statuses == 0) then
            mail.deliver_msg(ship, "dex19-1.msg")
            lume.remove(ship.api.updaters, print_statuses)
            table.insert(ship.api.updaters, engine_disabled)
         end
         t = 0
      end
   end
   table.insert(ship.api.updaters, print_statuses)
end
