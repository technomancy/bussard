-- for a new game
local mission = require("mission")
local mail = require("mail")
local upgrade = require("data.upgrades")
local lume = require("lume")

local statuses = {
   " Press ctrl-enter to toggle flight mode.\n\n" ..
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
      "Primary expansion bay...   " .. "[  ok  ]\n", -- this is you!
   "Secondary expansion bay... ", "[  ok  ]\n", -- spacetime junction
   "Automated repair...        ", "[ FAIL ]\n",
   "Solar panel...             ", "[  ok  ]\n",
   "Reaction mass collector... ", "[  ok  ]\n",
   "Main battery...            ", "[ FAIL ]\n",
   "Auxiliary battery...       ", "[ WARN ]\n",
   "\nRepair systems offline; engaging emergency maintenance protocols.\n",
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
   table.insert(ship.upgrade_names, "underclocker")
   ship.api.actions.underclocker = upgrade.underclocker.action
   ship.upgrades.underclolcker = upgrade.underclocker
   for _,v in pairs(love.filesystem.getDirectoryItems("data/src")) do
      ship.api.src[v] = ship.api.src[v] or love.filesystem.read("data/src/"..v)
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
            lume.remove(ship.updaters, print_statuses)
         end
         t = 0
      end
   end
   mission.accept(ship, "init")
   table.insert(ship.updaters, print_statuses)
end
