-- a fake lil' OS
local serpent = require("serpent")
local utils = require("utils")

assert(setfenv, "Needs lua 5.1; sorry.")

orb = { dir = "os/orb", name="orb" }

require("os.orb.utils")
require("os.orb.fs")
require("os.orb.shell")
require("os.orb.process")

orb.sandbox = function(ship, env, fs_raw, disconnect)
   local services = require("services")
   local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
   local sb = {
      buy_user = lume.fn(services.buy_user, ship, ship.target, fs_raw),
      buy_upgrade = lume.fn(services.buy_upgrade, ship),
      sell_upgrade = lume.fn(services.sell_upgrade, ship),
      refuel = lume.fn(services.refuel, ship, ship.target),
      cargo_transfer = lume.fn(services.cargo_transfer, ship.target, ship),
      port = lume.fn(services.port, ship),
      loan = lume.fn(services.loan, ship),

      upgrade_help = ship.api.help.get,
      station = utils.readonly_proxy(ship.target),
      ship = ship.api,
      distance = lume.fn(utils.distance, ship, ship.target),
      os = {time = lume.fn(utils.time, ship)},
      term = { set_prompt = ship.api.editor.set_prompt,
               get_prompt = ship.api.editor.get_prompt },
      set_prompt = ship.api.editor.set_prompt,
      get_prompt = ship.api.editor.get_prompt,
      pps = function(x) return serpent.block(x, serpent_opts) end,
      original_env = utils.readonly_proxy(env),
   }
   sb.pp = function(x) sb.print(serpent.block(x, serpent_opts)) end

   ship.sandbox.logout = function()
      disconnect()
      ship.comm_connected = false
   end

   if(ship.target and ship.target.subnet and env.USER=="subnet") then
      sb.subnet = lume.fn(services.subnet.request, ship)
   end

   return lume.merge(utils.sandbox, sb)
end

orb.login = function(fs, env, fs_raw, disconnect, ship, command)
   env.IN, env.OUT = "/tmp/in", "/tmp/out"
   orb.shell.exec(fs, env, "mkfifo " .. env.IN)
   fs[env.OUT] = ship.api.write
   local fifo = fs[env.IN]
   fs[env.IN] = function(x)
     assert((not x) or (type(x)=="string") or
         ((type(x)=="table") and not pairs(x)(x)),
       "Error transmitting non-text data")
     return fifo(x)
   end
   assert((not command) or (type(command)=="string"),
      "Error running non-string command.")
   orb.process.spawn(fs, env, command,
                     orb.sandbox(ship, env, fs_raw, disconnect))
   -- without this you can't have non-shell SSH commands
   orb.process.scheduler(fs)
end

return orb
