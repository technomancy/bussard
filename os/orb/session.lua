-- This is the file launched in a new thread for each user session on login.

local lume = require("lume")
local shell = require("os.orb.shell")
local fs = require("os.orb.fs")

local env, command, stdin, output, hostname, rpcs = ...

local read = function() return stdin:demand() end

local write = function(...)
   local out = table.concat(lume.map({...}, tostring), " ")
   output:push({ op = "stdout", out = out })
end

local add_rpc = function(acc, name)
   acc[name] = function(...)
      local chan = love.thread.newChannel()
      output:push({op="rpc", fn=name, args={...}, chan=chan})
      return unpack(chan:demand())
   end
   return acc
end

local err_handler = function(e) print(e, debug.traceback()) end

xpcall(fs.init_if_needed, err_handler, hostname)
xpcall(shell.exec, err_handler,
       env, command, lume.reduce(rpcs, add_rpc, {read = read, write = write}))
