-- This is the file launched in a new thread for each user session on login.

local lume = require("lume")
local shell = require("os.orb.shell")
local rpcs = require("os.orb.rpcs")
local fs = require("os.orb.fs")

local env, command, stdin, output, hostname = ...

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

local ok, err = pcall(fs.init_if_needed, hostname)
if(not ok) then print("init err:", err) return false end

xpcall(shell.exec, function(e) print(e, debug.traceback()) end,
       env, command, lume.reduce(rpcs, add_rpc, {read = read, write = write}))
