local lume = require("lume")
local utils = require("utils")
local serpent = require("serpent")
local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
local rpcs = require("os.rover.rpcs")
local map = require("os.rover.map")

local _, _, stdin, output, hostname = ...

local dbg = os.getenv("DEBUG") and print or function() end
local pack = function(...) return {...} end
local pps = function(x) return serpent.block(x, serpent_opts) end
local print_trace = function(e) print(e, debug.traceback()) end

local map_ok, state = xpcall(map.load, print_trace, hostname)
if(not map_ok) then print(state) end

local write = function(...)
   local out = table.concat(lume.map({...}, tostring), " ")
   output:push({ op = "stdout", out = out })
end

local round = function(x) return math.floor(x+0.5) end

local forward = function(dist)
   dist = dist or 1
   assert(map.move(state, "r",
                   round(dist*math.sin(state.dir)),
                   round(dist*-math.cos(state.dir))))
   local x, y = assert(map.find_pos(state, "r"))
   if(state.messages and state.messages[x.."x"..y]) then
      write(state.messages[x.."x"..y] .. "\n")
      state.messages[x.."x"..y] = nil
   end
end
end

local sandbox = {write = write,
                 print = function(...)
                    write(tostring(...) .. "\n")
                 end,
                 hostname = hostname,
                 prompt = "] ",
                 loadstring = function(sandbox, code, chunkname)
                    local chunk, err = loadstring(code, chunkname)
                    if(chunk) then
                       setfenv(chunk, sandbox)
                       return chunk
                    else
                       return chunk, err
                    end
                 end,
                 forward = forward,
                 left = function() state.dir = state.dir - math.pi/2 end,
                 right = function() state.dir = state.dir + math.pi/2 end,
}

sandbox.f, sandbox.l, sandbox.r = sandbox.forward, sandbox.left, sandbox.right

function sandbox.read()
   -- TODO: send status report over output channel
   write("\n"..map.tostring(state).."\n")
   local msg = stdin:demand()
   if(msg.op == "stdin") then
      return msg.stdin
   elseif(msg.op == "complete") then
      -- we are turning read into a generic RPC dispatch point, which we
      -- will likely turn out to regret! but it's our only entry point now.
      local ok, err = pcall(function()
            local targets = utils.completions_for(msg.input, sandbox, ".", {})
            output:push({op="rpc", fn="completions",
                         args={targets, msg.input}})
      end)
      if(not ok) then
         print("OS handler error:", err)
         output:push({op="status", status="err", out=err})
      end
   elseif(msg.op == "kill") then
      error("session terminated")
   else
      print("Unknown op!", lume.serialize(msg))
   end
   return sandbox.read()
end

local eval = function(input)
   local chunk, err = sandbox:loadstring("return " .. input, "repl")

   if(err and not chunk) then -- maybe it's a statement, not an expression
      chunk, err = sandbox:loadstring(input, "repl")
      if(not chunk) then
         sandbox.print("! Compilation error: " .. err or "Unknown error")
         return false
      end
   end

   local trace
   local result = pack(xpcall(chunk, function(e)
                                 trace = debug.traceback()
                                 err = e end))
   if(result[1]) then
      local out, i = pps(result[2]), 3
      -- pretty-print out the values it returned.
      while i <= #result do
         out = out .. ', ' .. pps(result[i])
         i = i + 1
      end
      if(result[2] == sandbox.invisible) then
         sandbox.print_prompt()
         return true
      end
      sandbox.print(out)
   else
      -- display the error and stack trace.
      sandbox.print('! Evaluation error: ' .. err or "Unknown")
      local lines = lume.split(trace, "\n")
      for i,l in pairs(lines) do
         -- editor infrastructure wraps 8 levels of irrelevant gunk
         if(i < #lines - 8) then sandbox.print(l) end
      end
   end
end

local repl = function()
   while true do
      local input = sandbox.read()
      if(input == nil) then return end
      eval(input)
   end
end

local add_rpc = function(sb, name)
   sb[name] = function(...)
      local chan = love.thread.newChannel()
      output:push({op="rpc", fn=name, args={...}, chan=chan})
      local response = chan:demand()
      if(response[1] == "_error") then
         table.remove(response, 1)
         error(unpack(response))
      else
         return unpack(response)
      end
   end
   return sb
end

output:push({op="rpc", fn="set_prompt", args={sandbox.prompt}})
sandbox.print((state.motd or "") .. "\n")
xpcall(repl, print_trace, lume.reduce(rpcs, add_rpc, sandbox))
