require("love.timer")
local lume = require("lume")
local serpent = require("serpent")
local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
local map = require("os.rover.map")
local forth = require("os.rover.smolforth")

local _, _, stdin, output, hostname = ...

local dbg = os.getenv("DEBUG") and print or function() end
local pps = function(x) return serpent.block(x, serpent_opts) end
local print_trace = function(e) print(e, debug.traceback()) end

local rpc = function(fn, ...)
   output:push({op="rpc", fn=fn, args=lume.serialize({...})})
end

local map_ok, state = xpcall(map.load, print_trace, hostname)
if(not map_ok) then print(state) end

local write = function(...)
   local out = table.concat(lume.map({...}, tostring), " ")
   output:push({ op = "stdout", out = out })
end

local round = function(x) return math.floor(x+0.5) end

local relativize = function(rover, rect)
   local newrect = lume.extend({}, rect)
   newrect[1], newrect[2] = newrect[1] - rover[1], newrect[2] - rover[2]
   return newrect
end

local send_state = function()
   local rects = lume.map(state.rects, lume.fn(relativize, state.rover))
   rpc("rover_state", {rects=rects, r=state.dir, rover=state.rover})
end

local forward = function()
   local dist = 10
   assert(map.move(state,
                   round(dist*math.sin(state.dir)),
                   round(dist*-math.cos(state.dir))))
   local message = map.get_in_range(state, "messages")
   if(message) then write(message.msg .. "\n") end
   local event = map.get_in_range(state, "events")
   if(event and event.event) then rpc("record_event", event.event) end
   send_state()
end

local sandbox = {
   prompt = "] ",
   hostname = function() return hostname end,
   forward = forward,
   left = function()
      state.dir = state.dir - math.pi/2
      send_state()
   end,
   right = function()
      state.dir = state.dir + math.pi/2
      send_state()
   end,
}

sandbox.f, sandbox.l, sandbox.r = sandbox.forward, sandbox.left, sandbox.right

sandbox.login = function()
   local i, o = map.get_channels(map.get_in_range(state, "hosts"),
                                 state.login_range)
   if(i and o) then
      o:push({op="login", username="guest", password=""})
      local response = i:pop()
      while not response do response = i:pop() love.timer.sleep(0.01) end
      dbg("<<", pps(response))
      write((response.out or "") .. "\n")
      local session_id = response.session_id
      if(response.ok) then
         while not response or response.op ~= "disconnect" do
            love.timer.sleep(0.01)
            if(response) then
               output:push(response)
            end
            local from_ship = stdin:pop()
            if(from_ship) then
               from_ship.session_id = session_id
               o:push(from_ship)
            end
            response = i:pop()
         end
         rpc("set_prompt", sandbox.prompt)
      else
         write("Login problem:" .. pps(response) .. "\n")
      end
   else
      error("No terminal found.")
   end
end

local function read()
   local msg = stdin:demand()
   if(msg.op == "stdin" and msg.stdin == "logout") then return nil
   elseif(msg.op == "stdin") then
      return msg.stdin
   elseif(msg.op == "kill") then
      error("session terminated")
   else
      print("Unknown op!", lume.serialize(msg))
   end
end

rpc("set_prompt", sandbox.prompt)
write("\n" .. (state.motd or "") .. "\n")
rpc("split_editor", "*rover*", "rover")
send_state()

local ok, env = pcall(forth.make_env, read, write, sandbox,
                      "os/rover/smolforth/smolforth.fs")
if(not ok) then print(env) end
xpcall(forth.repl, print_trace, env)

rpc("split_editor")
output:push({op="disconnect"})
