require("love.filesystem")
require("love.math")
require("love.thread")

local output, input, os_name, hostname = ...
local os_ok, os = pcall(require, "os." .. os_name .. ".init")
if(not os_ok) then print("Couldn't load OS:", os) return end

local is_authorized = function(username, password)
   return (username == "guest" and password == "")
end

local sessions = {}

local new_session = function(username)
   local session_id = string.format("%x", love.math.random(4294967296))
   local stdin = love.thread.newChannel()
   sessions[session_id] = os.new_session(stdin, output, username, hostname)
   sessions[session_id].stdin = stdin
   return session_id
end

while true do
   local msg = input:demand()
   print(">", require("lume").serialize(msg))
   if(msg.op == "kill") then return
   elseif(msg.op == "login") then
      if(is_authorized(msg.username, msg.password)) then
         local trace = nil
         local handle = function() trace = debug.traceback() end
         local ok, session_id = xpcall(new_session, handle, msg.username)
         if(ok) then
            output:push({op="status", out="Success.", ok=true,
                         ["new-session"] = session_id})
         else
            output:push({op="status", out="Error: " .. trace})
         end
      else
         output:push({op="status", out="Login failed."})
      end
   elseif(msg.op == "stdin") then
      local session = sessions[msg.session]
      if(session) then
         session.stdin:push(msg.stdin)
      else
         print("Warning: no session", msg.session)
      end
   elseif(msg.op == "debug") then
      pp(sessions)
   end
end
