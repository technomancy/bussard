local socket = require("socket")
local serpent = require("serpent")

local pack = function(...) return {...} end

local eval = function(conn, ship, input)
   local p = function(x) conn:send(x .. "\n") end
   local chunk, err = ship.sandbox.loadstring("return " .. input, "*socket*")
   if(err and not chunk) then -- statement, not expression
      chunk, err = ship.sandbox.loadstring(input, "*socket*")
      if(not chunk) then
         p("! Compilation error: " .. (err or "Unknown error"))
         conn:send(ship.api.editor.prompt())
         return false
      end
   end
   local trace
   local result = pack(xpcall(chunk, function(e)
                                 trace = debug.traceback()
                                 err = e end))
   if(result[1]) then
      local output, i = serpent.block(result[2]), 3
      if result[2] == ship.api.editor.invisible then
         conn:send(ship.api.editor.prompt())
         return true
      end
      while i <= #result do
         output = output .. ', ' .. serpent.block(result[i])
         i = i + 1
      end
      p(output)
   else
      p('! Evaluation error: ' .. (err or "Unknown"))
      local lines = lume.split(trace, "\n")
      for i,l in pairs(lines) do
         -- editor infrastructure wraps 8 levels of irrelevant gunk
         if(i < #lines - 8) then p(l) end
      end
   end
   conn:send(ship.api.editor.prompt())
end

local function loop(server, ship)
   local conn, err = server:accept()
   coroutine.yield()
   if(conn) then
      conn:settimeout(0.001)
      conn:send(ship.api.editor.prompt())
      while true do
         local input, err_r = conn:receive()
         coroutine.yield()
         if(input) then
            xpcall(lume.fn(eval, conn, ship, input), print)
         end
         if(err_r == "closed") then return loop(server, ship) end
      end
   else
      if(err ~= "timeout") then print("socket error: " .. err) end
      return loop(server, ship)
   end
end

return {
   start = function(ship, port)
      local server, err = assert(socket.bind("localhost", port))
      if(server) then
         server:settimeout(0.001)
         local coro = coroutine.create(lume.fn(loop, server, ship))
         ship.api.updaters.socket = function() coroutine.resume(coro) end
      else
         print("Error starting socket repl server: " .. err)
      end
   end,
}
