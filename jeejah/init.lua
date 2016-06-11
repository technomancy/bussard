local socket = require "socket"
local serpent = require "serpent"
local bencode = require "bencode"

local timeout = 0.001

local pack = function(...) return {...} end
local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
local d = function(_) end
local p = function(x) print(serpent.block(x, serpent_opts)) end
local sessions = {}

local response_for = function(old_msg, msg)
   msg.session, msg.id, msg.ns = old_msg.session, old_msg.id, ""
   return msg
end

local send = function(conn, msg)
   d("Sending", bencode.encode(msg))
   conn:send(bencode.encode(msg))
end

local write_for = function(conn, msg)
   return function(...)
      send(conn, response_for(msg, {out=table.concat({...}, "\t")}))
   end
end

local print_for = function(write)
   return function(...)
      for _,x in ipairs({...}) do write(x) end
      write("\n")
   end
end

local sandbox_for = function(write, provided_sandbox)
   local sandbox = { io = { write = write },
                     print = print_for(write), }
   for k,v in pairs(provided_sandbox) do
      sandbox[k] = v
   end
   return sandbox
end

-- for stuff that's shared between eval and load_file
local execute_chunk = function(session, chunk)
   -- p(session)
   local old_write, old_print = io.write, print
   if(session.sandbox) then
      old_write = (session.sandbox.io or {}).write or io.write
      old_print = session.sandbox.print or print
      setfenv(chunk, session.sandbox)
   else
      -- TODO: redirect stdin
      _G.print = print_for(session.write)
      _G.io.write = session.write
   end

   local trace, err
   local result = pack(xpcall(chunk, function(e)
                                 trace = debug.traceback()
                                 err = e end))
   _G.print = old_print
   _G.io.write = old_write

   if(result[1]) then
      local res, i = serpent.block(result[2], serpent_opts), 3
      while i <= #result do
         res = res .. ', ' .. serpent.block(result[i], serpent_opts)
         i = i + 1
      end
      return res
   else
      return nil, (err or "Unknown error") .. "\n" .. trace
   end
end

local eval = function(session, code)
   local chunk, err = loadstring("return " .. code, "*socket*")
   if(err and not chunk) then -- statement, not expression
      chunk, err = loadstring(code, "*socket*")
      if(not chunk) then
         return nil, "Compilation error: " .. (err or "unknown")
      end
   end
   return execute_chunk(session, chunk)
end

local load_file = function(session, file)
   local chunk, err = loadfile(file)
   if(not chunk) then
      return nil, "Compilation error in " .. file ": ".. (err or "unknown")
   end
   return execute_chunk(session, chunk)
end

local register_session = function(conn, msg, provided_sandbox)
   local session = tostring(math.random(999999999))
   local write = write_for(conn, msg)
   local sandbox = provided_sandbox and sandbox_for(write, provided_sandbox)
   sessions[session] = { conn = conn, write = write, sandbox = sandbox }
   return response_for(msg, {["new-session"]=session, status="done"})
end

local session_for = function(conn, msg, sandbox)
   local s = sessions[msg.session] or register_session(conn, msg, sandbox)
   s.write = write_for(conn, msg)
   return s
end

local complete = function(msg, sandbox)
   local clone = function(t)
      local n = {} for k,v in pairs(t) do n[k] = v end return n
   end
   local top_ctx = clone(sandbox or _G)
   for k,v in pairs(msg.libs or {}) do
      top_ctx[k] = require(v:sub(2,-2))
   end

   local function cpl_for(input_parts, ctx)
      if type(ctx) ~= "table" then return {} end
      if #input_parts == 0 and ctx ~= top_ctx then
         return ctx
      elseif #input_parts == 1 then
         local matches = {}
         for k in pairs(ctx) do
            if k:find('^' .. input_parts[1]) then
               table.insert(matches, k)
            end
         end
         return matches
      else
         local token1 = table.remove(input_parts, 1)
         return cpl_for(input_parts, ctx[token1])
      end
   end
   local input_parts = {}
   for i in string.gmatch(msg.input, "([^.%s]+)") do
      table.insert(input_parts, i)
   end
   return response_for(msg, {completions = cpl_for(input_parts, top_ctx)})
end

-- see https://github.com/clojure/tools.nrepl/blob/master/doc/ops.md
local handle = function(conn, handlers, sandbox, msg)
   if(msg.op == "clone") then
      d("New session.")
      send(conn, register_session(conn, msg, sandbox))
   elseif(msg.op == "eval") then
      d("Evaluating", msg.code)
      local value, err = eval(session_for(conn, msg, sandbox), msg.code)
      d("Got", value, err)
      send(conn, response_for(msg, {value=value, ex=err}))
      send(conn, response_for(msg, {status="done"}))
   elseif(msg.op == "load-file") then
      d("Loading file", msg.file)
      local value, err = load_file(session_for(conn, msg, sandbox), msg.file)
      d("Got", value, err)
      send(conn, response_for(msg, {value=value, ex=err}))
      send(conn, response_for(msg, {status="done"}))
   elseif(msg.op == "complete") then
      d("Complete", msg.input)
      local session_sandbox = session_for(conn, msg, sandbox).sandbox
      send(conn, complete(msg, session_sandbox))
   elseif(msg.op == "stdin") then
      d("Stdin", serpent.block(msg))
      return -- TODO: implement
   elseif(msg.op == "interrupt") then
      d("Interrupt")
      return -- we can't do anything to interrupt, ignore silently
   elseif(msg.op == "describe") then
      d("Describe")
      write_for(conn, msg)("Describe is not supported.\n")
   elseif(handlers[msg.op]) then
      d("Custom op:", msg.op)
      handlers[msg.op](conn, msg, session_for(conn, msg, sandbox))
   else
      send(conn, response_for(msg, {status="unknown-op"}))
      print("  | Unknown op", serpent.block(msg))
   end
end

local function receive(conn, partial)
   local s, err = conn:receive(1) -- wow this is primitive
   coroutine.yield()
   if(s) then
      return receive(conn, (partial or "") .. s)
   elseif(err == "timeout" and partial == nil) then
      return receive(conn)
   elseif(err == "timeout") then
      return partial
   else
      return nil, err
   end
end

local function handle_loop(conn, sandbox, handlers)
   local input, r_err = receive(conn)
   if(input) then
      local decoded, d_err = bencode.decode(input)
      coroutine.yield()
      if(decoded) then
         handle(conn, handlers, sandbox, decoded)
      else
         print("  | Decoding error:", d_err)
      end
      return handle_loop(conn, sandbox, handlers)
   else
      return r_err
   end
end

local function loop(server, sandbox, handlers, connections)
   local conn, err = server:accept()
   coroutine.yield()
   if(conn) then
      conn:settimeout(timeout)
      d("Connected.")
      local coro = coroutine.create(function()
            local h_err = handle_loop(conn, sandbox, handlers, yield)
            d("Connection closed: " .. h_err)
      end)
      table.insert(connections, coro)
      return loop(server, sandbox, handlers, connections)
   else
      if(err ~= "timeout") then print("  | Socket error: " .. err) end
      for _,c in ipairs(connections) do coroutine.resume(c) end
      return loop(server, sandbox, handlers, connections)
   end
end

-- Start an nrepl socket server on the given host and port. For opts
-- you can pass a table with fg=true to run in the foreground, debug=true for
-- verbose logging, and sandbox={...} to evaluate all code in a sandbox.
-- You can also give an opts.handlers table keying ops to handler functions
-- which take the socket, the decoded message, and the optional sandbox table.
return function(host, port, opts)
   host, port = host or "localhost", port or 7888
   local server, err = assert(socket.bind(host, port))
   opts = opts or {}
   if(opts.debug) then d = print end
   if(opts.timeout) then timeout = tonumber(opts.timeout) end

   if(server) then
      server:settimeout(0.000001)
      print("Server started on " .. host .. ":" .. port .. "...")
      return coroutine.create(function()
            loop(server, opts.sandbox, opts.handlers, {})
      end)
   else
      print("  | Error starting socket repl server: " .. err)
   end
end
