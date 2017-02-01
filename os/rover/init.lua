local lume = require("lume")
local utils = require("utils")
local serpent = require("serpent")
local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}

local pack = function(...) return {...} end
local pps = function(x) return serpent.block(x, serpent_opts) end

local id_for = function(p)
   return tostring(p):match(": 0x(.+)")
end

local eval = function(input, sb)
   local chunk, err = sb:loadstring("return " .. input)

   if(err and not chunk) then -- maybe it's a statement, not an expression
      chunk, err = sb.loadstring(input)
      if(not chunk) then
         sb.print("! Compilation error: " .. err or "Unknown error")
         return false
      end
   end

   local trace
   local result = pack(xpcall(chunk, function(e)
                                 trace = debug.traceback()
                                 err = e end))
   if(result[1]) then
      local output, i = pps(result[2]), 3
      -- pretty-print out the values it returned.
      while i <= #result do
         output = output .. ', ' .. pps(result[i])
         i = i + 1
      end
      if(result[2] == sb.invisible) then
         sb.print_prompt()
         return true
      end
      sb.print(output)
   else
      -- display the error and stack trace.
      sb.print('! Evaluation error: ' .. err or "Unknown")
      local lines = lume.split(trace, "\n")
      for i,l in pairs(lines) do
         -- editor infrastructure wraps 8 levels of irrelevant gunk
         if(i < #lines - 8) then sb.print(l) end
      end
   end
end

local repl = function(sb)
   while true do
      local input = sb.io.read()
      if(input == nil) then return end
      eval(input, sb)
   end
end

local spawn = function(fs, _env, sb, _command)
   local co = coroutine.create(lume.fn(repl, sb))
   local id = id_for(co)
   fs.proc[id] = { thread = co, id = id }
end

local sandbox = function(ship, logout)
   local sb = {
      disconnect = function()
         ship.api.editor.with_current_buffer("*console*", function()
                                                ship.api:activate_mode("console")
                                                ship.api.editor.set_prompt("> ")
         end)
         logout(ship, ship.target)
      end,
   }
   sb.loadstring = function(sandbox, code, chunkname)
      local chunk, err = loadstring(code, chunkname)
      if(chunk) then
         setfenv(chunk, sandbox)
         return chunk
      else
         return chunk, err
      end
   end
   return lume.merge(utils.sandbox, sb)
end

local scheduler = function(f)
   for k,p in pairs(f.proc) do
      if(type(p) == "table" and p.thread) then
         if(coroutine.status(p.thread) == "dead") then
            f.proc[k] = nil
         else
            local _, err = coroutine.resume(p.thread)
            if err then print(err) end
         end
      end
   end
end

return {
   name = "rover",
   fs = { proxy = function(x) return x end,
          new_raw = function() return { proc = {} } end,
          seed = function() end,
        },
   shell = { new_env = function() return {} end,
             auth = function(fs) return fs end,
           },
   process = { scheduler = scheduler, },
   login = function(fs, env, _, logout, ship, command)
      local buffer = {}
      local max_buffer_size = 1024
      local sb = sandbox(ship, logout)
      local write = ship.api.write
      env.IN = function(...)
         local arg = {...}
         if(#arg == 0) then
            while #buffer == 0 do coroutine.yield() end
            return table.remove(buffer, 1)
         elseif(arg[1] == {}) then
            return buffer
         else -- write
            while(#buffer > max_buffer_size) do coroutine.yield() end
            for _,output in pairs(arg) do
               table.insert(buffer, output)
            end
         end
      end

      sb.io = sb.io or { read = env.IN, write = write }
      sb.print = ship.api.print
      sb.invisible = ship.api.editor.invisible
      sb.print_prompt = ship.api.editor.print_prompt

      ship.api.editor.set_prompt(">> ")
      spawn(fs, env, sb, command)
   end,
}
