local lisp = require("os.lisp.l2l.core")
local lume = require("lume")

local id_for = function(p)
   return tostring(p):match(": 0x(.+)")
end

local lookup = function(f, path)
   local target = f
   for _,s in ipairs(lume.split(path, ".")) do
      target = f[s]
   end
   return target
end

return {
   shell = {
      auth = function(fs, username, password)
         return fs
      end,
      new_env = function(user)
         return {USER = user}
      end,
      spawn = function(fs, input, printer, sandbox)
         -- TODO: sandboxing, env, fs?
         local co = coroutine.create(lume.fn(lisp.repl, input,
                                             printer, sandbox.set_prompt))
         local id = orb.process.id_for(co)
         fs.proc[id] = { thread = co, id = id,
         }
      end,
   },
   process = {
      scheduler = function(f)
         for k,p in pairs(f.proc) do
            if(type(p) == "table" and p.thread) then
               if(coroutine.status(p.thread) == "dead") then
                  print("coro died")
                  f.proc[k] = nil
               else
                  local _, err = coroutine.resume(p.thread)
                  if err then print(err) end
               end
            end
         end
      end,
   },
   fs = {
      new_raw = function() return { proc = {} } end,
      proxy = function(x) return x end,
      dirname = function(_) end,
      mkdir = function(f, dir) end,
      seed = function(fs)
         fs.bin = {
            portal = love.filesystem.read("os/lisp/resources/portal.scm"),
            repl = love.filesystem.read("os/lisp/resources/repl.scm")
         }
         fs.etc = { motd = "Welcome to Lisp." }
      end,
      strip_special = function(f) end,
   },

   name = "lisp",
}
