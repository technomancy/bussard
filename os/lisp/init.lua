local _, compiler = require("os.lisp.l2l.compat"), require("os.lisp.l2l.compiler")
local reader = require("os.lisp.l2l.reader")
local lume = require("lume")
local portal_rc = love.filesystem.read("os/lisp/resources/portal.lsp")

local id_for = function(p)
   return tostring(p):match(": 0x(.+)")
end

local function repl(fs, env, sandbox)
   local print, io = (sandbox.print or print), (sandbox.io or io)
   print((fs.etc and fs.etc.motd) or ";; Welcome to the lisp REPL.")
   sandbox.set_prompt(">> ")
   while true do
      local str, complete_form, form, stream = "", false
      while not complete_form do
         local line = io.read("*line*")
         if line == nil then return end
         str = str .. " " .. (line or "")
         stream = reader.tofile(str)
         complete_form, form = pcall(reader.read, stream, true)

         if not complete_form then
            local metatable = getmetatable(form)
            if metatable ~= reader.UnmatchedLeftBraceException and
            metatable ~= reader.UnmatchedLeftParenException then
               print(form)
               break
            end
         end
      end
      if complete_form then
         local position = stream:seek("cur")
         local _ok, _form = pcall(reader.read, stream)
         if getmetatable(_form) ~= reader.EOFException then
            stream:seek("set", position)
            print("Unexpected input: "..stream:read("*all*"))
         else
            local ok, result = pcall(compiler.eval, form)
            print("=", result)
         end
      end
   end
end

return {
   shell = {
      auth = function(fs, username, password)
         if((username == "guest" and password == "") or
            (username == "admin" and password == "K'chua")) then
            return fs
         end
      end,
      new_env = function(user)
         -- the portal_rc login code should only be applied to guest logins
         return {USER = user, LOGIN = portal_rc}
      end,
      spawn = function(fs, env, sandbox)
         local co = coroutine.create(lume.fn(repl, fs, env, sandbox))
         local id = id_for(co)
         fs.proc[id] = { thread = co, id = id }
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
      mkdir = function(_, _) end,
      seed = function(fs)
         fs.bin = {
            portal = love.filesystem.read("os/lisp/resources/portal.scm"),
            repl = love.filesystem.read("os/lisp/resources/repl.scm")
         }
         fs.etc = { motd = "Welcome to Lisp." }
      end,
      strip_special = function(_) end,
   },

   name = "lisp",
}
