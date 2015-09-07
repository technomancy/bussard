local body = require("body")

local sessions = {}

return {
   login = function(ship, target, username, password)
      local fs_raw = body.login(target, username, password)
      if(fs_raw) then
         local fs = target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = target.os.shell.new_env(username)

         env.IN = "/tmp/in"
         env.OUT = "/tmp/out"
         target.os.shell.exec(fs, env, "mkfifo " .. env.IN)
         fs[env.OUT] = ship.repl.print

         sessions[target.name] = {fs, env}
         target.os.process.spawn(fs, env, "smash")
         return "Login succeeded."
      else
         return "Login failed."
      end
   end,

   send_input = function(target, input)
      local fs, env = unpack(sessions[target.name])
      assert(fs and env, "Not logged into " .. target.name)
      local in_function = fs[env.IN]
      assert(in_function, "Missing session input file.")
      in_function(input)
   end,
}
