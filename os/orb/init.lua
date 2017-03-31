local shell = require("os.orb.shell")
local fs = require("os.orb.fs")

return {
   new_session = function(stdin, output, username, hostname)
      local env = shell.new_env(username, hostname)
      local thread = love.thread.newThread("os/orb/session.lua")
      thread:start(env, "smash", stdin, output, hostname)
      return env
   end,

   is_authorized = function(hostname, username, password)
      local ok, err = pcall(fs.init_if_needed, hostname)
      if(not ok) then print("auth err", err) return false end
      return shell.auth(username, password)
   end
}
