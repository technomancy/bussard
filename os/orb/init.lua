local shell = require("os.orb.shell")
local rpcs = require("os.orb.rpcs")

return {
   new_session = function(stdin, output, username, hostname)
      local env = shell.new_env(username, hostname)
      local thread = love.thread.newThread("os/orb/session.lua")
      thread:start(env, "smash", stdin, output, hostname, rpcs)
      return env
   end,
}
