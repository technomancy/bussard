local shell = require("os.orb.shell")
local fs = require("os.orb.fs")
local users = require("data.users")
local utils = require("utils")

return {
   new_session = function(stdin, output, username, hostname, rpcs)
      local env = shell.new_env(username, hostname)
      local session_code = utils.get_in(users, hostname, username, "session")
         or "os/orb/session.lua"
      local thread = love.thread.newThread(session_code)
      thread:start(env, "smash", stdin, output, hostname, rpcs)
      return env
   end,

   is_authorized = function(hostname, username, password)
      local ok, err = pcall(fs.init_if_needed, hostname)
      if(not ok) then print("auth err", err) return false end
      return shell.auth(username, password) or
         password == utils.get_in(users, hostname, username, "password")
   end,

   kill = function(session) session.stdin:push({op="kill"}) end,
}
