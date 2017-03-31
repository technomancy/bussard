return {
   new_session = function(stdin, output, username, hostname)
      local thread = love.thread.newThread("os/lisp/session.lua")
      thread:start(stdin, output, username, hostname)
      return {}
   end,

   is_authorized = function(_, username, password)
      return username == "guest" and password == ""
   end,
}
