local mission = require("mission")
local body = require("body")

local sessions = {}

-- The I/O model for SSH sessions is pretty confusing, so listen up. Logging
-- in involves attempting to authenticate with the target, and storing a tuple
-- of {fs, env, fs_raw} in the sessions table. The first fs is a proxied table
-- which enforces access limitations for multi-user OSes.

-- From there it logs into orb (portal logins are different; using lisp_login)
-- by setting env.IN and env.OUT; the former to a FIFO file node, and the
-- latter to a function which writes its output to the console.

-- In the "ssh" mode of the editor, enter is rebound to `send_line`, which
-- looks up the env.IN FIFO node and puts the input into it, provided the ship
-- is within range.

-- Within remote OSes, `io.write` is bound to a function that calls env.OUT,
-- and `io.read` pulls from the env.IN FIFO. All the code that runs on the OS
-- uses the sandbox below.

local logout = function(ship, target)
   ship.api.editor.with_current_buffer("*console*", function()
                                          ship.api:activate_mode("console")
                                          ship.api.editor.set_prompt("> ")
   end)
   if(not target) then return ship.api.editor.invisible end
   local session = sessions[target.name]
   if(session) then
      local fs, env = unpack(session)
      -- guest account files are wiped on logout
      if(env.USER == "guest") then
         for k,_ in pairs(fs["/home/guest"] or {}) do
            if(k ~= "_user" and k ~= "_group") then
               session[1]["/home/guest/" .. k] = nil
            end
         end
      end
      sessions[target.name] = nil
   else
      (ship.api or ship).print("| Not logged in.")
   end
   return ship.api.editor.invisible
end

local send_line = function(ship, input)
   if(not ship.comm_connected) then
      logout(ship, nil) -- shouldn't happen, but get out of ssh mode anyway
   elseif(not ship:in_range(ship.target)) then
      ship.api.print("| Out of range. Run `logout` to disconnect" ..
                        " or move back in range.")
   elseif(not sessions[ship.target.name]) then
      ship.api.print("Not logged in to " .. ship.target.name ..
                        ". Run `logout` to disconnect.")
   else
      local fs, env = unpack(sessions[ship.target.name])
      assert(fs and env, "Not logged into " .. ship.target.name)
      local handle = fs[env.IN] or env.IN
      handle(input)
   end
end

local disconnect = function(ship)
   ship.api.editor.with_current_buffer("*console*", function()
                                          ship.api:activate_mode("console")
                                          ship.api.editor.set_prompt("> ")
   end)
   logout(ship, ship.target)
end

local get_connection = function(ship, username, password)
   if(not ship:in_range(ship.target)) then
      return ship.api.editor.print("| Out of communications range.")
   end

   assert(ship.target.os.name == "orb", "TODO: support non-orb")
   local fs_raw = body.login(ship, ship.target, username, password)
   if(fs_raw) then
      local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
      local env = ship.target.os.shell.new_env(username)
      local session_id = tostring(love.math.random(99999999))
      env.HOST = body.hostname(ship.target.name)
      env.OUT = "/tmp/out-" .. session_id

      env.IN = "/tmp/in-" .. session_id
      fs[env.IN] = function() return false; end

      mission.on_login(ship)
      return function(command)
         assert(fs, "Already logged out; establish a new connection.")
         if(command == "logout") then
            fs[env.OUT], fs, env = nil, nil, nil
            return
         end

         local output = ""
         fs[env.OUT] = function(...)
            for _,x in ipairs({...}) do
               output = output .. x
            end
         end

         assert((not command) or (type(command)=="string"),
            "Error running non-string command.")
         local sandbox = ship.target.os.sandbox
         ship.target.os.shell.exec(fs, env, command,
                                   sandbox(ship, env, fs_raw, logout))
         return output
      end
   else
      return nil, "Login failed."
   end
end

return {
   connect = function(ship, username, password, command)
      ship.api.closest_cycle = 1
      if(not ship:in_range(ship.target)) then
         return ship.api.editor.print("| Out of communications range.")
      end

      local fs_raw = body.login(ship, ship.target, username, password)
      if(fs_raw) then
         local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = ship.target.os.shell.new_env(username)

         env.HOST = body.hostname(ship.target.name)
         sessions[ship.target.name] = {fs, env, fs_raw}
         ship.comm_connected = ship.target.name

         assert(ship.target.os.login, "Unknown OS.")
         ship.target.os.login(fs, env, fs_raw, lume.fn(disconnect, ship),
                              ship, command)
         mission.on_login(ship)
      else
         ship.api.print("Login failed.")
      end
   end,

   -- for non-interactive use
   get_connection = get_connection,

   send_line = send_line,
   logout = logout,

   logout_all = function() end,
}
