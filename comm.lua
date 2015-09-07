local body = require("body")

local sessions = {}

return {
   login = function(ship, target, username, password)
      local fs_raw = body.login(target, username, password)
      if(fs_raw) then
         local fs = target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = target.os.shell.new_env(username)
         local out_buffer = {}

         env.IN = "/tmp/in"
         env.OUT = "/tmp/out"
         target.os.shell.exec(fs, env, "mkfifo " .. env.IN)

         -- buffer output that happens when out of range
         fs[env.OUT] = function(output)
            -- repl doesn't have an io:write equivalent
            output = output:gsub("\n$", "")
            if(ship.sensors.in_range(ship.sensors, target)) then
               ship.repl.print(output)
            else
               print("out of range, buffering")
               -- TODO: readonly proxy is interfering with updates to target
               print(ship.sensors.x .. ", " .. ship.sensors.y)
               print(target.x .. ", " .. target.y)
               print(target)
               table.insert(out_buffer, function()
                               assert(target)
                               if(ship.sensors.in_range(ship.sensors, target)) then
                                  ship.repl.print(output) end end)
            end
         end

         sessions[target.name] = {fs, env, out_buffer}
         target.os.process.spawn(fs, env, "smash")
         return "Login succeeded."
      else
         return "Login failed."
      end
   end,

   send_input = function(ship, target, input)
      if(not ship.sensors.in_range(ship.sensors, target)) then
         ship.repl.print("| Out of range.")
         return
      end

      local fs, env = unpack(sessions[target.name])
      assert(fs and env and fs[env.IN], "Not logged into " .. target.name)
      fs[env.IN](input)
   end,

   flush = function()
      for _,v in pairs(sessions) do
         local _, _, out_buffer = unpack(v)
         for _,f in ipairs(out_buffer) do f() end
      end
   end
}
