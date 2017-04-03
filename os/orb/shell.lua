-- shell
local lume = require("lume")
local fs = require("os.orb.fs")
local sandbox = require("os.orb.sandbox")

local shell = {}

shell.new_env = function(user, host)
   local home = "/home/" .. user
   return { PATH = "/bin:" .. home .. "/bin", PROMPT = "${CWD} $ ",
            SHELL = "/bin/smash", CWD = home, HOME = home,
            USER = user, HOST = host }
end

shell.exec = function(orig_env, command, extra_sandbox)
   local env = lume.merge({}, orig_env)
   local args = lume.array((command.." "):gmatch("(.-)( +)"))
   local executable_name = table.remove(args, 1)
   local try_run = function(executable_path)
      if(fs.exists(executable_path)) then
         local chunk = assert(loadstring(fs.read(executable_path),
                                         executable_name))
         local box = sandbox.make(shell, env, extra_sandbox)
         -- getting the filesystem metatable would be a security leak
         assert(not box.getmetatable, "Sandbox leak")
         setfenv(chunk, box)
         chunk(env, args)
         return true
      end
   end

   if(executable_name:match("^/")) then
      if try_run(executable_name) then return end
   else
      for _, d in pairs(lume.split(env.PATH, ":")) do
         local path = fs.normalize(d .."/".. executable_name, env.CWD)
         if try_run(path) then return end
      end
   end
   error(executable_name .. " not found.")
end

-- Like exec, but protected in a pcall.
shell.pexec = function(env, command, extra_sandbox)
   return pcall(function() shell.exec(env, command, extra_sandbox) end)
end

shell.auth = function(user, password)
   assert(user, "No username provided")
   assert(password, "No password provided")
   return fs.read("/etc/passwords/" .. user) ==
      fs.get_password_hash(user, password)
end

shell.sudo = function(env, user, args, extra_sandbox)
   assert(fs.in_group(env.USER, "sudoers"), "Must be in the sudoers group.")
   assert(fs.exists("/etc/passwords/" .. user) or user == "root",
          "User does not exist: " .. user)
   local new_env = shell.new_env(user, env.HOST)
   orb.shell.exec(new_env, args, extra_sandbox)
end

shell.change_password = function(user, old_password, new_password, repeat_new)
   assert(shell.auth(user, old_password), "Incorrect password for " .. user)
   assert(new_password == repeat_new, "New passwords do not match.")
   fs.write("/etc/passwords/" .. user,
            fs.get_password_hash(user, new_password))
end

return shell
