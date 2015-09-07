-- shell

orb.shell = {
   new_env = function(user)
      local home = "/home/" .. user
      return { PATH = "/bin:" .. home .. "/bin", PROMPT = "${CWD} $ ",
               SHELL = "/bin/smash", CWD = home, HOME = home, USER = user,
      }
   end,

   -- This function does too much: it turns a command string into a tokenized
   -- list of arguments, but it also searches the argument list for stdio
   -- redirects and sets up the environment's read/write appropriately.
   parse = function(f, env, command)
      local tokens
      if(type(command) == "string") then
         tokens = orb.utils.split(command, " +")
      elseif(not command) then
         return nil, {}
      else
         tokens = command
      end

      local args = {}
      local executable_name = table.remove(tokens, 1)
      local t = table.remove(tokens, 1)
      while t do
         if(t == "<") then
            env.IN = orb.fs.normalize(tokens[1], env.CWD)
            break
         elseif(t == ">") then
            local target = table.remove(tokens, 1)
            target = orb.fs.normalize(target, env.CWD)
            local dir, base = orb.fs.dirname(target)
            if(type(f[dir][base]) == "string") then f[dir][base] = "" end
            env.OUT = target
            break
         elseif(t == ">>") then
            local target = table.remove(tokens, 1)
            env.OUT = orb.fs.normalize(target, env.CWD)
            break
         -- elseif(t == "|") then
         --    -- TODO: support pipelines of arbitrary length
         --    -- TODO: IN and OUT as buffer tables?
         --    local env2 = orb.utils.shallow_copy(env)
         --    local buffer = {}
         --    env2.read = function()
         --       while #buffer == 0 do coroutine.yield() end
         --       return table.remove(buffer, 1)
         --    end
         --    env.write = function(output)
         --       table.insert(buffer, output)
         --    end
         --    local co = orb.process.spawn(f, env, table.concat(tokens, " "))
         --    break
         else
            table.insert(args, t)
         end
         t = table.remove(tokens, 1)
      end
      return executable_name, args
   end,

   -- Execute a command directly in the current coroutine. This is a low-level
   -- call; usually you want orb.process.spawn which creates it as a proper
   -- process.
   exec = function(f, orig_env, command, extra_sandbox)
      local env = orb.utils.shallow_copy(orig_env)
      local executable_name, args = orb.shell.parse(f, env, command)

      local try_run = function(executable_path)
         if(type(f[executable_path]) == "string") then
            local chunk = assert(loadstring(f[executable_path]))
            local sandbox = orb.shell.sandbox(f, env, extra_sandbox)
            -- getting the filesystem metatable would be a security leak
            assert(not sandbox.getmetatable, "Sandbox leak")
            setfenv(chunk, sandbox)
            chunk(f, env, args)
            return true
         end
      end

      if(executable_name:match("^/")) then
         if try_run(executable_name) then return end
      else
         for _, d in pairs(orb.utils.split(env.PATH, ":")) do
            local path = orb.fs.normalize(d .."/".. executable_name, env.CWD)
            if try_run(path) then return end
         end
      end
      error(executable_name .. " not found.")
   end,

   -- Like exec, but protected in a pcall.
   pexec = function(f, env, command, extra_sandbox)
      return pcall(function() orb.shell.exec(f, env, command, extra_sandbox) end)
   end,

   -- Set up the sandbox in which code runs. Need to avoid exposing anything
   -- that could allow security leaks.
   sandbox = function(f, env, extra_sandbox)
      local read = function() return orb.fs.read(f, env.IN) end
      local write = function(...) return orb.fs.write(f, env.OUT, ...) end

      -- env is just a table; it can be modified by any user script.
      -- Therefore any function exposed in the sandbox which trusts env.USER
      -- must be wrapped with this function which asserts that the USER
      -- value has not been modified.
      local lock_env_user = function(fn, env_arg_position)
         return function(...)
            local args = {...}
            assert(args[env_arg_position].USER == env.USER, "Changed USER!")
            return fn(...)
         end
      end

      local box = { orb = { utils = orb.utils,
                            mkdir = orb.fs.mkdir,
                            dirname = orb.fs.dirname,
                            normalize = orb.fs.normalize,
                            add_user = orb.fs.add_user,
                            add_to_group = orb.fs.add_to_group,
                            in_group = orb.shell.in_group,
                            change_password = orb.shell.change_password,
                            sudo = lock_env_user(orb.shell.sudo, 2),
                            exec = lock_env_user(orb.shell.exec, 2),
                            pexec = lock_env_user(orb.shell.pexec, 2),
                            read = orb.utils.partial(orb.fs.read, f),
                            write = orb.utils.partial(orb.fs.write, f),
                            append = orb.fs.append,
                            reload = orb.fs.reloaders[f],
                            extra_sandbox = extra_sandbox, },
                    pairs = orb.utils.mtpairs,
                    ipairs = ipairs,
                    unpack = unpack,
                    print = function(...)
                       write(tostring(...)) write("\n") end,
                    coroutine = { yield = coroutine.yield,
                                  status = coroutine.status },
                    io = { write = write, read = read },
                    tonumber = tonumber,
                    tostring = tostring,
                    math = math,
                    type = type,
                    table = { concat = table.concat,
                              remove = table.remove,
                              insert = table.insert,
                    },
                  }
      for k,v in pairs(extra_sandbox or {}) do box[k] = v end
      return box
   end,

   groups = function(f, user)
      local dir = f["/etc/groups"]
      local found = {}
      for group,members in orb.utils.mtpairs(dir) do
         if(type(members) == "table" and orb.utils.includes(members, user)) then
            table.insert(found, group)
         end
      end
      return found
   end,

   in_group = function(f, user, group)
      local group_dir = f.etc.groups[group]
      return group_dir and group_dir[user]
   end,

   auth = function(f, user, password)
      local raw_fs = (getmetatable(f) and getmetatable(f).raw_root) or f
      return raw_fs.etc.passwords[user] ==
         orb.utils.get_password_hash(user, password)
   end,

   sudo = function(f, env, user, args, extra_sandbox)
      local raw_fs = getmetatable(f).raw_root
      assert(orb.shell.in_group(raw_fs, env.USER, "sudoers"),
             "Must be in the sudoers group.")
      assert(raw_fs.etc.passwords[user] or user == "root",
             "User does not exist.")
      local new_f = orb.fs.proxy(raw_fs, user, raw_fs)
      local new_env = orb.shell.new_env(user)
      orb.shell.exec(new_f, new_env, args, extra_sandbox)
   end,

   change_password = function(f, user, old_password, new_password)
      assert(orb.shell.auth(f, user, old_password),
             "Incorrect password for "..user)
      local raw = getmetatable(f).raw_root
      raw.etc.passwords[user] = orb.utils.get_password_hash(user, new_password)
   end,
}
