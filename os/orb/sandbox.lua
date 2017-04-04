-- Set up the sandbox in which code runs. Need to avoid exposing anything
-- that could allow security leaks.

local lume = require("lume")
local utils = require("utils")
local fs = require("os.orb.fs")

return {
   make = function(shell, env, extra)
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

      local box = { orb = { dirname = fs.dirname,
                            normalize = fs.normalize,
                            expand_globs = fs.expand_globs,
                            -- TODO: protect these
                            add_user = fs.add_user,
                            add_to_group = fs.add_to_group,
                            in_group = fs.in_group,
                            change_password = shell.change_password,
                            sudo = lock_env_user(shell.sudo, 1),
                            exec = lock_env_user(shell.exec, 1),
                            pexec = lock_env_user(shell.pexec, 1),
                            extra_sandbox = extra, },
                    print = function(...)
                       extra.write(tostring(...) .. "\n")
                    end,
                    io = { write = extra.write,
                           read = extra.read,

                           exists = fs.if_readable(env.USER, fs.exists, true),
                           ls = fs.if_readable(env.USER, fs.ls, true),
                           isdir = fs.if_readable(env.USER, fs.isdir),
                           readfile = fs.if_readable(env.USER, fs.read),

                           writefile = fs.if_writeable(env.USER, fs.write),
                           rm = fs.if_writeable(env.USER, fs.rm),
                           mkdir = fs.if_writeable(env.USER,
                                                   lock_env_user(fs.mkdir, 3)),
                    },
                  }

      box.loadstring = function(s)
         local chunk = loadstring(s)
         if(chunk) then setfenv(chunk, box) end
         return chunk
      end

      return lume.merge(box, utils.sandbox, extra)
   end
}
