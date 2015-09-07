-- fake lil filesystem
orb.fs = {

   -- This gives us a raw filesystem that's just a table with permissions data
   new_raw = function()
      return {_user = "root", _group = "all", proc = {
                 _user = "root", _group = "all"
      }}
   end,

   mkdir = function(f, path, env)
      assert(f and path)
      if(path == "/") then return end
      local dir,base = orb.fs.dirname(orb.fs.normalize(path, env and env.CWD))
      local parent = f[dir]

      if(not parent) then orb.fs.mkdir(f, dir, env) end
      if(parent[base]) then return parent[base] end

      parent[base] = {
         _user = parent._user,
         _group = parent._group,
      }

      return parent[base]
   end,

   -- Actually returns both the dirname and the basename.
   -- for instance, "/path/to/file" returns "/path/to" and "file"
   dirname = function(path)
      local t = orb.utils.split(path, "/")
      local basename = t[#t]
      table.remove(t, #t)

      return "/" .. table.concat(t, "/"), basename
   end,

   -- read/write/append here are wrappers that help you work with
   -- function files, which is how pipes and other special devices are
   -- implemented.  When dealing with regular files, you can just grab
   -- them straight out of the filesystem as strings to read or drop strings
   -- into the directory to write.
   read = function(f, path)
      if(not path) then return io.read() end
      local contents = f[path]
      if(type(contents) == "string") then
         return contents
      elseif(type(contents) == "function") then
         return contents()
      else
         error("Tried to read " .. type(contents) .. " at " .. path)
      end
   end,

   write = function(f, path, content)
      if(not path) then io.write(content) return end
      local dir, base = orb.fs.dirname(path)
      local target = f[path]
      if(not target) then
         f[dir][base] = content
      elseif(type(target) == "string") then
         f[dir][base] = f[dir][base] .. content
      elseif(type(target) == "function") then
         target(content)
      else
         error("Tried to append to " .. type(target) .. " at " .. path)
      end
   end,

   add_user = function(f, user, password)
      local home = "/home/" .. user
      orb.fs.mkdir(f, home)
      f[home]._user = user
      f[home]._group = user
      orb.fs.mkdir(f, home .. "/bin")
      orb.fs.mkdir(f, home .. "/diginet")
      orb.fs.add_to_group(f, user, user)
      orb.fs.add_to_group(f, user, "all")
      orb.fs.mkdir(f, "/proc/" .. user)
      f.proc[user]._user = user
      f.proc[user]._group = user
      f.etc.passwords[user] = orb.utils.get_password_hash(user, password)
   end,

   add_to_group = function(f, user, group)
      assert(type(user) == "string" and type(group) == "string")
      local group_dir = f[orb.fs.normalize("/etc/groups/" .. group)]

      if(not group_dir) then
         group_dir = orb.fs.mkdir(f, "/etc/groups/" .. group)
         group_dir._user = user
      end

      group_dir._group = group
      group_dir[user] = user
   end,

   -- This is for copying stuff from the host OS into the virtualized OS.
   copy_to_fs = function(f, fs_path, real_path, resources)
      local dir, base = orb.fs.dirname(fs_path)
      local path = resources .. real_path
      local file = io.open(path, "r")
      dir = dir:gsub("^/", "")
      f[dir][base] = file:read("*all")
      file:close()
   end,

   load_bin = function(f)
      for real_path, fs_path in pairs({ls = "/bin/ls",
                                       mkdir = "/bin/mkdir",
                                       cat = "/bin/cat",
                                       env = "/bin/env",
                                       mv = "/bin/mv",
                                       cp = "/bin/cp",
                                       rm = "/bin/rm",
                                       echo = "/bin/echo",
                                       smash = "/bin/smash",
                                       chmod = "/bin/chmod",
                                       chgrp = "/bin/chgrp",
                                       chown = "/bin/chown",
                                       ps = "/bin/ps",
                                       grep = "/bin/grep",
                                       reload = "/bin/reload",
                                       mkfifo = "/bin/mkfifo",
                                       sudo = "/bin/sudo",
                                       adduser = "/bin/adduser",
                                       addgroup = "/bin/addgroup",
                                       passwd = "/bin/passwd",
      }) do
         orb.fs.copy_to_fs(f, fs_path, real_path, orb.dir.."/resources/")
      end
   end,

   -- Load up an empty filesystem. Provided users will be added as sudoers.
   seed = function(f, users)
      for _,d in pairs({"/etc", "/home", "/tmp", "/bin"}) do
         orb.fs.mkdir(f, d)
         f[d]._group = "all"
      end

      orb.fs.mkdir(f, "/etc/passwords")
      orb.fs.mkdir(f, "/etc/groups")
      f["/tmp"]._group_write = "true"

      for user, password in pairs(users) do
         orb.fs.add_user(f, user, password)
         orb.fs.add_to_group(f, user, "sudoers")
      end

      orb.fs.load_bin(f)
      return f
   end,

   normalize = function(path, cwd)
      if(path == ".") then return cwd end
      if(not path:match("^/")) then path = cwd .. "/" .. path end

      local final = {}
      for _,segment in pairs(orb.utils.split(path, "/")) do
         if(segment == "..") then
            table.remove(final, #final)
         else
            final[#final + 1] = segment
         end
      end

      return "/" .. table.concat(final, "/")
   end,

   dir_meta = function(dir)
      assert(dir, "Directory not found.")
      return dir._user, dir._group, dir._group_write
   end,

   readable = function(f, dir, user)
      if(user == "root") then return true end
      local owner, group = orb.fs.dir_meta(dir)
      return owner == user or orb.shell.in_group(f, user, group)
   end,

   writeable = function(f, dir, user)
      if(user == "root") then return true end
      local owner, group, group_write = orb.fs.dir_meta(dir)
      return owner == user or
         (group_write and orb.shell.in_group(f, user, group))
   end,

   reloaders = (orb.fs and orb.fs.reloaders) or {},

   -- Reload all of orb's own code, and reset the /bin directory.
   reloader = function(f)
      return function()
         dofile(orb.dir .. "/init.lua")
         orb.fs.load_bin(f)
      end
   end,

   strip_special = function(f)
      for k,v in orb.utils.mtpairs(f) do
         if(type(v) == "table") then
            orb.fs.strip_special(v)
         elseif(type(v) ~= "string") then
            f[k] = nil
         end
      end
   end,

   -- Proxying a raw filesystem has two purposes: one is to enforce filesystem
   -- permissions rules (this is done using a metatable) and one is to allow
   -- access using full filenames. For instance, these are equivalent:
   --
   -- f.home.technomancy.bin["myls"]
   -- f["/home/technomancy/bin/myls"]
   --
   -- Raw filesystems require the first style, but the latter works with
   -- proxied filesystems.
   --
   -- Be aware that f["/home"] will return another proxied subfilesystem
   -- that looks like a filesystem but is actually just sliced off at
   -- a subdirectory. This is a bit of a problem since calculating permissions
   -- requires access to the "/etc/groups" directory, which is why this
   -- function takes a raw_root argument as well.
   proxy = function(raw, user, raw_root)
      local descend = function(f, path, descending_user)
         local target = f
         for _,d in pairs(orb.utils.split(path, "/")) do
            if(d == "") then break end
            assert(target, "Not found: " .. path)
            -- readable here needs a fully-rooted fs to read groups
            assert(type(target) == "string" or
                      orb.fs.readable(raw_root, target, descending_user),
                   ("Not readable: " .. path .. " d: " .. d))
            target = target[d]
         end
         return target
      end

      local unreadable = function(_, v)
         return {_user = v._user, _group = v._group}
      end

      local f = {}
      local mt = {
         __index = function(_, path)
            local target = descend(raw, path, user)
            if(type(target) == "table") then
               return orb.fs.proxy(target, user, raw_root)
            else
               return target
            end
         end,

         __newindex = function(_, path, content)
            local segments = orb.utils.split(path, "/")
            local base = table.remove(segments, #segments)
            local target = descend(raw, "/"..table.concat(segments,"/"), user)

            assert(orb.fs.writeable(raw_root, target, user),
                   "Not writeable: " .. path)
            target[base] = content
         end,

         -- Unfortunately Lua 5.1 has no way to specify an iterator from the
         -- metatable, so this only works with orb.utils.mtpairs. =(
         __iterator = function(_)
            assert(orb.fs.readable(raw_root, raw, user), "Not readable")
            local f2 = {}
            for k,v in pairs(raw) do
               if(type(v) == "string" or type(v) == "function") then
                  f2[k] = v
               elseif(type(v) == "table" and
                      orb.fs.readable(raw_root, v, user)) then
                  f2[k] = orb.fs.proxy(v, user, raw_root)
               elseif(type(v) == "table") then
                  f2[k] = unreadable(k, v)
               else
                  print("Unknown file type: " .. k)
                  print(v)
               end
            end
            return next,f2,nil
         end,

         -- if getmetatable ever leaks into the sandbox, this leaks too
         raw_root = raw_root,
      }
      setmetatable(f, mt)

      -- Only need this for fs roots.
      if(raw == raw_root) then
         orb.fs.reloaders[f] = orb.fs.reloader(raw_root)
      end

      return f
   end,
}
