-- Wrapping access to the real filesystem to check permissions and such.
local globtopattern = require("globtopattern").globtopattern
local lume = require("lume")
local lfs = require("love.filesystem")

local fs = {}

local realpath = function(path, cwd)
   return "fs/" .. fs.hostname .. "/" .. fs.normalize(path, cwd)
end

-- Actually returns both the dirname and the basename.
-- for instance, "/path/to/file" returns "/path/to" and "file"
fs.dirname = function(path)
   local t = lume.split(path, "/")
   local basename = t[#t]
   table.remove(t, #t)

   return "/" .. table.concat(t, "/"), basename
end

fs.expand_globs = function(input, env)
   local segments = lume.split(input, " ")
   local expand = function(token)
      local matches = {}
      if(token:match("*")) then
         local pattern = globtopattern(token)
         -- TODO: glob into directories
         for _,name in ipairs(lfs.getDirectoryItems(realpath(env.CWD))) do
            if(name:match(pattern) and not name:match("^_")) then
               table.insert(matches, name)
            end
         end
         return table.concat(matches, " ")
      else
         return token
      end
   end
   return table.concat(lume.map(segments, expand), " ")
end

fs.ls = function(path, cwd)
   local items = lfs.getDirectoryItems(realpath(path, cwd))
   local no_meta = function(entry)
      return not entry:match("/_meta$") and not entry:match("^_meta$")
   end
   return lume.filter(items, no_meta)
end

fs.read = function(path, cwd)
   assert(not path:match("/_meta$"), "Don't mess with fs meta!")
   local contents, _ = lfs.read(realpath(path, cwd))
   return contents
end

fs.write = function(path, cwd, content)
   assert(not path:match("/_meta$"), "Don't mess with fs meta!")
   return lfs.write(realpath(path, cwd), content)
end

fs.chown = function(path, cwd, user, group, group_write)
   assert(lfs.write(realpath(path .. "/_meta", cwd),
                    lume.serialize({user=user,group=group,
                                    group_write=group_write})))
end

fs.exists = function(path, cwd)
   return lfs.exists(realpath(path, cwd))
end

fs.isdir = function(path, cwd)
   return lfs.isDirectory(realpath(path, cwd))
end

fs.rm = function(path, cwd)
   lfs.remove(realpath(path, cwd))
end

fs.mkdir = function(path, cwd, env)
   if(fs.isdir(path, cwd)) then return end
   -- TODO: create dir meta for parents that lack it
   lfs.createDirectory(realpath(path, cwd))
   if(env) then
      fs.chown(path, cwd, env.USER, env.USER)
   else
      fs.chown(path, cwd, "root", "root")
   end
end

fs.dir_meta = function(dir, cwd)
   assert(lfs.isDirectory(realpath(dir, cwd)), dir .. " is not a directory.")
   assert(lfs.isFile(realpath(dir .. "/_meta", cwd)), dir .. " has no meta.")
   return lume.deserialize(lfs.read(realpath(dir .. "/_meta", cwd)))
end

fs.get_password_hash = function(u, p)
   return require("md5").sumhexa(u .. ":" .. p)
end

fs.add_user = function(user, password)
   local home = "/home/" .. user
   fs.mkdir(home)
   fs.chown(home, "/", user, user)
   fs.mkdir(home .. "/bin", "/")
   fs.chown(home .. "/bin", "/", user, user)
   fs.add_to_group(user, user)
   fs.add_to_group(user, "all")
   fs.write("/etc/passwords/" .. user, "/",
            fs.get_password_hash(user, password))
end

fs.add_to_group = function(user, group)
   assert(type(user) == "string" and type(group) == "string")
   local group_dir = "/etc/groups/" .. group

   if(not lfs.isDirectory(group_dir)) then
      fs.mkdir(group_dir, "/")
      fs.chown(group_dir, "/", user, user)
   end

   fs.write(group_dir .. "/" .. user, "/", "")
end

local load_bin = function()
   local files = lfs.getDirectoryItems("os/orb/resources/")
   for _,path in ipairs(files) do
      local real_path = "os/orb/resources/" .. path
      fs.write("/bin/" .. path, "/", assert(lfs.read(real_path)))
   end
end

-- Load up an empty filesystem. Provided users will be added as sudoers.
fs.seed = function(hostname, users)
   for _,d in pairs({"/", "/etc", "/home", "/tmp", "/bin"}) do
      fs.mkdir(d, "/")
      fs.chown(d, "/", "root", "all")
   end

   fs.mkdir("/etc/passwords", "/")
   fs.mkdir("/etc/groups", "/")
   fs.chown("/tmp", "/", "root", "all", true)

   for user, password in pairs(users or {}) do
      fs.add_user(user, password)
      fs.add_to_group(user, "sudoers")
   end
   if(lfs.exists("data/motd/" .. hostname)) then
      fs.write("/etc/motd", "/", lfs.read("data/motd/" .. hostname))
   end

   load_bin()
end

fs.normalize = function(path, cwd)
   if(path == ".") then return cwd end
   if(not path:match("^/")) then path = assert(cwd) .. "/" .. path end

   local final = {}
   for _,segment in pairs(lume.split(path, "/")) do
      if(segment == "..") then
         table.remove(final, #final)
      else
         final[#final + 1] = segment
      end
   end

   return "/" .. table.concat(final, "/")
end

fs.in_group = function(user, group)
   local group_dir = realpath("/etc/groups/" .. group)
   return lfs.isDirectory(group_dir) and lfs.isFile(group_dir .. "/" .. user)
end

fs.readable = function(f, user)
   if(user == "root") then return true end
   local dir = lfs.isDirectory(realpath(f)) and f or fs.dirname(f)
   local m = fs.dir_meta(dir)
   return m.user == nil or m.user == user or fs.in_group(user, m.group)
end

fs.writeable = function(f, user)
   if(user == "root") then return true end
   local dir = lfs.isDirectory(realpath(f)) and f or fs.dirname(f)
   local m = fs.dir_meta(dir)
   return m.user == nil or m.user == user or
      (m.group_write and fs.in_group(user, m.group))
end

fs.if_readable = function(user, f, for_dir)
   return function(path, cwd)
      local dir = for_dir and path or fs.dirname(path)
      assert(fs.readable(dir, user), dir .. " is not readable by " .. user)
      return f(path, cwd)
   end
end

fs.if_writeable = function(user, f, for_dir)
   return function(path, cwd, ...)
      local dir = for_dir and path or fs.dirname(path)
      assert(fs.writeable(dir, user), dir .. " is not writeable by " .. user)
      return f(path, cwd, ...)
   end
end

fs.init_if_needed = function(hostname)
   if(fs.hostname) then
      assert(hostname == fs.hostname,
             "Hostname mismatch! " .. hostname .. " / " .. fs.hostname)
   else
      fs.hostname = hostname
      fs.seed(hostname)
      fs.add_user("guest", "")
   end
end

return fs
