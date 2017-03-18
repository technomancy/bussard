require("love.filesystem")

local t = love.thread.getThread()
local c = love.thread.getChannel(tostring(t))

local seed_users = require("data.seed_users")
local orb = require("os.orb")

local is_authorized = function(username, password)
   return (username == "guest" and password == "")
end

local fs_raw = orb.fs.new_raw()

local init_fs = function(name)
   local fs = orb.fs.proxy(fs_raw, "root")
   local users = {guest = ""}
   for username,u in pairs(seed_users[name] or {}) do
      if(username ~= "root") then
         users[u.username] = u.password
      end
   end
   orb.fs.seed(fs, users)

   for _,user in pairs(seed_users[name] or {}) do
      for fname,contents in pairs(user.files) do
         local dir,_ = os.fs.dirname(fname)
         os.fs.mkdir(fs, dir)
         fs[fname] = contents
      end
   end

   if(love.filesystem.isFile("data/motd/" .. name)) then
      fs.etc.motd = love.filesystem.read("data/motd/" .. name)
   end
end

local msg = c:demand()
assert(msg.command == "init", "Uninitialized thread.")
local name = msg.name
local sessions = {}
init_fs(name)

while true do
   msg = c:demand()
   if(msg.command == "kill") then return
   elseif(msg.command == "login") then
      if(is_authorized(msg.username, msg.password)) then
         sessions[msg.session] = {}
      else
         c:push({out="Login failed."})
      end
   elseif(msg.command == "debug") then
      pp(sessions)
   end
end
