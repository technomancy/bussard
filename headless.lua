-- headless runner to allow poking around in the repl alone

local f = function() end
local o = function() return 1 end

love = love or { graphics = { newImage = f, getWidth = o, getHeight = o,
                              newFont = f, setFont = f, getFont = f, },
                 keyboard = {
                    isDown = f,
                 },
                 filesystem = { save = f, newFile = f,}
               }

package.path = package.path .. ";?/init.lua"
local ship = require "main"

ship.api.repl.print = print

love.load()

return ship

-- inside a lua-repl session, run this:
-- > ship = require "headless"
--
-- > ship.x, ship.y
-- 17149
-- 39120
--
-- > love.update(1)
--
-- > ship.x, ship.y
-- 17145.566158361
-- 39112.130548087

--[[
ship = require "headless"
comm = require "comm"
utils = require "utils"

m = ship.system.bodies[2]
s = ship.system.bodies[1]
ship.target = m
ship.x, ship.y = m.x, m.y
comm.headless_login(ship, "guest", "", "cargo buy Food 2")

-- for backgrounded login sessions
ship.api.comm.login(ship.api, ship.target, "guest", "")
ship.api.comm.send_input(ship.api, "echo ohai > /tmp/hi")

fs = ship.api.comm.sessions["Mirduka station"][1]
m.os.process.scheduler(fs)
fs.tmp.hi
]]--
