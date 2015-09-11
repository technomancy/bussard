-- headless runner to allow poking around in the repl alone

local f = function() end
local o = function() return 1 end

love = { graphics = { newImage = f, getWidth = o, getHeight = o,
                      newFont = f, setFont = f, getFont = f,
                    },
         keyboard = {
            isDown = f,
         },
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

m = ship.system.bodies[2]
ship.target = m
ship.x, ship.y = m.x, m.y
ship.api.comm.login(ship.api, ship.target, "guest", "")
ship.api.comm.send_input(ship.api, "echo ohai > /tmp/hi")

fs = ship.api.comm.sessions["Mirduka station"][1]
fs.tmp.hi
]]--
