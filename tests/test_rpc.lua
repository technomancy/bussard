local t = require("lunatest")
local lume = require("lume")

local ship = require("ship")
local body = require("body")
local client = require("os.client")

local exec = function(command)
   local send, recv = ship.sandbox.ssh_connect("guest", "")
   assert(send, "Could not connect")
   recv(true) recv(true) -- discard motd, set_prompt
   send(command)
   local val = recv(true)
   send("logout")
   recv(true) recv(true) -- discard set_prompt, etc
   return val
end

local eval = function(code)
   local chunk = assert(ship.sandbox.loadstring(code))
   return chunk()
end

local pass = function()
   love.timer.sleep(0.1)
   client.update(ship, true)
end

local function test_loans()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.target_number, ship.target = 2, solotogo
   ship.credits, ship.loan = 128, 0

   -- initial borrow
   exec("loan borrow 128")
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- attempting to exceed credit limit
   exec("loan borrow 10000")
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- repaying
   exec("loan repay 154")
   t.assert_equal(102, ship.credits)
   t.assert_equal(0, ship.loan)
end

local test_completion = function()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.target_number, ship.target = 2, solotogo

   eval("ssh_activate('guest', '')")
   ship.api.editor.textinput("/bin/up", true)
   ship.api.editor.end_of_buffer()
   pass()
   ship.api.editor.keypressed("tab")
   ship.api.editor.end_of_buffer()
   pass()

   local first_completed = ship.api.editor.get_line()
   ship.api.editor.keypressed("return")
   pass()

   ship.api.editor.textinput("bi", true)
   ship.api.editor.end_of_buffer()
   -- ship.api.editor.debug(true)
   ship.api.editor.keypressed("tab")
   pass()

   local second_completed = ship.api.editor.get_line()
   ship.api.editor.get("ssh_send")("logout")
   pass()

   t.assert_equal("/home/guest $ /bin/upgrade", first_completed)
   t.assert_equal("/home/guest $ bin/", second_completed)
end

if(love.getVersion and ({love.getVersion()})[2] > 9) then
   return {test_loans=test_loans, test_completion=test_completion}
else
   return {}
end
