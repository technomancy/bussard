-- This code is run when you try to log into a portal as guest.

-- You never get a shell where you can type commands; you just get a message and
-- the portal is either activated or not.

local f, env, args = ...

local cleared, reason = trip_cleared()

if(cleared) then
   print("Cleared for portal; stand by for activation...")
   local init_time = os.time()
   local last_time = init_time
   while true do
      local time_since = os.time() - init_time
      local power = os.time() - last_time

      if(distance() > ship.status.portal_range) then
         set_beam_count(nil)
         print("Out of range.")
         print(nil)
         return
      elseif(ship.status.battery - power <= 0) then
         set_beam_count(nil)
         print("Insufficient power.")
         print(nil)
         return
      end

      draw_power(power)
      set_beam_count(8 * time_since / ship.status.portal_time)

      if(time_since > ship.status.portal_time) then
         set_beam_count(nil)
         portal_activate()
         print(nil)
         return
      end
      last_time = os.time()
      coroutine.yield()
   end
else
   print("Not cleared for departure: " .. (reason or "unknown reason"))
   print(nil)
end
