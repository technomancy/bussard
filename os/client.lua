local lume = require("lume")
local rpcs = require("rpcs")
local utils = require("utils")

local sessions = {}

local dbg = os.getenv("DEBUG") and print or function() end

-- TODO: try to unify send/receive a bit
local transmit_success = function(distance, range)
   -- TODO: re-enable once we can fix attempts to resend
   -- currently resend attempt only happens when we try to send another msg
   -- this is checked several times per second
   local chance = love.math.random()
   local signal_strength = range * range * chance / (distance * distance)
   return true or signal_strength > 1
end

local queued = {}

local disconnect = function(ship)
   ship.api:activate_mode("console")
   ship.api.editor.set_prompt("> ")
   ship.api.editor.print("Logged out.")
end

local function send(channel, session_id, get_distance, range, data)
   if(type(data) == "table" and data.op) then
      data.session_id = session_id
      queued[channel] = queued[channel] or {}
      local queue = queued[channel]
      if(transmit_success(get_distance(), range)) then
         lume.map(queue, lume.fn(channel.push, channel))
         lume.clear(queue)
         channel:push(data)
      else
         table.insert(queue, 1, data)
      end
   elseif(type(data) == "string") then
      send(channel, session_id, get_distance, range,
           {op="stdin", stdin=data, session_id=session_id})
   else
      error("Unsupported message type: " .. tostring(data))
   end
end

local function recv(ship, port, channel, blocking)
   local msg = channel:pop()
   if(msg) then
      dbg("<", require("serpent").block(msg))
      if(msg.out) then ship.api.write(msg.out) end
      if(msg.op == "disconnect") then
         ship.api.editor.with_current_buffer("*console*", disconnect, ship)
      elseif(msg.op == "rpc") then
         local resp = {rpcs[msg.fn](ship, port, unpack(msg.args or {}))}
         dbg("r>", require("serpent").block(resp))
         if(msg.chan) then msg.chan:push(resp) end
      end
      return msg
   elseif(blocking) then
      love.timer.sleep(0.1)
      return recv(ship, port, channel, blocking)
   end
end

local try_sending_queued_msgs = function(session, distance, range)
   local queue = queued[session.o]
   if(not queue) then return end
   for _, msg in ipairs(queue) do
      if(transmit_success(distance, range)) then
         session.o:push(msg)
      else
         return -- don't let messages be delivered out of order
      end
   end
end

return {
   connect = function(ship, username, password)
      if(not ship.target) then return end
      if(utils.distance(ship, ship.target) > ship.comm_range) then
         ship.api.editor.print("Out of range.")
         return false
      end
      ship.api.closest_cycle = 1
      local i, o = ship.target.input, ship.target.output
      if(not i and not o) then
         ship.api.print("Connection refused: " .. ship.target.name)
         return false
      end
      o:push({op="login", username=username, password=password})
      local response = i:pop()
      while not response do response = i:pop() love.timer.sleep (0.01) end
      dbg("<<", require("serpent").block(response))
      ship.api.print(response.out)
      if(response.ok) then
         sessions[response.session_id] = {input=i, output=o,
                                          port=ship.target}
         return lume.fn(send, o, response.session_id,
                        lume.fn(utils.distance, ship, ship.target),
                        ship.comm_range), lume.fn(recv, ship, ship.target, i)
      else
         return false
      end
   end,

   update = function(ship, all)
      for _, session in pairs(sessions) do
         local distance = utils.distance(ship, session.port)
         try_sending_queued_msgs(session, distance, ship.comm_range)
         if(all) then
            while(session.input:peek() and
               transmit_success(distance, ship.comm_range)) do
               recv(ship, session.port, session.input)
            end
         else
            if(session.input:peek() and
               transmit_success(distance, ship.comm_range)) then
               recv(ship, session.port, session.input)
            end
         end
      end
   end,

   is_connected = function(_, target)
      for _,s in pairs(sessions) do
         if(target == s.port.name) then return true end
      end
   end,
}
