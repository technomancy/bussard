local lume = require("lume")
local rpcs = require("rpcs")
local utils = require("utils")

local sessions = {}

local dbg = os.getenv("DEBUG") and print or function() end

-- TODO: try to unify send/receive a bit
-- TODO: limited retry, backoff?
local transmit_success = function(distance, range)
   -- this is checked several times per second
   local chance = love.math.random()
   local signal_strength = range * range * chance / (distance * distance)
   return signal_strength > 1
end

local queued = {}

local function send(channel, session, get_distance, range, data)
   if(type(data) == "table" and data.op) then
      data.session = session
      queued[channel] = queued[channel] or {}
      local queue = queued[channel]
      if(transmit_success(get_distance(), range)) then
         lume.map(queue, lume.fn(channel.push, channel))
         lume.clear(queue)
         channel:push(data)
      else
         table.insert(queue, 1, data)
      end
   elseif(data == nil) then
      channel:push({op="kill", session=session})
   elseif(type(data) == "string") then
      send(channel, session, get_distance, range,
           {op="stdin", stdin=data, session=session})
   elseif(type(data) == "table" and data.__get_response) then
      session.input:demand() -- for debugging/tests
   else
      error("Unsupported message type: " .. tostring(data))
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

local disconnect = function(ship)
   ship.api:activate_mode("console")
   ship.api.editor.set_prompt("> ")
end

return {
   connect = function(ship, username, password)
      if(not ship.target) then return end
      if(utils.distance(ship, ship.target) > ship.comm_range) then
         return ship.api.editor.print("Out of range.")
      end
      ship.api.closest_cycle = 1
      local i, o = ship.target.input, ship.target.output
      if(not i and not o) then
         ship.api.print("Connection refused: " .. ship.target.name)
         return
      end
      o:push({op="login", username=username, password=password})
      local response = i:demand()
      dbg("<", require("serpent").block(response))
      ship.api.print(response.out)
      if(response.ok) then
         sessions[response["new-session"]] = {input=i, output=o,
                                              port=ship.target}
         return lume.fn(send, o, response["new-session"] or "session",
                        lume.fn(utils.distance, ship, ship.target),
                        ship.comm_range)
      else
         return false
      end
   end,

   update = function(ship, _dt)
      for _, session in pairs(sessions) do
         local distance = utils.distance(ship, session.port)
         try_sending_queued_msgs(session, distance, ship.comm_range)
         if(session.input:peek() and transmit_success(distance,
                                                      ship.comm_range)) then
            local msg = session.input:pop()
            if(msg) then
               dbg("<", require("serpent").block(msg))
               if(msg.out) then ship.api.write(msg.out) end
               if(msg.op == "disconnect") then
                  ship.api.editor.with_current_buffer("*console*",
                                                      disconnect, ship)
               elseif(msg.op == "rpc") then
                  local resp = {rpcs[msg.fn](ship, session.port,
                                             unpack(msg.args or {}))}
                  dbg(">", require("serpent").block(resp))
                  msg.chan:push(resp)
               end
            end
         end
      end
   end,
}
