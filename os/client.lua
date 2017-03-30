local lume = require("lume")
local rpcs = require("rpcs")

local sessions = {}

local send = function(channel, session, data)
   if(type(data) == "string") then
      channel:push({op="stdin", stdin=data, session=session})
   else
      error("Unsupported message type: " .. tostring(data))
   end
end

return {
   connect = function(ship, username, password)
      -- TODO: range check
      if(not ship.target) then return end
      local i, o = ship.target.input, ship.target.output
      if(not i and not o) then
         ship.api.print("Connection refused: " .. ship.target.name)
         return
      end
      o:push({op="login", username=username, password=password})
      local response = i:demand()
      print("<", require("serpent").block(response))
      ship.api.print(response.out)
      if(response.ok) then
         sessions[response["new-session"]] = {input=i, output=o,
                                              port=ship.target}
         return lume.fn(send, o, response["new-session"] or "session")
      else
         return false
      end
   end,

   update = function(ship, _dt)
      for _, session in pairs(sessions) do
         local msg = session.input:pop()
         if(msg) then
            print("<", require("serpent").block(msg))
            if(msg.out) then ship.api.write(msg.out) end
            if(msg.op == "rpc") then
               msg.chan:push({rpcs[msg.fn](ship, session.port,
                                           unpack(msg.args or {}))})
            end
         end
      end
   end,

   logout = function(_ship) end, -- TODO
}
