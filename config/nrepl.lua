local editor = require("polywell")
local lume = require("polywell.lume")
local socket = require("socket")
local bencode = require("polywell.config.bencode")
require("polywell.config.console")

responses = {}

editor.define_mode("nrepl", "console")

local active_requests, request_counter = {}, 0

local send = function(conn, msg, callback)
   msg.id, request_counter = request_counter, request_counter + 1
   active_requests[msg.id] = true
   local ok, err = conn:send(bencode.encode(msg))
   if(not ok) then editor.echo("Error: " .. err) end
end

local handle_input = function(session)
   local conn = assert(editor.get_prop("conn"), "Not connected.")
   editor.read_line("Input: ", function(input, cancel)
                       if(cancel or input == nil) then input = "" end
                       send(conn, {op="stdin", session=session, stdin=input})
   end)
end

local p = function(...)
   editor.with_output_to(editor.current_buffer_path(),
                         lume.fn(editor.write, "\n", ...))
end

local handler = function(response)
   table.insert(responses, response)
   if(response.err) then p(response.err) end
   if(response.out) then p(response.out) end
   if(response.value) then p(response.value) end

   if(response.ns) then
      editor.set_prop(response.ns)
      editor.set_prompt(response.ns .. "=> ")
   end
   if(response.status) then
      if(lume.find(response.status, "interrupted")) then
         editor.echo("Evaluation interrupted.")
         active_requests[response.id] = nil
      end
      if(lume.find(response.status, "done")) then
         active_requests[response.id] = nil
      end
      if(lume.find(response.status, "need-input")) then
         handle_input(response.session)
      end
   end
end

local receive = function(conn, buffer)
   while true do
      local data, err, partial = conn:receive("*a")
      if(data or partial and partial ~= "") then
         local decodeds, d_err = bencode.decode_all(data or partial)
         if(decodeds) then
            for _,decoded in ipairs(decodeds) do
               editor.with_current_buffer(buffer, handler, decoded)
            end
         else
            p("Decoding error: " .. d_err, data or partial, "\n")
         end
         coroutine.yield(true)
      elseif(err and err ~= "timeout") then
         p(err)
         coroutine.yield(true)
      else
         coroutine.yield(false)
      end
   end
end

local send_eval_input = function(input)
   local conn = assert(editor.get_prop("conn"), "Not connected.")
   local session = editor.get_prop("nrepl-session")
   send(conn, {op="eval", code=input, session=session})
end

editor.bind("nrepl", "return", function()
               if(editor.get_line_number() ~= editor.get_max_line()) then
                  editor.end_of_buffer()
               end

               local input = editor.get_input()
               editor.history_push(input)
               editor.end_of_line()
               editor.newline()
               editor.no_mark()
               editor.print_prompt()
               editor.debug()
               send_eval_input(input)
end)

editor.bind("nrepl", "ctrl-x ctrl-i", function()
               local conn = assert(editor.get_prop("conn"), "Not connected.")
               local session = editor.get_prop("nrepl-session")
               for _,id in ipairs(active_requests) do
                  send(conn, {op="interrupt", ["interrupt-id"]=id,
                              session=session})
               end
end)

editor.bind("nrepl", "ctrl-x ctrl-d", function()
               local conn = assert(editor.get_prop("conn"), "Not connected.")
               local session = editor.get_prop("nrepl-session")
               local code = "(require 'clojure.repl) (clojure.repl/doc %s)"
               editor.read_line("Describe: ", function(var, cancel)
                                   if(cancel or var=="") then return end
                                   send(conn, {op="eval", session=session,
                                               code=code:format(var)})
               end)
end)

local new_session = function(response)
   if(response["new-session"]) then
      p("Connected.\n")
      editor.set_prop("nrepl-session", response["new-session"])
   end
end

local connect = function(port, cancel)
   if(cancel) then return end
   local conn, err = socket.connect("localhost", tonumber(port))
   if(conn) then
      local buffer = "*nrepl " .. port .. "*"
      conn:settimeout(0)
      send(conn, {op="clone"}, new_session)
      editor.open(nil, buffer, "nrepl")
      editor.set_prop("conn", conn)
      editor.set_prompt("user=> ")
      editor.print_prompt()
      local r = lume.fn(receive, conn, buffer)
      table.insert(editor.coroutines, coroutine.create(r))
   else
      editor.echo(err)
   end
end

return {
   connect = connect,
   go = function()
      editor.read_line("Connect to nREPL server: ", connect)
   end,
}
