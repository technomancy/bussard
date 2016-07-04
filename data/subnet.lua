 -- -*- lua -*-

local _f, _env, args = ...

if(lume.find(args, "--help")) then
   print("Establish raw subnet connection.")
else
   while true do
      set_prompt("@ ")
      local input = io.read()
      if(not input or input == "exit" or input == "logout") then
         logout()
      else
         local response, err = subnet.request(input)
         if(response) then
            print(response)
         else
            print("Error: ".. err)
         end
      end
   end
end
