-- -*- lua -*-

local _env, args = ...

local commands = {"toggle", "open", "close"}

if((not args[1] or not args[1]:match("[0-9]+")) or
      args[2] and not lume.find(commands, args[2]) or (#args > 2)) then
   print("Open and close doors by door number.\n")
   print("Usage:")
   print("  door 1 open")
   print("  door 4 (action defaults to toggling)")
else
   door(args[1], args[2])
end
