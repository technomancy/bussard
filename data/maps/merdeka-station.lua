local turn_first = "All right, so you turned first; that's fine. Use `forward()` and more\nturns to reach the terminal."
local messages = {
   ["5x4"]="Good; you successfully moved forward. Now turn by calling `left()'.",
   ["4x4"]="You're getting the idea. A little further, then head around the corner.'",
   ["6x5"]=turn_first,
   ["4x5"]=turn_first,
   ["4x2"]="You have reached the terminal. Run `login()' to connect.",
}

return {
   "ooooooo",
   "o   t o",
   "o  oooo",
   "o     o",
   "o o s o",
   "ooooooo",

   motd="Welcome to stage 1 of the PTMC bot operations training course.\n\n"..
      "You are currently logged in to an indoor training rover. There is a\n"..
   "a terminal at the other end of this room. Pilot the rover to the spot\n"..
      "next to the terminal using `forward()', `left()', and `right()'.",

   hosts={["5x2"]={name="trainee01", os="orb"}},
   messages=messages,
}
