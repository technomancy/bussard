local turn_first = "All right, so you turned first; that's fine. Use `forward()` and more\nturns to reach the terminal."
local messages = {
   ["5x4"]="Good; you successfully moved forward. Now turn with `left()` and go that way.",
   ["4x4"]="You're getting the idea. A little further, then head around the corner.",
   ["6x5"]=turn_first,
   ["4x5"]=turn_first,
   ["3x3"]="There are shorter aliases for `forward()`, `left()`, and `right()`:\n`f()`, `l()`, and `r()`",
   ["4x2"]="You have reached the terminal. Run `login()` to connect.",
}

return {
   "ooooooo",
   "o   t o",
   "o  oooo",
   "o     o",
   "o o r o",
   "ooooooo",

   motd=assert(love.filesystem.read("data/motd/merdeka-trainee1")),

   hosts={["5x2"]={name="trainee01", os="orb"}},
   messages=messages,
}
