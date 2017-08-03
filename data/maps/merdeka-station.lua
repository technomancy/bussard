local fs = require("love.filesystem")

return {
   rects = {
      { 0, 0,70,10, color="green"},
      { 0, 0,10,60, color="green"},
      {60, 0,10,60, color="green"},
      {0, 50,70,10, color="green"},
      {20,40,10,10, color="green"},
      {30,20,40,10, color="green"},
      {50,10,10,10, color="blue"},
   },

   rover = {45,45,9,9},
   motd = assert(fs.read("data/motd/merdeka-trainee1")),
   hosts = {{50,20,10,10, name="trainee01", os="orb"}},

   messages = {
      {50,40,10,10, msg="Good; you moved forward. Now turn with `left()` and go that way."},
      {40,40,10,10, msg="You're getting the idea. A little further, then head around the corner."},
      {60,50,10,10, msg="All right, so you turned first; that's fine. " ..
          "Use `forward()` and more\nturns to reach the terminal."},
      {40,50,10,10, msg="All right, so you turned first; that's fine. " ..
          "Use `forward()` and more\nturns to reach the terminal."},
      {30,30, 5, 5, msg="There are shorter aliases for `forward()`, `left()`, and `right()`:\n`f()`, `l()`, and `r()`"},
      {45,20, 10, 10, msg="You have reached the terminal. Run `login()` to connect."},
   },
}
