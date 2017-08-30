local fs = require("love.filesystem")

return {
   rects = {
      {0,0,7,1, color="green"},
      {0,0,1,6, color="green"},
      {6,0,1,6, color="green"},
      {0,5,7,1, color="green"},
      {2,4,1,1, color="green"},
      {3,2,4,1, color="green"},
      {5,1,1,1, color="blue"},
   },

   rover = {4.5,4.5,0.9,0.9},
   motd = assert(fs.read("data/motd/merdeka-trainee1")),
   hosts = {{5,2,1,1, name="trainee01", os="orb"}},

   messages = {
      {5,4,1,1, msg="Good; you moved forward. Now turn with `left` and go forward that way."},
      {4,4,1,1, msg="You're getting the idea. A little further, then head around the corner."},
      {6,5,1,1, msg="All right, so you turned first; that's fine. " ..
          "Use `forward` and more\nturns to reach the terminal."},
      {4,5,1,1, msg="All right, so you turned first; that's fine. " ..
          "Use `forward` and more\nturns to reach the terminal."},
      {3,3,0.5,0.5, msg="There are shorter aliases for `forward`, `left`, and `right`: `f`, `l`, and `r`"},
      {4.5,2,1,1, msg="You have reached the terminal. Run `login` to connect."},
   },
}
