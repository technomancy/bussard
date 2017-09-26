local fs = require("love.filesystem")

return {
   rects = {
      {0,0,7,1, color="green"},
      {0,1,1,4, color="green"},
      {6,1,1,2, color="green"},
      {6,4,1,1, color="green"},
      {0,5,7,1, color="green"},
      {2,4,1,1, color="green"},
      {3,2,3,1, color="green"},
      {5,1,1,1, type="term"},
      {3,1,1,1, vertical=false, open=true,  type="door", color="blue", door=1},
      {6,3,1,1, vertical=false, open=false, type="door", color="blue", door=2},
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
