return {
   rects = {
      {0,-10,120,10},
      {20, 20, 20, 20}, {80, 20, 20, 20},
      {20, 50, 30, 20}, {80, 80, 20, 20},
      {0, 80, 20, 20},
      {40,100,10,10, color="green"},
   },
   rover = {5,5,9,9},
   motd = "TODO: fill in more buildings",
   triggers = {
      [{37,97,18,18}] = function() record_event("memory_card_delivered") end,
   },
}
