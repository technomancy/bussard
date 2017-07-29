return {
   rects = {
      {0,-10,120,10},
      {42,207,70,15},
   },
   rover = {5,5,9,9},
   motd = "This isn't finished yet, sorry!",
   triggers = {
      [{40,205,72,17}] = function() record_event("memory_card_delivered") end,
   },
}
