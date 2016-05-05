# Lonesome Planet Map

The Lonesome Planet Map contains a survey of all the known colonized
systems in human space. Portal connections are clearly marked,
distances are to scale, and color-coding is used to show the
affiliations of each system at the time of publication:

* White: Solar Union
* Green: Terran Republic
* Yellow: Kingdom of Bohk
* Red: Yueh
* Purple: Republic of Katilay
* Turquoise: Tana Protectorates

To set up the map, load this code:

    ship.map = { x=0, y=0 }

    map_pan = function(x, y)
       ship.map.x, ship.map.y = ship.map.x + x, ship.map.y + y
    end

    define_mode("map")
    bind("map", "escape", ship.editor.close)
    bind("map", "down", lume.fn(map_pan, 0, -0.1))
    bind("map", "up", lume.fn(map_pan, 0, 0.1))
    bind("map", "left", lume.fn(map_pan, -0.1, 0))
    bind("map", "right", lume.fn(map_pan, 0.1, 0))
    ship.modes.map.draw = ship.actions.map

    map = function() ship.editor.open(nil, "*map*") ship:activate_mode("map") end
    bind("flight", "alt-m", map)

It's recommended to place this in the "src.map" file and add this to
"src.config" at the bottom:

    dofile("src.map")

Then running map() from the console or pressing alt-m from flight mode
will activate the map. You can pan around the map with the arrows and
close it with escape, but of course you can change the keys in the code.

Happy Travels!

Copyright © 2430 Lonesome Planet Publishing, All Rights Reserved.
