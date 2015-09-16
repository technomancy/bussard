# Bussard

<img src="http://p.hagelb.org/bussard.png" alt="screenshot" />

A space-flight adventure exploration game, with a programmable space ship and space stations.

Read the [in-game manual](manual.md) for a taste of how the game works.

## Status

Currently the game supports flying around, mining asteroids, trading
cargo, and running some code in the in-game REPL. You can also edit
your ship's config file in order to reprogram it, change key bindings,
and add convenience functions.

The interaction with the planet and station computers is very limited
at this time. (Basically just running vanilla Unix commands and cargo
transactions.) No ship hardware/software upgrades have been
implemented.

See the list of [open issues](https://gitlab.com/technomancy/bussard/issues).

<img src="http://p.hagelb.org/bussard-edit.png" alt="edit screenshot" />

## Usage

Use `make` to launch.

Requires [LÖVE](http://love2d.org) 0.9.x; tested on 0.9.1.

Recommended soundtrack:
[Contingency](http://music.biggiantcircles.com/album/contingency) by
[Big Giant Circles](http://www.biggiantcircles.com/) though the
[FTL soundtrack](https://benprunty.bandcamp.com/album/ftl) is a great
fit too.

<img src="http://p.hagelb.org/bussard-repl.png" alt="repl screenshot" />

## License

Code copyright © 2015 Phil Hagelberg and contributors

Distributed under the GNU General Public License version 3; see file COPYING.

[Planet graphics](http://opengameart.org/content/planets-and-stars-set-high-res) © Rawdanitsu, CC0 licensed.

Station graphics by [MillionthVector](http://millionthvector.blogspot.de/p/free-sprites_12.html), CC-BY licensed.
