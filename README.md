## Tile RTS

It's supposed to be an RTS written in Rust, but I still don't know what I'm doing.

Right now it looks like this:

![Mining](/screenshots/1-mining.png?raw=true)

Current features:
* Made easily with [ggez](https://docs.rs/crate/ggez)!
* Two sprites!
* Units, selecting units, right-clicking to set target
* Right-clicking rocks to turn them into objects
* Noise-based terrain with the [noise](https://docs.rs/crate/noise) crate
* Dijkstra-based pathfinding with the [pathfinding](https://docs.rs/crate/pathfinding) crate
* Buggy line-of-sight pathfinding override
