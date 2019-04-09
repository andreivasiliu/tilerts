## Tile RTS

It's supposed to be an RTS written in Rust, but I still don't know what I'm doing.

Right now it looks like this:

![Simple stuff](/screenshots/0-simplestuff.png?raw=true)

Current features:
* Made easily with [ggez](https://docs.rs/crate/ggez)!
* One sprite!
* Units, selecting units, right-clicking to set target
* Noise-based terrain with the [noise](https://docs.rs/crate/noise) crate
* Dijkstra-based pathfinding with the [pathfinding](https://docs.rs/crate/pathfinding) crate
* Buggy line-of-sight pathfinding override
