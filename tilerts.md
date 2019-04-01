Things I need to solve:
* Coordinate system
* Collision detection, if any
* Collision shapes
* Physics, if any
* Nudging, if any
* Path-finding
* Map structure

### Coordinate system

* Most likely 2D tiles, hex would be prettier but be a real pain to manage.
* Units must be able to occupy multiple tiles.
  * Although is that really necessary?
  * Not really, but it would look too ugly.
  * What if unit tiles were much bigger than map tiles though?
  * That is actually acceptable. Hmm.

### Map structure

* Static tiles. Possibly with multiple vertical layers, but that's optional.
* A tile on the map has the same size as a tile on a unit, although they may not be part of the same coordinate system.

### Path-finding

* What's the MVP?
