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
  * Still have no clue.

### Structures

* Gathering tiles
  * Worker unit (done!)
  * Tile unit
  * Right-click map tile selector
  * Pathfinding
  * Proximity check
  * Per-tile mining progress
  * Gathering GFX
  * Map tile deletion, unit tile creation
* Processing
  * Turn a tile into another kind of tile
  * Factory tile sprite
* Tile towing
  * Right-click unit tile selector
  * Unit -> unit association
  * Some sort of physics?
* Terraforming
  * Mark intent on ground tile
  * Factory slot tile sprite
  * Intent GFX
* Tile snapping
  * Non-intentional rotation, movement
  * Three-way (unit, tile, slot) decision
* Factory
  * ???