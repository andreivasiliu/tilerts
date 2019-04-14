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
  * So far I made reverse Dijsktra and line-of-sight pathing, but I don't know how to do unit-unit collision.
  * Found some really good stuff on [GameAIPro](http://www.gameaipro.com/GameAIPro/GameAIPro_Chapter23_Crowd_Pathfinding_and_Steering_Using_Flow_Field_Tiles.pdf).

### Structures

Story:
* worker mines rock, which becomes ore on the ground
* worker processes the ore tile unit into a factory tile unit
* player draws a multi-tile factory on the ground (how? dunno)
* worker processes ground tiles into specific shapes (which? dunno)
* worker tows the factory tile into a factory slot (how? dunno)
* if the factory slot has a factory tile, and is surrounded by the correctly terraformed terrain, it acts as a Factory structure

Factory:
```
f^.  Legend:
Fof   f - factory shape
f^.   F - factory shape (requires factory unit slotted into it)
      ^ - conveyor maybe?
      . - ground
```

Details:
* Gathering tiles
  * Worker unit (done!)
  * Tile unit
  * Right-click map tile selector (done!)
    * Hovering selector (done!)
    * Requires knowing types of current selected units (done!)
    * Mine (maybe a pickaxe or drill) icon
  * Pathfinding (done!)
  * Proximity check (done!)
  * Per-tile mining progress (done!)
  * Gathering GFX (done!)
  * Map tile deletion (done!)
  * Unit tile creation (done!)
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