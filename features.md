## Main idea

The map is made of tiles. Ground tiles and structure tiles are on a grid. Units are on a continuous space.

Workers can mine tiles out of terrain and bring them into unit space, where they can be moved, processed, transformed. Factories combine multiple tiles into units. Destroyed units leave wrecked tiles behind. Tiles can never be deleted, only wrecked.

Tiles can be arranged into various unit types of various sizes. Workers move tiles between structures, and structures process/assemble them.

That's it!

## Long-term features

They won't all be part of the MVP, but it's good to keep them in mind so I don't lock out support for them.

* Units carrying tiles
  * Resource gatherers, scavengers
  * Maybe all units can tow one tile behind them
* Flying units
  * They will be able to land and take off (not in MVP)
* Container units (not in MVP)
  * E.g. land carrier, flying carrier
* Maybe hybrid units? (not in MVP)
  * E.g. the engines of a large ship can split off and become units
* Power (not in MVP)
  * Infinite supply, limited throughput, limited range
  * Posts, cables, imaginary links, etc
* Conveyors? (not in MVP)
  * They use an infinite resource
  * Automates gathering
* Blueprints (not in MVP)
  * Of destroyed buildings
* Unit zones (not in MVP)
  * Guard areas, gathering zones
  * Idle goes to gathering, gathering goes to guard
* Terraform (not in MVP)
  * Resources are scarce, so just transform ground to make walls/conveyors
  * Maybe also require preparing ground before building
  * Cannot raise/lower elevations, but can make bridges
* Stacked elevations, quarries (not in MVP)
  * Only elevation 0 is ever walkable
  * High elevations serve as tiles that can be mined multiple times
  * Can be mined into negative elevations too
  * Quarry can strip mine an entire region all the way down
