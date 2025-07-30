---
uid: forward_fill
---

# The Forward Fill Algorithm

## Overview

The @RandomizerCore.Randomization.Randomizer uses forward fill, an algorithm
which starts with no items placed, and then iteratively identifies items to
place to unlock new locations. The implementation of the forward fill can be
found in @RandomizerCore.Randomization.SphereBuilder.

Each step of the forward fill proceeds by:

1. Add unplaced items cumulatively to progression until a new location is
   unlocked.
2. Then, traverse the items that were added backwards. Remove any item which is
   not necessary to maintain that at least one new location is unlocked.
3. Obtain a list of items which, when all placed, unlock at least one location.
   Place the items at unfilled reachable locations (not including the newly
   unlocked locations).

Once all locations are unlocked, items not used for progression can be placed
arbitrarily.

### Notes on Step 1

- We assume the order in which items are added does not matter. This is a
  fundamental assumption of the randomizer.
- This step always finds new progression, except in the case that there is a
  location which cannot be unlocked by adding all items. Typically, this
  indicates erroneous logic, but it can occur legitimately when randomizing
  items with location-dependent effects on state logic (such as transitions).

### Notes on Step 2

- Testing each item is done by reverting to the progression state at the start
  of the step, and then adding the current list of candidate items excluding the
  item currently being tested.
- The list of items produced by this step is minimal in the _lexicographic
  order_ on the list of unplaced items, among all sublists that unlock new
  locations.

### Notes on Step 3

- Item placement is handled by the stage/group placement strategy. The
  randomizer passes to the strategy the items that must be placed, and the legal
  locations where they can be placed. The placement strategy can be used to
  apply further constraints or weights in selecting final item-location pairs.
- Step 3 can fail if the number of items to be placed exceeds the number of
  currently reachable locations. This is especially likely to occur when there
  are some locations that require a large number of items to unlock, but no
  locations that require an intermediate subset of these items.
  - For example, suppose the item list includes 50 "coin" items, and the
    location list includes a location that requires 25 "coins", but there is no
    logic requiring a threshold below 25 "coins". Then the first step that
    places "coins" will necessarily place 25, and will error if there are not 25
    locations available at that time.
- The error thrown when this step fails is
  @RandomizerCore.Exceptions.OutOfLocationsException. The randomizer discards
  the current attempt and starts a new attempt when this error is thrown.
