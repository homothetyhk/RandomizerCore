# About the Randomizer

## The forward fill algorithm

RandomizerCore uses forward fill, an algorithm which starts with no items
placed, and then iteratively identifies items to place to unlock new locations.
Each step of the forward fill proceeds by:

1. Add unplaced items cumulatively to progression until a new location is
   unlocked.
2. Then, traverse the items that were added backwards. Remove any item which is
   not necessary to maintain that at least one new location is unlocked.
3. Obtain a list of items which, when all placed, unlock at least one location.
   Place the items at locations that were previously reachable and update the
   lists of reachable locations, unplaced items, and unfilled locations.

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

## The randomizer as a whole

The randomizer is divided into stages, which are in turn divided into groups.

- Stages control the sequencing of randomization. When a stage is randomized,
  the forward fill algorithm runs on the items of all of its groups. In
  particular, a combination of items from different groups can be used to unlock
  new locations from any of the groups.
- Groups contain the lists of items and locations to be paired together. If a
  stage has multiple groups, the items from one group will not be paired with
  the locations of another group.

As an example, the Hollow Knight randomizer is divided as:

0. Main Item Stage [ Main Item Group ]

1. Main Transition Stage [ Left-to-right, Right-to-left, Top-to-bot, Bot-to-top,
   One-way ]

Here, the stages enforce that ordinary items and transitions are randomized at
different times. The groups enforce that ordinary items are randomized at
ordinary locations, while transitions of a certain direction are matched with
transitions of the opposite direction.

Stages are randomized in two passes, which occur in ascending and descending
order respectively. Each stage is randomized twice, except for the last stage
which is only randomized once. Randomizing a stage consists of running the
forward pass algorithm on its groups jointly until all of the locations of all
of the groups are reachable.

On the first pass, the items of stages not yet reached are considered accessible
without requirements. This is achieved using
@RandomizerCore.Randomization.IndeterminateLocation, which has special handling
for transitions. On the second pass, each stage is rerandomized subject to the
placements of the other stages.

For the example above, this results in

```text
Main Item Stage -> Main Transition Stage -> Main Item Stage
```

To interpret this, consider that it is not possible to randomize transitions
without some knowledge of where ordinary items are or could be, since those
items might be needed to reach some transitions. Thus, prior to randomizing
transitions, the randomizer create a temporary placement of items. Since there
is no transition placement available at this time, it treats all transitions as
reachable when generating the item placement (with special handling for state
logic to share state between transitions).

This results in a placement of items which can all be collected assuming all
transitions are reachable. Thus, transition randomization can access all items
from the first stage, provided it has enough reachable transitions. The
transition stage will create a placement of transitions which can all be reached
for a certain placement of items. The second pass item stage then creates a new
placement of items, using the result of the transition stage.

Generally speaking, since the second pass item stage ensures all of its items
are reachable, and the result of the transition stage was reachable with all
items, the final result is a placement for all stages where all of their
locations are reachable. However, with location-dependent effects (i.e. state
logic), it is possible to create scenarios where rerandomizing one stage renders
locations in another stage inaccessible. Thus, the final result must be
validated.

By default, validation checks that all locations are reachable. The
@RandomizerCore.Randomization.Validator is set at the group level, and callers
can choose to make validation more or less strict than default. Validation fails
by sending @RandomizerCore.Exceptions.ValidationException. Currently this halts
the randomizer without starting a new attempt.

## Monitoring the randomizer

The @RandomizerCore.RandoMonitor allows receiving events as the randomizer
progresses. Most importantly, it broadcasts each attempt of the randomizer, each
stage reached, and the error that caused the attempt to fail, if any.

An attempt refers to the sequence beginning with permutating of the groups and
ending with the conclusion of the second pass of stage randomization. An attempt
can be interrupted by any exception; unrecognized exceptions are not caught and
halt the randomizer. The following exceptions are caught, and cause the
randomizer to discard the current attempt and start a new attempt:

- @RandomizerCore.Exceptions.OutOfLocationsException
  - Triggered by a progression step produced by the forward pass algorithm which
    could not be placed with the available locations.
- @RandomizerCore.Exceptions.UnreachableLocationException
  - Triggered by a location which did not become reachable even after all items
    were given. Often this represents a logic error, but it can occur
    legitimately in some situations involving state logic.

Caught exceptions trigger error events for the @RandomizerCore.RandoMonitor.
Large attempt counts can be indicative of errors or overly restrictive
constraints on randomization.

## Customizing the randomizer

As described in the previous section, callers are required to specify the basic
stage-group structure of the randomizer. This section describes additional
options to control or weight its output.

### OnPermute and Priority

At the start of randomization, the item and location lists of each group are
permuted, which is to say put in random order. At the same time, each
item/location is assigned a _priority_ which reflects its position in the list.
Priorities are calculated as the `float` value `index / count`, and hence take
values in the interval `[0.0, 1.0)`. Priorities are stored in a property on
@RandomizerCore.IRandoItem or @RandomizerCore.IRandoLocation respectively.

Each group has a @RandomizerCore.Randomization.RandomizationGroup.OnPermute
event which runs after this operation, and allows subscribers access to the
random number generator to potentially further shuffle elements. Elements are
sorted by priority after this event runs, so subscribers should not manually
swap elements, just assign new priorities. The general rule is that lower
priority items and locations are placed earlier.

- This rule is enforced to some extent by the forward fill, which uses
  lexicographic order on the item list to select progression items.
- However, since the placement strategy allows the caller to customize how the
  final placement is chosen, this rule is not rigidly enforced.

Typical uses of OnPermute are to make a specific item much more likely to occur
early or late in progression. For example, by setting priority `p` to
`Math.Max(p, 0.8f)`, one can ensure that the item is always at least in the 80th
percentile of items, without rigidly forcing it to be last. As a note,
subscribers can set priorities to values outside `[0.0, 1.0)` if desired.

### Placement Strategy

Each stage and group has a placement strategy to allow customization of how
placements are made by callers. In general, it is recommended to use the
@RandomizerCore.Randomization.StagePlacementStrategy on the stage, and to modify
the @RandomizerCore.Randomization.GroupPlacementStrategy on the group as
desired. The @RandomizerCore.Randomization.DefaultGroupPlacementStrategy comes
with several customization options already, detailed below.

#### Constraints

Constraints allow the placement strategy to enforce requirements on placements
beyond those imposed by logic. For example, a constraint could be used to
require a certain item to only be placed at locations from a certain map area,
or to prevent an item from being placed at a location where the placement could
not be implemented. Constraints have a
@RandomizerCore.Randomization.DefaultGroupPlacementStrategy.Constraint.Test
property which contains the condition for the constraint to be satisfied, and a
@RandomizerCore.Randomization.DefaultGroupPlacementStrategy.Constraint.Fail
property which specifies behavior when the constraint cannot be satisfied. In
general, there are two ways to handle an unsatisfiable constraint:

- Soft constraints: `Fail` returns, optionally logging a message to the
  @RandomizerCore.RandoMonitor.
- Hard constraints: `Fail` throws
  @RandomizerCore.Exceptions.OutOfLocationsException with a message appropriate
  to the particular constraint.

The implementation is that for each item to be placed, the strategy searches
through the list of available locations for the first which satisfies all
constraints with the item. If no location satisfies all constraints, the first
location by priority is selected for the placement (regardless of how many
constraints the pair may or may not satisfy), and `Fail` is called sequentially
on the constraints that the pair do not satisfy.

#### Depth Priority Transform

The depth priority transform provides a way to weight item placements toward
locations unlocked later in randomization. This is done by transforming location
priorities via a function which depends on the number of forward fill steps
prior to the location becoming reachable, and possibly also on the current step
of the item being placed.

Many possible transforms can be generated with
@RandomizerCore.Randomization.PriorityTransformUtil. "Good" parameters depend on
many factors including personal preference, so it is recommended to experiment
with different settings. Parameters which are too large may lead to the
randomizer being very predictable, with the next item almost always being in the
location unlocked by the previous one. On the other hand, parameters which are
too small may lead to locations with restrictive logic rarely receiving
progression items.
