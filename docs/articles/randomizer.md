# About the Randomizer

This article introduces commonly used elements of the
@RandomizerCore.Randomization.Randomizer API. The randomizer is a forward fill
algorithm which places items iteratively in progression steps, each of which
contains one or more items needed to unlock new locations. For more advanced
discussion of the algorithm used by the randomizer, see <xref:forward_fill>.
This article does not require detailed knowledge of the algorithm, but in some
places it may mention things that depend on the steps of the forward fill.

## Stages and Groups

Randomization is divided into stages, which are in turn divided into groups.

- Stages control the sequencing of randomization. When a stage is randomized,
  the forward fill algorithm runs on the items of all of its groups. In
  particular, a combination of items from different groups can be used to unlock
  new locations from any of the groups.
- Groups contain the lists of items and locations to be paired together. If a
  stage has multiple groups, the items from one group will not be paired with
  the locations of another group.

As an example, a combined item-transition randomizer might be divided as:

0. Main Item Stage [ Main Item Group ]

1. Main Transition Stage [ Left-to-right, Right-to-left, Top-to-bot, Bot-to-top,
   One-way ]

Here, the stages enforce that ordinary items and transitions are randomized at
different times. The groups enforce that ordinary items are randomized at
ordinary locations, while transitions of a certain direction are matched with
transitions of the opposite direction.

Stages are randomized in two passes, which occur in ascending and descending
order respectively. Each stage is randomized twice, except for the last stage
which is only randomized once. The randomization order for the above example is

```text
Main Item Stage -> Main Transition Stage -> Main Item Stage
```

On the first pass, the items of stages not yet reached are considered accessible
without requirements (with special handling when state logic is involved). The
goal of the first pass, at a heuristic level, is to create "temporary" or "fake"
placements which can all be obtained given the right placements of the
subsequent stages. On the second pass, each stage is rerandomized subject to the
placements of the other stages to produce a final result.

The two-pass structure allows, in the running example, randomizing transitions
subject to a random placement of ordinary items. Since the first stage only
assumed that all transitions were reachable, it still used logic to place the
items, and so no item was placed at a location locked by itself. Barring issues
from running out of locations, this guarantees that the second stage can place
transitions to access any necessary progression items, and that there exists a
placement of items which works for the final stage (namely, the result of the
first stage).

### Validation

In the absence of state logic and location-dependent effects, the randomizer
always produces a placement in which all locations of all stages are reachable.
This is because each stage's rerandomization results in all of its items being
accessible; if logic only depends on the items the player has, then this
guarantee carries through to the end of randomization.

On the other hand, with location-dependent effects, it is possible for moving an
item to a new location to cause a location in another stage to become
unreachable. Thus, the final result must be validated.

By default, validation checks that all locations are reachable. The
@RandomizerCore.Randomization.Validator is set at the group level, and callers
can choose to make validation more or less strict than default. Validation fails
by sending @RandomizerCore.Exceptions.ValidationException. Currently this halts
the randomizer without starting a new attempt.

## Monitoring the Randomizer

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

Caught exceptions trigger error events for the RandoMonitor. Large attempt
counts can be indicative of errors or overly restrictive constraints on
randomization.

## Customizing the Randomizer

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

> [!TIP] The randomizer's forward fill uses lexicographic order on the item list
> to select progression items, so there is a direct correspondence between
> priority and intended progression. However, since the placement strategy
> allows the caller to customize how the final placement is chosen, the use of
> location priority order is not rigidly enforced.

Typical uses of OnPermute are to make a specific item much more likely to occur
early or late in progression. For example, by setting priority `p` to
`Math.Max(p, 0.8f)`, one can ensure that the item is always at least in the 80th
percentile of items, without rigidly forcing it to be last. As a note,
subscribers can set priorities to values outside `[0.0, 1.0)` if desired.

### Placement Strategy

Each stage and group has a placement strategy to allow customization of how
placements are made by callers. In general, it is recommended to use the default
@RandomizerCore.Randomization.StagePlacementStrategy on each stage and customize
by modifying the @RandomizerCore.Randomization.GroupPlacementStrategy on each
group as desired. To note, StatePlacementStrategy is non-abstract with a
standard implementation, while GroupPlacementStrategy is abstract.
@RandomizerCore.Randomization.DefaultGroupPlacementStrategy is an implementation
of GroupPlacementStrategy which comes with several customization options,
detailed in the next few subsections.

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

> [!CAUTION] Since hard constraints restart the randomizer, they may
> dramatically increase the number of randomization attempts if
> constraint-satisfying pairs are unlikely to occur. Randomization groups are
> strongly prefered over constraints when possible.

#### Depth Priority Transform

The depth priority transform provides a way to weight item placements toward
locations unlocked later in randomization.
@RandomizerCore.Randomization.PriorityTransformUtil has many options for
creating transforms. For example, `PriorityTransformUtil.CreateTransform(3.0f)`
returns a transform with the effect:

```text
for a location placed in the nth step of forward fill
  reduce the priority of the location by (3.0/100) * n
unless the item being placed "ought to be placed later than the location"
  in which case increase the priority of the location by 1.0
```

After all transforms run, locations are sorted by priority and the first by
priority satisfying all constraints is selected for placement. Recall from above
that priorities by default are uniformly spaced in `[0.0, 1.0)`. Thus, the
example above means that the transform advances locations by 3 percentiles in
priority order, per step. This is initially a minor effect, but makes locations
unlocked in step 20 or 40 much more likely to be selected.

The implementation of "ought to be placed later than the location" is involved,
and is explained in the next section. Roughly speaking, it says that items with
large priority (late in priority order) should not receive a boost toward being
placed at locations unlocked early in progression order.

There are many settings for PriorityTransformUtil, so the task of choosing a
transform may seem overwhelming. It is recommended to experiment with different
settings, and choose according to personal preference.

> [!TIP] Large coefficients, or transform type `Quadratic`, strongly encourage
> placing items in newly unlocked locations, potentially at the cost of making
> the randomizer more predictable. Smaller coefficients, or transform type
> `SquareRoot`, may make the effect of the transform fairly weak. Try starting
> with `Linear`, coefficient 3.0, and tweaking parameters from there!

##### Implementation of the Depth Priority Transform

A priority transform is a function which takes the following parameters:

- `item` - The item being placed
- `location` - The candidate location for placement
- `itemDepth` - The index of the forward fill step in which the item is being
  placed; i.e. the first set of progression is depth 0, the next 1, and so on.
- `itemPriorityDepth` - For each previous forward fill step, compute the average
  of the priorities of the items placed in the step. Then, count how many of
  these numbers are less than the current item's priority. This can be seen as
  an adjusted version of `itemDepth` which is much smaller for items with low
  priority placed late in forward fill.
- `locationDepth` - The index of the forward fill step in which the location
  became reachable.
- `locationPriority` - ref parameter initially set to the original priority of
  the location. Can be set by the function to a new value.

PriorityTransformUtil transforms are composed of:

- An initial block depending on the
  @RandomizerCore.Randomization.PriorityTransformUtil.ItemPriorityDepthEffect
  setting. In general, this block only runs if
  `itemPriorityDepth < locationDepth`, in which case it acts to reduce the
  effect of the transform according to the setting.
- A second block depending in the
  @RandomizerCore.Randomization.PriorityTransformUtil.TransformType setting, and
  the `coefficient` of the transform. This decrements the location priority by
  some mathematical function of the `locationDepth`. The `locationDepth` passed
  to the second block may be reduced if the first block runs.

In general, the idea is that a larger location depth results in a larger
adjustment to priority. Locations are ordered by priority after all transforms
run, and the first location by priority is selected for placement. To balance
somewhat with the original priorities, the ItemPriorityDepthEffect allows
reducing or eliminating the adjustment when the location was unlocked in an
earlier step than the item priority implies the item should be placed in.
