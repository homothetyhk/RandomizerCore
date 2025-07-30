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
- This step always finds new progression, except if there is a location which
  cannot be unlocked by adding all items. In this case,
  @RandomizerCore.Exceptions.UnreachableLocationException is thrown, causing the
  randomizer to discard the current attempt and start a new attempt.
  - Typically, this indicates erroneous logic, but it can occur legitimately
    when randomizing items with location-dependent effects on state logic (such
    as transitions).

### Notes on Step 2

- The items tested in this step are called "proposed items". Items which pass
  the test are changed from "proposed" to "accepted" and items which fail the
  test are changed from "proposed" to "rejected".
- Testing each item is done by reverting to the progression state at the start
  of the step, and then adding the current list of proposed and accepted items,
  excluding the item currently being tested.
- Whenever the test accepts an item, a follow-up test runs to check whether the
  currently accepted items are sufficient to unlock a new location, in which
  case all remaining candidates are rejected.

#### Commutativity Failures

Consider the following sequence of events during Step 2:

> 1. Revert to initial progression. Add proposed items. Add accepted items.
> 2. Observe that a new location is unlocked.
> 3. Revert to initial progression. Add proposed items except current test item.
>    Add accepted items.
> 4. Observe that no new locations are unlocked.
> 5. Accept the test item, and add it back to progression.
> 6. Observe that still no new locations are unlocked.

The observations in (2) and (6) are contradictory, since the progression
contains the same items, albeit added in different orders. The assumption that
"the order in which items are added does not matter" has been violated. In this
case, @RandomizerCore.Exceptions.CommutativityFailureException is thrown, and
the randomizer halts. This error is generally difficult to debug, but is likely
due to one of the following:

- An `=` or `<` comparison on a term, possibly occurring in logic or logic
  variable definitions.
- An `=` or `>` comparison on a state field, possibly occurring in logic or
  logic variable definitions.
- Any logic evaluated to determine the effect of an item.

For a concrete example, if we have a Term `T`, then the item string `T++` does
not commute with the item `` `T < 1` => T++ ``. If we give the former, followed
by the latter, the result sets `T` to `1`. If we give the items in reverse
order, the result sets `T` to 2. Conditional effects in items have legitimate
uses, but they always require careful consideration for how they interact with
other items that modify the same terms.

Another example is if we have a logic def or logic variable which checks extra
cases when progression or state is "bad". It is easy to accidentally write the
implementation to succeed (or return a better state) only when these extra cases
run. This quickly turns into a subtle bug that only appears when different item
orders lead to a difference in whether the extra cases run as logic is
re-evaluated.

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

### Progression Order Guarantee

In this section, we explain how the priority order of items corresponds to the
order in which progression is selected. The brief summary is that the forward
fill algorithm optimizes toward avoiding the use of items later in priority
order over all other considerations.

> [!TIP]  
> An item cannot be placed as intended progression unless there is no
> progression from all of the previous items in priority order combined. When a
> progression step consists of multiple items, there is no progression with all
> items before the highest priority item, nor with the highest priority item
> along with all items before the second highest priority item, and so on.  
> For consumers, the @RandomizerCore.Randomization.RandomizationGroup.OnPermute
> event can be used to put an item later in priority order, preventing it from
> being used for intended progression until all earlier progression is
> exhausted.

The rest of this section examines how the forward fill selects progression in a
toy scenario, to give some motivation for where the above claims come from.
First, let us recall the algorithm:

- Step 1 computes the least `n` such that the first `n` unplaced items in
  priority order are progression (unlock a new location).
- Step 2 finds a progression sublist of the first `n` items from Step 1 which is
  minimal in terms of sublist-inclusion; that is, a sublist which is
  progression, but which contains no proper sublist which is progression.
- More specifically, the result of Step 2 is found by eagerly removing items
  occurring later in priority order if they are not required for progression.

For illustration, let us write the result of Step 1 as `ABCDEFG`, where each of
the seven letters represents an item, and the items are in priority order
`A < B < ... < G`. Then, for example, the progression sublists which are minimal
in terms of sublist-inclusion might be `AEG`, `DEG`, `BG`, `FG`. Note that they
all contain `G`, since `ABCDEF` was not progression in Step 1. Something like
`ACEG` is progression, but not minimal in terms of sublist-inclusion, since it
contains the sublist `AEG`.

The result selected by Step 2 from these four sublists would be `BG`. We can see
this iteratively:

> 0. `ABCDEF` is not progression, so accept `G`. `G` alone is not progression,
>    so continue testing.
> 1. `ABCDEG` is progression since it contains `AEG`, so reject `F`.
> 2. `ABCDG` is progression since it contains `BG`, so reject `E`.
> 3. `ABCG` is progression since it contains `BG`, so reject `D`.
> 4. `ABG` is progression since it contains `BG`, so reject `C`.
> 5. `AG` is not progression, so accept `B`. `BG` is progression, so reject `A`
>    and finish.

The property that characterizes `BG` among the 4 sublists is that it is their
minimum element with respect to a certain ordering, called "colexicographic
ordering". If the standard dictionary ordering would start by comparing the
priorities of the first element of each list, this ordering instead starts by
comparing the priorities of the last element of each list. So `BG < AEG` since
they have the same priority in the last element, and for the next-to-last
element, `B` has lower priority than `E`. Similarly, `AEG < DEG` since they
agree in the last two entries, but `A` has lower priority than `D`. Finally,
`DEG < FG`, since `E` has lower priority than `F`.
