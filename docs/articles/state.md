# Intro to State Logic

At a very high level, the randomizer consists of locations which have certain requirements and items which 
combine to fulfill those requirements. In RandomizerCore, these requirements are expressed through terms 
that appear in both logic and item definitions. Terms must represent permanent abilities or events; one of 
the core assumptions of the randomizer is that items only expand (and never restrict) the class of reachable 
locations, so items cannot have a temporary effect on terms.

The purpose of state logic is to allow representation of temporary effects (whether from items or pathing) 
in logic. Instead of partitioning locations into a binary reachable/unreachable, state logic allows a location 
to produce a list of alternative states that characterize its reachability. States may contain information 
regarding consumable resources, equipment which can't be swapped out in certain locations, and other types of 
decisions which might be made on a path-dependent basis. In what follows, some of the basic concepts of state 
logic are explored, including what exactly RandomizerCore allows in a state and how they can be created and 
modified using logic.

## Background

When Hollow Knight's Randomizer 2 was first released, state logic would have been completely unnecessary. 
Back then, logic for every location was written painstakingly in terms of every manner to reach that part of 
the world from the start of the game in King's Pass. As Randomizer 2 developed into Randomizer 3, and start 
locations and room transitions became randomizable, more and more indirection had to be used in logic to 
make things still work. The result is that the logic became *relative* -- instead of just listing combinations 
of items to unlock a location, it started to list combinations of items, along with points of the world that 
had to be "reachable". A location in King's Pass which previously had logic which always evaluated true, now 
had logic which evaluated true when King's Pass was "reachable", which might be always if the game started 
in King's Pass, but might mean something very different if the game started elsewhere.

One problem with this system is that "reachable" may not have a single clear definition. In Hollow Knight, 
different paths to the same location may require equipping different charms, spending soul (MP), or taking 
damage. To get around this originally, we wrote the randomizer logic with very stringent restrictions on 
"nonterminal" logic: basically, we required transitions and waypoints to never make those kinds of decisions. 
Only "terminal" logic, meaning logic for actual locations, could require charms or soul or anything along 
those lines. In practice, these guidelines were overly limiting and had to be repeatedly broken. State logic 
was introduced to handle path-dependent information in an automatic, consistent, and decentralized fashion.

## State Mechanics

The @RandomizerCore.Logic.StateLogic.State class consists of a collection of Bool values and a collection of Int 
values. These fields are accessed through IDs which can be retrieved by name (as 
[StateFields](xref:RandomizerCore.Logic.StateLogic.StateField) in the 
@RandomizerCore.Logic.StateLogic.StateManager, which is attached to the @RandomizerCore.Logic.LogicManager.

The most important aspect of State is its ordering. First, the type Bool is totally ordered by false < true, 
and the type Int is totally ordered by its usual ordering. Then, State is *partially* ordered by the ordering 
on each of its fields. That is, for two states s1 and s2, we have s1 <= s2 if and only if for every state field 
f, s1.f <= s2.f. This is very important, because when the randomizer determines that a list of states can access 
a location, it reduces the list by removing states which are not minimal in the list with respect to the partial
order.

The @RandomizerCore.Logic.StateLogic.StateUnion class represents a collection of states which have been 
reduced by the process above. In other words, the states in a StateUnion are always pairwise incomparable 
with respect to the partial order, and a StateUnion is formed from a list of states by discarding states 
which compare as greater or equal to some other state in the list. Generally speaking, State should be 
interpreted as a particular combination of resources, and StateUnion should be interpreted as an 
alternative of possible states. StateUnion has two special values. A null StateUnion represents nonaccessibility. 
An empty StateUnion represents indeterminate (or minimum) accessibility. This notion is useful for example 
when randomizing transitions: for a given target transition, until its source is known, its state is unknown. 
By giving it the empty StateUnion, it can succeed at trivial state checks, though not nontrivial state checks. 
When the transition is finally placed at a source, the @RandomizerCore.ILocationDependentItem implementation 
of @RandomizerCore.RandoTransition or @RandomizerCore.Logic.LogicTransition updates the state of the target to 
include the state of its source.

## State Logic

The @RandomizerCore.Logic.StateLogic.StateLogicDef class is the base class for logic defs which support state 
calculations. @RandomizerCore.Logic.DNFLogicDef is the RandomizerCore implementation of StateLogicDef.

Before evaluating state logic, the logic has to be put in disjunctive normal form. What this means is that 
all nested "or" operations are expanded out, so that the expression becomes an "or" of subexpressions which only 
contain "and" (a disjunction of conjunctions). For example, the disjunctive normal form of 
"A + (B | (C + D) | E)" is "(A + B) | (A + C + D) | (A + E)". The state modifiers in each conjunction then act 
sequentially left-to-right to perform state modifications. For more information, see the article on 
[implementing state logic](state_adv.md).

## Basic State Model

We examine a toy model to illustrate how state logic works. We suppose we have a 
@RandomizerCore.Logic.LogicManager with the following terms:
- `Item1` of type Int
- `Item2` of type Int
- `Room[left]` of type State (representing a doorway, transition, etc into the room)

and a @RandomizerCore.Logic.StateLogic.StateManager with the following fields:
- `SBool1` of type Bool
- `SBool2` of type Bool
- `SInt1` of type Int

We also assume that we have defined a custom @RandomizerCore.Logic.VariableResolver to provide the following 
[StateModifiers](xref:RandomizerCore.Logic.StateLogic.StateModifier):
- `$TryIncrement[INTNAME,CAP]` (increments the named StateInt, fails if the result exceeds the cap)
- `$TrySetBool[BOOLNAME]` (sets the named bool true, fails if the bool is already true)

(Note that the `$` in front of variables in logic is only a convention; the `$` does not have any special role 
here.)

Then, given the above, a fragment of state logic might look like: 

`Room[left] + Item1 + Item2 + $TryIncrement[SInt1,2] + $TrySetBool[SBool1]`

This fragment breaks down into
- a state provider (the state-valued term `Room[left]`)
- a stateless requirement (the non-state terms `Item1` and `Item2`)
- a requirement on the provided state (given by the statemodifiers `$TryIncrement[SInt1,2]` and 
  `$TrySetBool[SBool1]`)

The entire fragment can be interpreted as: "Require `Item1` and `Item2`, and also require that `Room[left]` 
can be reached with a state with `SInt1 < 2` and `SBool1 = false`."

For a more complicated example, we could add another state-valued term `Room[right]` and then consider the 
following logic:

`(Room[left] | Room[right]) + (Item1 | Item2) + ($TrySetBool[SBool1] | $TryIncrement[SInt1,1]) + 
($TrySetBool[SBool1] | $TryIncrement[SInt1,2])`

The disjunctive normal form of this expression, given by distributing all of the operations, results in 16 clauses:

```text
  Room[left] + Item1 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
| Room[left] + Item1 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
| Room[left] + Item1 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
| Room[left] + Item1 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
| Room[left] + Item2 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
| Room[left] + Item2 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
| Room[left] + Item2 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
| Room[left] + Item2 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
| Room[right] + Item1 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
| Room[right] + Item1 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
| Room[right] + Item1 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
| Room[right] + Item1 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
| Room[right] + Item2 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
| Room[right] + Item2 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
| Room[right] + Item2 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
| Room[right] + Item2 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
```

In each clause, the state-valued term `Room[left]` or `Room[right]` provides a state; there are then requirements 
from nonstate terms which are independent of the provided state, and requirements from the state modifiers which 
act on the provided state sequentially. A few remarks:
 - Some of the clauses above are impossible to satisfy. For example, anything with 
   `$TrySetBool[SBool1] + $TrySetBool[SBool1]` can never be satisfied, since the first 
   `$TrySetBool[SBool1]` sets `SBool1` to `true`, and the second `$TrySetBool[SBool1]` subsequently fails. 
 - The left-to-right application of state modifiers is essential: 
   `$TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]` succeeds on a state which initially has `SInt1 = 0`, 
   since neither increment causes `SInt1` to exceed either cap. On the other hand, 
   `$TryIncrement[SInt1,2] + $TryIncrement[SInt1,1]` fails, since after 2 increments we have `SInt1 = 2`, 
   which exceeds the cap for the second increment.
 
 The location represented by the logic is reachable if any of the 16 clauses is satisfied. A state union 
 representing the states that can reach the location could be created by accumulating the state-valued results 
 of the satisfied clauses, and then reducing according to the state partial order. For example, suppose we 
 evaluate with the following term values:

 ```text
 Item1: 1
 Item2: 0
 Room[left]: [{ SBool1: false, SBool2: true, SInt1: 0 }]
 Room[right]: [{ SBool1: false, SBool2: false, SInt1: 1 }, { SBool1: true, SBool2: false, SInt1: 0 }]
 ```

 Then clause-by-clause evaluation yields:
 
 ```text
Room[left] + Item1 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
    fails
Room[left] + Item1 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
    produces [{ SBool1: true, SBool2: true, SInt1: 1 }]
Room[left] + Item1 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
    produces [{ SBool1: true, SBool2: true, SInt1: 1 }]
Room[left] + Item1 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
    produces [{ SBool1: false, SBool2: true, SInt1: 2 }]
Room[left] + Item2 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
    fails
Room[left] + Item2 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
    fails
Room[left] + Item2 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
    fails
Room[left] + Item2 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
    fails
Room[right] + Item1 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
    fails
Room[right] + Item1 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
    [{ SBool1: true, SBool2: false, SInt1: 2}]
Room[right] + Item1 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
    fails
Room[right] + Item1 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
    fails
Room[right] + Item2 + $TrySetBool[SBool1] + $TrySetBool[SBool1]
    fails
Room[right] + Item2 + $TrySetBool[SBool1] + $TryIncrement[SInt1,2]
    fails
Room[right] + Item2 + $TryIncrement[SInt1,1] + $TrySetBool[SBool1]
    fails
Room[right] + Item2 + $TryIncrement[SInt1,1] + $TryIncrement[SInt1,2]
    fails
```

The list of states produced is

```text
[
    { SBool1: true, SBool2: true, SInt1: 1 }, 
    { SBool1: true, SBool2: true, SInt1: 1 }, 
    { SBool1: false, SBool2: true, SInt1: 2 }, 
    { SBool1: true, SBool2: false, SInt1: 2 }
]
```
Reducing along the partial order, the resulting @RandomizerCore.Logic.StateLogic.StateUnion is

```text
[
    { SBool1: true, SBool2: true, SInt1: 1 }, 
    { SBool1: false, SBool2: true, SInt1: 2 }, 
    { SBool1: true, SBool2: false, SInt1: 2 }
]
```