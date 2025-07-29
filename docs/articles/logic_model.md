# Logic Model Basics

Most successful randomizers do not place items purely randomly, since this could result in seeds that cannot be
completed. Instead, they rely on a set of access rules usually referred to as **logic** to evaluate what locations
are available at any given point in the randomization process. This article provides a conceptual overview of how 
logic is modeled in RandomizerCore. It first discusses the logic string grammar to contextualize the model, then
it describes each component in more depth.

## Logic Strings

Logic in RandomizerCore is represented with a fairly simple string-based grammar. To ground our explanation with
an example, we can imagine that the logic for some location is represented as the following string:

```text
Room1 + (DoubleJump | Dash + Airstalls)
```

We can read this string to mean that the location can be accessed if the player has access to room 1 and has the
double jump ability, or, if airstall skips are enabled, the dash ability.

The logic language has a few operators, seen in the table below

| Operator | Name | Example | Read as | Note |
| --- | --- | --- | --- | --- |
| `+` | Conjunction (boolean AND) | `Foo + Bar` | Foo and Bar | |
| `|` | Disjunction (boolean OR) | `Foo | Bar` | Foo or Bar | |
| `>` | Term comparison | `Foo>1` | Foo is greater than 1 | |
| `?` | Coalescing | `Foo ? Bar` | Foo if Foo is defined, otherwise Bar | Useful for compatibility between third party logic edits |
| `*` | Reference | `*Location` | Copy the logic of Location | Reduces logic duplication when 2 locations are very near each other |
| `/` | Projection | `Foo/` | Foo as a boolean | Advanced feature, see <xref:state> |

Note that the `<` and `=` operators, as well as comparisons between two terms are technically supported; however
they are generally unsafe for use in logic since they can cause locations which were previously in logic to go out
of logic later, which is not allowed.

### Order of Operations

Operators are applied in the following order:

* Reference
* Projection
* Term comparison
* Coalesing
* Conjunction
* Disjunction

This means that an (unlikely) expression like the following:

```text
A + B | C>1 ? *D/
```

Is implicitly parenthesized as:
```text
(A + B) | ( (C>1) ? ( (*D)/ ) )
```

Most notably, this operator precedence order allows listing complex alternatives without needing parentheses, for
example `A + B | C + D | E + F + G + H | ...` is interpreted as `(A + B) | (C + D) | (E + F + G + H) | ...` and
thinking about logic as different sets of alternative requirements is a very natural way to approach writing logic.

## Terms

Terms are the fundamental building block of logic in RandomizerCore. They act as global data storage
which can be referred to in logic expressions. In the first example above, "Room1", "DoubleJump", "Dash", and
"Airstalls" are all examples of terms. Note that even in this example, though they generally represent things
things you "have" or "have access to", they are not the same as items. We will discuss the relationship between 
terms and items in more detail later in this article.

Most terms are signed integer types (either 8 bit or 32 bit). This allows, for instance, counting multiple
collected occurrences of the same item or counting levels of an ability. However, as you can see in the examples
above, an integer term appearing in a logic string outside of a comparison is evaluated as a boolean, resulting
in true if the value is greater than zero. For advanced use cases, terms can also have the State type. For the
purpose of this article we will only consider integer terms. More details on state logic can be found in the
[dedicated article](xref:state) on the topic.

RandomizerCore defines the following terms automatically. Other terms are defined by randomizer implementations.
* `TRUE` or `ANY` - evaluates to true
* `FALSE` or `NONE` - evaluates to false

## Waypoints

Waypoints are a special type of term which have logic associated with them. When the logic is satisfied, the term
is then granted. In the first example above, it might be the case that "Room1" is a waypoint with the logic:

```text
Room1_LeftDoor | Room1_RightDoor
```

This would mean that when either door to Room1 is accessible, the Room1 waypoint itself would become accessible.
We can see from this example that waypoints can be an efficient way to create reusable logic expressions, which
can make it easier to read and write logic as well as improve performance in some situations by pre-evaluating
part of the logic.

## Macros

Macros are another way to create reusable logic. Logic macros are substituted in-place, which can be desirable
in some situations, particular when state logic is involved. They are also an efficient way to create aliases for
terms or short snippets. However for more complex logic, using a waypoint is recommended in most cases for performance
reasons.

## Items

Items are the primary way that terms are modified. Items may share a name with terms and locations. Most items
will simply increment a term with the same name as an item, however items may also do any of the following:
* Modify terms not sharing their name (for example, giving the term a shorter name than the item)
* Modify multiple terms (for example, indicate that you have a specific item as well as increment the count
  of a specific type of collectible)
* Do nothing ("filler" items)

There is also a string-based grammar for representing item effects. For a more in-depth discussion, see
<xref:item_strings>. This allows complex item effects to be easily serialized and statically analyzed.

## Locations

Locations represent where items can be placed. Similar to waypoints, locations have logic associated with them.
However, when they become accessible, instead of granting a term, they apply the effect of the item that is placed
there.

## Transitions

Transitions are a special case of location which are also an item. The main differences between normal locations
and transitions are as follows:
* Every transition location is created with a corresponding term and item with the same name
* Transition items grant the corresponding term when obtained. They also [transmit state](xref:state) from the
  location that they are placed at.
* When a transition location becomes accessible in logic, the corresponding item is granted automatically (similar
  to a waypoint). In other words, once you "can reach" that transition, you also "have" it available for use in
  logic strings.

## Constructing a LogicManager

To be able to initialize logic in code, there are 2 simple high-level steps:

* Add terms, waypoints, macros, items, locations, and transitions to a @RandomizerCore.Logic.LogicManagerBuilder
* Use the LogicManagerBuilder to initialize a @RandomizerCore.Logic.LogicManager

A LogicManagerBuilder is used to construct the logic model from your data. The various methods on the class allow
you to add all logic objects individually, but it is more common to use
@"RandomizerCore.Logic.LogicManagerBuilder.DeserializeFile(RandomizerCore.Logic.LogicFileType,RandomizerCore.Logic.ILogicFormat,System.IO.Stream)"
to load logic in bulk from files. Currently the only publicly available @RandomizerCore.Logic.ILogicFormat
implementation is defined in [RandomizerCore.Json](https://www.nuget.org/packages/RandomizerCore.Json), which
allows specifying the logic in a JSON format. However, custom logic formats can also be defined if desired.

Once constructed, a LogicManager contains all the compiled logic and item definitions, which are used during
randomization. You can also use the LogicManager to statically analyze the logic of locations or the effects of
items, which has various practical applications outside of the randomization process.