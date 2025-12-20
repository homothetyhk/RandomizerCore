# Intro

RandomizerCore is a .NET library for randomizer logic and algorithms, created for and released with
Hollow Knight's Randomizer 4 mod. It is also available on [nuget.org](https://www.nuget.org/packages/RandomizerCore).
Although it was created for use with Hollow Knight, it is designed to be game-agnostic and can be used in any
.NET game supporting .NET Standard 2.0 or later.

## Features

* Game-agnostic randomization
* Serializable string-driven logic format
* Resource tracking (state) logic support (not supported by most other randomizers)
* Randomization stages and groups
* Default support for transition randomizer

## Randomizer Taxonomy

Readers familiar with randomizers may be interested to know how RandomizerCore compares with other randomization
techniques. Here are some fast facts about RandomizerCore's implementation, presented with some additional detail
for those who may not be familiar:

* **Randomization Algorithm**: Heavily modified forward fill
  * Forward fill is essentially the process of starting with an empty inventory, determining what locations are
    currently reachable, placing some progression, and then repeating until all locations are reachable.
  * This approach often biases towards front-loaded progression. RandomizerCore adds various tunable parameters to
    offset the bias and otherwise tweak the behavior of the randomizer.
* **Logic Model**: Internally represented as a dependency graph (as opposed to a topology graph), serialized as a
  location list
  * RandomizerCore's logic is represented in a simple to use string-based logic language. Each location's logic
    is unstructured in that it can be represented in isolation of other modeling constructs, such as nodes in a
    graph.
  * RandomizerCore parses these strings to construct an efficient graph representation in which logic for a
    location only needs to be re-evaluated when one of its logical dependencies is modified.
* **Transition/Entrance Randomization**: Built-in; transition placements are modeled as item placements
  * Many randomizers treat transition randomization as a second-class citizen. In RandomizerCore, transition
    randomization is fully supported and transitions are largely treated the same as other placements, randomized
    in a different stage.

## Randomizers Using RandomizerCore

* [Hollow Knight](https://github.com/homothetyhk/RandomizerMod)
* [Death's Door](https://github.com/dpinela/DeathsDoor.Randomizer)

If you implement a randomizer using RandomizerCore, feel free to submit a PR adding yourself here!
