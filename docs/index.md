# Intro

RandomizerCore is a .NET library for randomizer logic and algorithms, created for and released with
Hollow Knight's Randomizer 4 mod. It is also available on [nuget.org](https://www.nuget.org/packages/RandomizerCore).
Although it was created for use with Hollow Knight, it is designed to be game-agnostic and can be used in any
.NET game supporting .NET Standard 2.0 or and later.

## Features

* Game-agnostic randomization
* Serializable string-driven logic format
* Resource tracking (state) logic support (not supported by most other randomizers)
* Randomization stages and groups
* Default support for transition randomizer

## Randomizer Taxonomy

Readers familiar with randomizers may be interested to know how RandomizerCore compares with other randomization
techniques. Here are some fast facts about RandomizerCore's implementation:

* **Randomization Algorithm**: Heavily modified forward fill
* **Logic Model**: Internally represented as a dependency graph (as opposed to a topology graph), serialized as a
  location list
* **Transition/Entrance Randomization**: Built-in; transition placements are modeled as item placements

## Randomizers Using RandomizerCore

* [Hollow Knight](https://github.com/homothetyhk/RandomizerMod)
* [Death's Door](https://github.com/dpinela/DeathsDoor.Randomizer)

If you implement a randomizer using RandomizerCore, feel free to submit a PR adding yourself here!