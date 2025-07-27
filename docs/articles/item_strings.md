---
uid: item_strings
---

# Item Strings

Item strings provide a simple grammar for defining items that act on the terms which represent the player's 
permanent abilities and progression. Item strings consist of primitive effects, which can be chained together 
and controlled via conditionals depending on current progression. The primitive effects are:
- Addition assignment `+=` (`Term += Int`) and increment (`Term++`, equivalent to `Term += 1`), 
  which add the specified amount to the term.
- Max assignment `=/` (`Term =/ Int`), which raises the term to the specified value if it is below that value.

Effects can be combined via 
- Conditional `=>` (`Bool => Effect`), which performs the effect if the bool test evaluates true. 
  The bool test should be standard logic, wrapped in backticks.
- Chaining (`Effect >> Effect`), which performs the effects in sequence.
- Short-circuit chaining (`Effect >|> Effect`), which attempts to perform the first effect, and performs the 
  second effect only if the first failed (e.g. if the first effect were a conditional and the bool test 
  evaluated false).

There are also a few miscellaneous meta-symbols.
- Negation: a logic statement can be preceded by `!` to invert the test.
- Reference expressions: `*` followed by text resolves to the effect of the item with that name. 
  This can be used to introduce effects which cannot otherwise be expressed in terms of item strings.
- Coalescing expressions: In expressions which expect a term, a `?` can be added after the term name. 
  When the item string is converted to an item, if the term with that name was not defined, the expression 
  directly containing the term is replaced with an empty (no-op) effect. This can be useful for compatibility 
  between third party logic edits.

## Order of Operations

Item effects are always applied left to right. Parenthesization is important however for understanding how 
conditional and chaining expressions interact. Operators are bound to their arguments in the following order:

- Addition assignment, increment, max assignment, negation, reference, and coalescing expressions
- Conditionals
- Short-circuit chains
- Chains

Also, it should be noted that the chaining operators have higher right binding power (thus, are left associative). 
In particular, the order of operations ensures that
```text
A++ >> `A>0` => B++
```
is well-formed without parentheses, and can be interpreted as "increment A, then if A is now positive, 
increment B".

We give a few examples of parenthesizations (equivalence denoted with `===`):

```text
`A` => B++ >|> C++ >> D++ === (((`A` => B++) >|> C++) >> D++)
`A` => B++ >|> C++ >|> D++ === (((`A` => B++) >|> C++) >|> D++)
```

For a more complex example:

```text
`A` => (`B` => (C++ >> D++) >|> `E` => F += 2 >|> G++)
```

is interpreted as

```text
`A` => (((`B` => (C++ >> D++)) >|> (`E` => F += 2)) >|> G++)
```

or rather

```text
`A` => (`B` => (C++ >> D++) 
            >|> `E` => F += 2 
            >|> G++)
```

where the indentation matches short-circuit operators with the corresponding conditionals (in other words, the 
`E` branch is reached if `A` succeeds and `B` fails, while the final branch `G++` is reached only if `A` succeeds 
and `B` and `E` both fail). Note that the parentheses around `(C++ >> D++)` are needed or else it would bind as 
``(`B` => C++) >> D++``. The outermost parentheses are also needed, or else we would get

```text
((`A` => (`B` => (C++ >> D++))) >|> (`E` => F += 2)) >|> G++
```

In this case, `G` is reached only if `E` fails and one of `A` or `B` fails.