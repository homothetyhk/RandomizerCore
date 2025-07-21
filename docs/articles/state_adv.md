# Implementing State Logic

## Defining State Fields

State fields can be defined by using the @RandomizerCore.Logic.StateLogic.StateManagerBuilder attached to a 
@RandomizerCore.Logic.LogicManagerBuilder, or by defining them in a Json file and feeding the file into the 
LogicManagerBuilder. Once defined, the type and name of a state field cannot be changed. Custom properties can 
be assigned to the state field via the @RandomizerCore.Logic.StateLogic.StateManagerBuilder; this mechanism, 
for example, allows specifying the default value of the field and many other things.

When adding a state field, it is essential to carefully consider how it will interact with the State 
ordering. For example, by default, state bools default to false, and can be set true. This is ideal for 
representing a consumable resource which one starts with, and can be spent once. States which have spent the 
resource will be discarded if there is a strictly better alternative which has not spent the resource. 
On the other hand, to represent a resource which one does not start with, but can be obtained once later, 
the state bool should be created to default to true, and be set false once the resource is obtained, 
so that states with the resource will not be pruned.

## Defining LogicVariables to Interact with State

A @RandomizerCore.Logic.LogicVariable is a token in logic which has special effects defined in code. 
To define any LogicVariable, one needs to define a @RandomizerCore.Logic.VariableResolver which can 
identify it by name. The VR is attached to the LogicManagerBuilder and to the LogicManager. To extend the 
strings recognized by a VR, first make a class deriving from VariableResolver which overrides TryMatch, 
then when replacing the old VariableResolver, assign the old VR to the Inner property of the new VR. 
Then calls which the outer VR cannot handle will be passed to the inner VR.

### StateModifier

[StateModifiers](xref:RandomizerCore.Logic.StateLogic.StateModifier) take an input state and produce a sequence 
of output states, through the ModifyState method. They also can produce a sequence of output states with no input, 
through the ProvideState method. In a conjunction, StateModifiers act left-to-right sequentially, modifying the 
output of the previous modifier and providing any additional states. StateModifiers work in the general setting 
by first converting logic to disjunctive normal form and determining the input state for each conjunction in the 
DNF, then working as described for conjunctions.

Note: since StateModifiers do not derive from LogicInt, they cannot be used in comparisons (i.e. expressions 
with '<','=', or '>').

To define a StateModifier, one must implement ModifyState to return a non-null sequence of states. ModifyState 
should return an empty sequence if the input fails. 

ProvideState can be optionally overriden, to express when the StateModifier is able to succeed regardless of 
its input. Here, ProvideState should return a null sequence if the input fails, and its default implementation 
is to always return null. The empty sequence expresses that ProvideState succeeded with indeterminate output.

### StateProvider

Ordinarily, input state is determined by the first state-valued term (e.g. transition or waypoint) which appears 
in a conjunction. However, a @RandomizerCore.Logic.StateLogic.StateProvider variable can be defined to supply a 
state determined in code, if it appears before any other state provider terms or variables in the conjunction. 
For example, `$DEFAULTSTATE` provides a StateUnion containing the state which has all fields at their default 
values. `$ANY` provides an empty StateUnion.

A StateProvider is additionally a LogicInt. Its `LogicInt.GetValue` returns 1 if the provided state is non-null, 
and 0 otherwise.

### StateAccessVariable

A @RandomizerCore.Logic.StateLogic.StateAccessVariable receives progression data and a state, and produces an int. 
These can be used in comparisons (i.e. expressions with '<','=', or '>'), and can be seen as a special type of 
StateModifier which filters out states that fail the comparison. In other words, ModifyState returns its state 
input as a singleton sequence if the comparison succeeds, and returns an empty sequence if the comparison fails. 
ProvideState always return null, since we cannot say that the comparison would succeed for an indeterminate state.

RandomizerCore has a builtin class of StateAccessVariables which are recognized by the default VariableResolver: 
[StateFieldAccessors](xref:RandomizerCore.Logic.StateLogic.StateFieldAccessor). These are represented in logic by 
the exact name of a state field, when used in a comparison. For a state int, the StateFieldAccessor retrieves 
the value of the int from its input state. For a state bool, the StateFieldAccessor retrieves 1 if the bool is 
true, and 0 otherwise.

## Integration with the ProgressionManager and MainUpdater

When evaluating bool logic, all terms are interpreted as int-valued. The int value of a state-valued term is 0 if 
its state is null, and 1 otherwise. This is the value returned by pm.Get(id) if id is the id of a state-valued 
term, and the value used for derived computations such as pm.Has, etc. Use pm.GetState to retrieve the full 
@RandomizerCore.Logic.StateLogic.StateUnion associated with the term.

The @RandomizerCore.Logic.MainUpdater automatically manages logic-derived state updates for state-valued 
waypoints and transitions. Use @"RandomizerCore.Logic.MainUpdater.AddManagedStates(System.Collections.Generic.IEnumerable{System.ValueTuple{RandomizerCore.Logic.Term,RandomizerCore.Logic.StateLogic.StateLogicDef}})" 
to give a state-valued term logic-derived state updates. Often, the effect of an item may be to trigger an 
ongoing state modification. This can be done by modifying the MainUpdater attached to the 
@RandomizerCore.Logic.ProgressionManager. If the state effect depends on the item's location, this can further be 
done within an @RandomizerCore.ILocationDependentItem implementation for the item.

## Warnings in State Logic Generation

### DNF Clause Count
A warning is generated when the expansion of logic into disjunctive normal form produces a large number of clauses. To avoid degraded performance:
- Avoid adding redundant branches to logic. When adding disjunctions as in 
  `(A1 | B1) + (A2 | B2) + ... + (An | Bn)`, the number of clauses grows exponentially as `2^n`, so eliminating 
  branches can yield large savings. Note that these disjunctions may be hidden from view through macros or logic 
  references, but are still costly.
- Consider moving part of the logic into a waypoint. Waypoints can potentially be more efficient as they do not 
  have to recompute logic as often.

### Ambiguous State Provider
A warning is generated when a clause in the disjunctive normal form contains multiple state providers 
(state-valued terms or @RandomizerCore.Logic.StateLogic.StateProvider variables). To avoid the warning, one can 
add a trailing `/` to the non-provider, as in `Room[left] + Room[right]/`. However, if the situation requires the 
state from both providers (for example, the first one provides the state that should propagate forward, 
but the other needs to satisfy a state condition), then the logic for the second state provider should be moved 
to a new logic def or waypoint, and changed to a reference or waypoint term accordingly.

### Missing State Provider
A warning is generated when logic in disjunctive normal form contains some clauses with state providers and some 
clauses without. To avoid the warning, do one of the following:
- Move the state-valued logic to a stateless waypoint, and use the waypoint in the stateless logic.
- Use the trailing `/` to suppress the state providers in the relevant clauses. Note that this does not work if 
  state modifiers are present.
- Add a state provider to the stateless logic. Common choices would be `$ANY` or `$DEFAULTSTATE`.

### State Modifier occurring before State Provider
A warning is generated when logic in disjunctive normal form contains a conjunctive clause with a state modifier 
occurring to the left of the state provider. To avoid the warning, move the state provider to occur before any 
state modifiers. For example, replace `$StateModifier + Transition` with `Transition + $StateModifier`. 
These clauses are equivalent in meaning, but it is preferred to list the state provider first to avoid confusion 
regarding order of application.