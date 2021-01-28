# Push/Pull FRP

# Problem

## Thinking backwards gets sometimes hard

> Problem formulation credits go to Maciej Bielecki.

Consider requirement: *when Clear button is clicked, set Name and Address fields to empty*. (1)

Imperative code is straightforward:

```
clear.onClick do
  name.set ""
  address.set ""
```

In FRP, however you must think backwards (from outcomes back to triggers) which can be seens as harder:

```
name <- holdDyn "" $ leftmost [ userChangedName, "" <$ clear.clicked ]
address <- holdDyn "" $ leftmost [ userChangedAddress, "" <$ clear.clicked ]
```

This is more like the requirement was *Address gets empty when Clear button is clicked* (2), which is counter-natural and not the usual way people express requierements.

Morever, (1) fully specifies Clear button and partially specifies Name and Address fields, (2) specifies partially both the button and the fields.
FRP claims it's benefitial to fully specify things at the time of declaration.

The problem with imperative code as above is that the effects of an event can be arbitrarily spread all over the code what's hard to reason about.

# Hypothesis

Imperative code (as used in this context) vs FRP is false contrary.

[The essence of FRP is to specify the dynamic behavior of a value completely at the time of declaration](https://apfelmus.nfshost.com/blog/2011/03/28-essence-frp.html), where dynamic behaviour can be effectful imperative code.
So we should be able to specify imperative behaviour completely at the time of declaration.

The following code would still be FRP even though it contains imperative specification of clear behaviour:

```
-- complete and only specification of Clear, but still imperative
clear = ...

-- complete and only specification of Name, FRP as we are used to: more declarative
name = ...

-- complete and only specification of Address, FRP as we are used to: more declarative
address = ...
```

At the risk of overloading the term push-pull FRP (originally coined by Conal Elliot) let me characterize `clear` as *push* (imperative, effectful etc.) and `name` and `address` as *pull* (declarative).

Going further I'd like to check whether it's the general rule that events can only be *pushed* and behaviors can only be *pulled*.
Events (and only events) can change the state of the application so only *push* can change the state.
Conversely, *pulled* behaviours can not change the state.

As a *cell* I introduce a stored variable that can be pushed into and pulled from.

Pseudocode:

```
clear :: Push ()
clear = write nameCell "" <> write addressCell ""

name :: Pull String
name = read nameCell

address :: Pull String
address = read addressCell
```

Notice that tracing dependencies between such defined `Push`es and `Pull`s requires only looking at their declarations.

`mkCallback`/`triggerCallback` pattern

With this approach we can get rid of plain imperative dispatching of callbacks, like the following:

```
callback1 = mkCallback \status -> do
              (...)
              triggerCallback callback2 status
```

Such code has drawbacks that resemble *callback hell* problem:
  * we're not restricted in effects we do in `do` block, we can do any effect: access mutable state, log, change UI etc.
  * we're exposed to various programmer errors, as the `do` block can quickly get obsuscated

Again, FRP is about replacing callbacks with declarative way of specifying dynamic behaviors, so we can write:

```
callback1 = (...) $ callback2
```

`triggerCallback` should then be reserved for things that really triggers push (UI controls, HTTP reponses, browser events etc) the rest of the push is really not a "trigger".

All in all, the proposed approach will let us contain all business logic in FRP manner using declarations such as `clear = ...` and keep it separated from presentation code.

# Proof of concept

[code]

# Call to action

  * introduce combinators for push FRP to Specular
  * retrofit parts that
