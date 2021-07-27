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

In FRP we're used to, we rather "think backwards" (from outcomes back to triggers) which can be seen as harder to grasp:

```
name <- holdDyn "" $ leftmost [ userChangedName, "" <$ clear.clicked ]
address <- holdDyn "" $ leftmost [ userChangedAddress, "" <$ clear.clicked ]
```

This is more like the requirement was *Address gets empty when Clear button is clicked* (2), which is counter-natural and unusual. Does that mean that FRP is not suitable in such case?

Moreover, (1) fully specifies Clear button and partially specifies Name and Address fields while (2) fully specifies the fields but partially the button.
FRP claims it's benefitial to fully specify things at the time of declaration. Here it's seems that we cannot have both the fields and the button specified completely at the time of declaration. Does it mean FRP is wrong?

# Hypothesis

Imperative code vs FRP is a false contrary. Imperative code can be FRP. FRP has to embrace imperative code, otherwise it's not suitable.

[The essence of FRP is to specify the dynamic behavior of a value completely at the time of declaration](https://apfelmus.nfshost.com/blog/2011/03/28-essence-frp.html).
If imperative code could be thought of as dynamic behaviour and imperative code could be somehow declared and then if imperative could be fully specified by its declaration then we could call that imperative code FRP.

Generally, full specification of the button could be done in imperative or declarative style.
By imperative style we mean monadic computation with effects, while by declarative we mean for example the [combinator pattern](https://wiki.haskell.org/Combinator_pattern).
If we specify the button fully at the time of its declaration using either imperative or declarative approach would that still be FRP?

Our proposition is that if the imperative code is based on combinator pattern then it fulfils FRP definition.

The following code would still be FRP-like even though it contains imperative specification of clear behaviour:

```
-- complete specification of imperative behavior in declarative way
clear = ...

-- complete specification of value behavior - FRP as we are used to it
name = ...

-- complete specification of value behavior - FRP as we are used to it
address = ...
```

At the risk of overloading the term push-pull FRP (originally coined by Conal Elliot) let me characterize `clear` as *push* (imperative, effectful etc.) and `name` and `address` as *pull* (declarative).

Going further I'd like to check whether it's the general rule that events can only be *pushed* and entities can only be *pulled*.
And, even further, check whether there's a one-to-one correspondence between state writes realized by *pushing* events, and state reads by *pulling* entities, so that pushed events (and only events) can change the state of the application and entites (and only entities) can be *pulled* from the state of the application

As a *cell* I introduce a stored variable we push value into and pull value from.

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

# Proof of concept

[code]
