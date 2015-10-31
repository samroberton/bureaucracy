# bureaucracy

A library for defining composeable state machines à la [Harel
statecharts][Harel], and using them to manage — and test — the run-time state
of an application's user interface.

`bureaucracy` manages the hierarchy of faceless machines that make the (app's)
world go round.

Using `bureaucracy`, you define state machines with enumerated states,
responding to events which trigger state transitions and which effect the world
by calling transition functions on those transitions.  You describe your views
in a declarative "view tree" data structure which says "when my state machine
hierarchy matches this state, call this (pure) function to render this bit of my
view, giving it the data the state machine is managing to render".

State machines compose in two ways:

1. Substate composition: where the state of a 'parent' state machine determines
   which of several 'child' sub-machines is active, so that when you're in the
   'logged-in' state, for example, you are also are in of a discrete set of
   sub-states which is controlled by the 'child' submachine.  (Yes, at some
   point I will probably think of a decent gun joke to go with 'submachine'.  In
   the meantime, feel free to pretend I said something funny.)

2. Peer composition: where multiple active machines operate simultaneously
   without a parent/child type relationship.  (Harel calls these 'components' —
   which is a great term, except that React nicked it for something else.)

The `StateMachine` protocol is also open: if you want to define a new way to
compose state machines, you can.  (Though feel free to contribute a PR so that I
can take all the credit!)


## Getting started

[![Clojars Project](http://clojars.org/com.samroberton/bureaucracy/latest-version.svg)](http://clojars.org/com.samroberton/bureaucracy)

Besides the Rationale section below, there's a [Tutorial](doc/Tutorial.md).

There are some notes on [Testing](doc/Testing.md) which I hope are worth a
read: I test my `bureaucracy`-based application entirely on the JVM, in
`deftest` "unit" tests which exercise the UI and mock my AJAX calls to make
actual in-process calls to the actual server application in JVM Clojure.  I
think this is a powerful and simple testing mechanism.  If you've ever had
difficulty testing your user interface code (or, for those of you who haven't,
if you've ever felt uneasy about the fact that you're not testing your user
interface code), I recommend at least a quick look at the Testing page.

There are also some notes on [Debugging](doc/Debugging.md) your UIs with
`bureaucracy`'s debug view inspector.

Finally, there's some rambling rantings in the [Motivation](doc/Motivation.md)
doc, if you're really short on reading material.

### Warning: here be dragons

At this stage, `bureaucracy` is immature code.  I sincerely hope you find it
interesting and useful, and I'm using for a real project and it's working well
for me.  But this is alpha-quality, still, and the API is most definitely
subject to change.


## Rationale

Change in any user interface occurs as discrete events: the user clicks the
'shopping cart' link, the user types text into the 'username' field, the AJAX
call to retrieve search results returns this wad of data, etc.  Between those
events, your application is (or should be!) in a steady state.

`bureaucracy` is built on three theories:

1. Your UI rendering code (with its `on-change` handlers and its AJAX callbacks,
   etc) *and your tests* should be able to trigger those events in the simplest,
   most declarative (ie data-driven) way possible.  Effecting change in your
   application should be equivalent to saying: "given the current state of the
   world (`db`) and the event `:update` happening with data `:username "sam"`,
   what is the next state of the world?".  Followed by: "OK, please render this
   as the current state of the world".

2. Composing state machines is a simple and effective way to describe the
   behaviour of your application — and it encourages you to do so in a way that
   is highly declarative, and entirely independent of the way your pages render.
   `bureaucracy`'s composeable state machine model allows you to write your user
   interface code in the same way you talk about the result: "when the user is
   logged in, ..."; "the user clicks 'library' and the application does `X` and
   now displays as being in mode `Y`"; etc.  It also happens to help solve the
   question of how to create discrete (and potentially reusable) components
   which don't require full knowledge of the layout of (and coordination to
   access) the global state atom, but which also don't need cursors/lenses or
   two-way binding.  Much like a React component automatically gets local state
   whose lifecycle is managed in line with the component's, a state machine in a
   state machine hierarchy automatically defines the lifecycle of state (in the
   sense of data) that lives while the world is in the states which the machine
   manages.  But, obviously, without the icky impenetrability of React
   component-local state.

3. You can describe your entire system as a combination of (a) a state machine
   (hierarchy), (b) a single atom "database" holding the current data that
   pertains to that state machine, and (c) a data structure representing the
   view functions which should be called to produce the view given the state
   machine and the "database" (`bureaucracy` calls this data structure the "view
   tree").  If you do describe it like this, then you can write very succinct
   and very powerful tests — traditionally one of the harder things to do
   effectively in user interfaces.


## Thanks

`bureaucracy` is heavily inspired by Kevin Lynagh's talks about building UIs
with Harel statecharts.  You can also think of a lot of what this library tries
to facilitate as a slightly different take on Day 8 / Mike Thompson's excellent
[re-frame] — especially in the way `re-frame`'s dispatch mechanism is built.
Many thanks to both Kevin and Mike.


## Contributing / feedback

Pull requests and Github issues are welcome, regardless of whether they're
concrete code, or more general design feedback, questions, or random thoughts.

`bureaucracy` is still at that immature stage of its existence where nothing is
really quite bedded down yet.  If you do reach out with feedback or suggestions,
please bear with me if it takes me a while to think them over before I figure
out how to integrate them into my view of what `bureaucracy` should be.


## License

Copyright © 2015 Sam Roberton

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.



[Harel]:http://www.inf.ed.ac.uk/teaching/courses/seoc/2005_2006/resources/statecharts.pdf
[re-frame]:https://github.com/Day8/re-frame
