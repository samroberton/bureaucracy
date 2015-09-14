## Motivation

Imagine for a moment that mythical beast, the well-written bug report.  It's
specific and thorough, but it's concise.  It reads, for example: "when the user
is logged in, and on the 'widget details' screen, in edit mode, and creates a
new sub-widget using the 'create sub-widget' form, the server catches on fire".

That one sentence doesn't tell you absolutely everything about the application's
run-time state.  But it tells you a lot.  And yet, chances are your application
doesn't have a (simple) data structure that says "the current state is that the
user is logged in, and on the 'widget details' screen, ...".  I think it should,
and I think that the value in that data structure should drive your application.

Even if we're talking about a very complex web app with dozens upon dozens of
complicated enterprisey screens, declaratively specifying the run-time state of
the application is often possible in surprisingly concise terms.  My claim is
that driving the behaviour of your user interface from such a declarative
description will make your code simpler and easier to reason about.

Along those lines, `bureaucracy` is motivated by a few simple principles:

1.  When we describe user interfaces, we talk about them in terms that map
    naturally to a simple hierarchy of state machines.  A user is not yet logged
    in, logging in, or logged in.  When the user is logged in, they are on the
    'dashboard' screen, or the 'widget list' screen, or the 'widget view'
    screen, or the 'account profile' screen, or ...  When they're on the 'widget
    view' screen, they are viewing the details of an existing widget, or editing
    the details of an existing widget, or entering the details of a new widget
    ...  These are states.  Talking about our UIs in terms of states is succinct
    and efficient.  Implementing them in those terms might be a good idea too.

2.  The run-time state of our applications is important, and deserves to be
    carefully modelled and clearly expressed as a first-class concern.
    Application states that we name when talking about our application are worth
    naming in the code: `(when (= state :editing-widget) ...)` is clearer than a
    local `editing?` flag.

3.  The scope and lifecycle of run-time state should be determined by deliberate
    choices, not incidental consequences of implementation details.  The fact
    that the user is currently editing a widget on the 'widget edit' screen is a
    fundamental property of the current run-time state of your application.  No
    matter how you write your application, this fact is necessarily stored
    somewhere.  "There is a React component currently mounted in the DOM that is
    the component which renders the 'widget edit' screen" is not a good model
    for storing important facts.  Similarly, when you're editing widget details,
    the `{:widget-name "Frozzle", :sprocket-count 5}` map of details that backs
    the form in the UI "belongs to" the code that's managing the editing state.
    Initialising it in a React 'component-did-mount' method and having it
    disappear after 'component-did-unmount' works, but feels backwards: the view
    rendering is driving the model.

4.  The bulk of unavoidable code in a user interface is to render the view.
    This code is, however, mostly trivial (very simple logic, no side effects,
    no complicated algorithms, no coordination of state, etc), and it is not
    difficult to reason about or maintain trivial code, even when there's lots
    of it.  It *is* difficult to reason about and maintain code which is
    *mostly* trivial, but which has more complex behaviour occasionally embedded
    within it.  Thus, it is worth devoting real effort and attention to make
    sure that we don't introduce anything non-trivial into our view code ---
    like state management, or anything else that is trying to implement actual
    behaviour.

5.  State is easier to coordinate when it's all in one place and can be treated
    as a value.

6.  Encapsulation and separation of concerns are worthy goals.  However,
    "encapsulating" the run-time state of your application inside the code that
    renders its views is like encapsulating your keys down the back of the
    couch: you've put it in a new home, OK, but you've "chosen" that home
    because, well, it's gotta be somewhere, and that's kind of where you were at
    the time.

7.  Your user interface code will be lot simpler if the code that is responsible
    for rendering a form with a button doesn't have to know what's supposed to
    happen when the button gets clicked.
