## The Basics

Here is a simple state machine for a user login:

```clojure
(ns bureaucracy.readme
  (:require [bureaucracy.core :as bcy]))

(bcy/defmachine login-machine
  :login-required {::submit  :logging-in}
  :logging-in     {::success :logged-in
                   ::failure :login-required}
  :logged-in      {::logout  :login-required})
```

It has three states: `:login-required`, `:logging-in`, and `:logged-in`.  In
`bureaucracy`, states are represented by keywords.

When you are in the `:login-required` state, a `::submit` event will cause you
to transition to the `:logging-in` state.  Events are represented by keywords as
well.  (You don't have to, but I recommend bare keywords for states and
namespaced keywords for events.)

State machines are a bit useless without state, so we're going to need some of
that.

```clojure
(def db (atom {:app-db   {}
               :state-db {:state :login-required}}))
```

`bureaucracy` always expects there to be a `db` value with an `:app-db` and a
`:state-db`.  We're putting the value in an atom so that we can change it over
time, but `bureaucracy` always operates on it as a pure value, taking it as an
argument, and returning the new value as a result.

For the moment, just ignore `:app-db`.

Now we can manually poke at the `login` state machine like this:

```clojure
(bcy/event login-machine @db nil nil ::submit nil)
;=> {:app-db {}, :state-db {:state :logging-in}}
```

> And hooray!  Look Mummy, I did a program!

But obviously this is not a very useful login process.  After all, we didn't
actually collect a username and password, and we didn't post anything to the
server to actually do the login.  We can do better.

```clojure
;; Assume we have an AJAX method defined somewhere else with this signature.
(defn ajax-post [url post-body success-callback error-callback]
  (println "Posting" (pr-str post-body) "to url" url))

;; Here we're going to define some transition functions — these are where your
;; logic goes.

(defn update-state [db _ k v]
  (assoc-in db [:state-db k] v))

(defn submit-form [{:keys [state-db] :as db} dispatch]
  (ajax-post "/auth"
             (select-keys state-db [:username :password])
             #(dispatch ::success %)
             #(dispatch ::failure %))
  db)

(defn success [db _ ajax-response]
  (-> db
      (update :app-db assoc :auth ajax-response)
      (update :state-db dissoc :username :password)))

(bcy/defmachine login-machine
  :login-required {::update     [:login-required update-state]
                   ::submit     [:logging-in     submit-form]
                   ::logged-out :login-required}
  :logging-in     {::success    [:logged-in      success]
                   ::failure    :login-required}
  :logged-in      {::logout     :login-required})
```

Not a very complex state machine, still, but enough to do something useful.

```clojure
;; Ok, let's enter a username. Ignore the nils for now, and read this as having
;; rest-args, like:  (do-things ::update :username "sam")
(reset! db (bcy/event login-machine @db nil nil ::update '(:username "sam")))
;=> {:app-db {}, :state-db {:state :login-required, :username "sam"}}

;; Encore!
(reset! db (bcy/event login-machine @db nil nil ::update '(:password "pa$$w0rd")))
;=> {:app-db {}, :state-db {:state :login-required, :username "sam", :password "pa$$w0rd"}}

;; Submit the form — this is going to call our dummy ajax-post function.
(reset! db (bcy/event login-machine @db nil nil ::submit '()))
;<> Posting {:username "sam", :password "pa$$w0rd"} to url /auth
;=> {:app-db {}, :state-db {:state :logging-in, :username "sam", :password "pa$$w0rd"}}

;; And let's manually execute the AJAX success callback ourselves.
(reset! db (bcy/event login-machine @db nil nil ::success '({:auth-token "awesome"})))
;=> {:app-db {:auth {:auth-token "awesome"}}, :state-db {:state :logged-in}}
```

There's a bit going on in the code above, but hopefully after looking at it for
a little while, you'll agree that each individual part is pretty simple.

`update-state`, `submit-form` and `success` are transition functions. Transition
functions are optional: you specify them when you define the machine by giving a
vector `[:new-state transition-fn]` as the value in the event map, instead of
the bare keyword.  The old machine just changed its state when we gave it a
`::submit` event. The new one changes state too, but also calls the
`submit-form` transition function.

Transition functions are called automatically by the state machine: the caller
doesn't know anything about them.  (The caller just says "hey, event `::submit`
is happening, guys and girls — take care of that, would you?")  The state
machine calls them with at least two arguments: the `db` value, and a `dispatch`
function which they can use as a callback if they need to (most won't).
Additionally, they'll be given any other arguments you supply as the
`event-args` (final) parameter when you call the `event` method — as we do above
with the `::update` event.

If you squint real hard, you might also be able to identify in the above the
distinction between the `app-db` part of the `db` value and the `state-db`: the
`state-db` is the data that's associated with the current state of the state
machine; `app-db` is more global.  The `:username` and `:password` values are
pretty intrinstic to the mechanism of logging in; once you're logged in, the
auth token you get back is going to be needed by more than just the
`login-machine`.


## Composition: sub-machines and sub-states

> Cute enough, but I was pretty good at managing logging in before ...

Fair point.  But what happens after login?

What we haven't covered so far at all is the composeable, hierarchical aspects
of `bureaucracy`.  Let's say for the moment that once you're past the login
form, you get taken to a widget screen, where you can view, edit or delete a
widget.

```clojure
(defn init-widget [db & _]
  (assoc-in db [:state-db :widget] {:widget-name "Frozzle", :sprocket-count 5}))

(defn update-widget-state [db _ k v]
  (assoc-in db [:state-db :widget k] v))

(defn save-widget [db _]
  (assoc-in db [:app-db :saved-widget] (get-in db [:state-db :widget])))

(bcy/defmachine widget-machine
  :viewing  {bcy/enter       [:viewing init-widget]
             ::edit-widget   :editing
             ::delete-widget :deleting}
  :editing  {::update-widget [:editing update-widget-state]
             ::cancel-widget :viewing
             ::submit-widget [:viewing save-widget]}
  :deleting {::cancel-widget :viewing})

(def composed-machine
  (bcy/with-substates login-machine
    {:logged-in widget-machine}))
```

Like it says on the tin, this is a composed state machine.  It behaves exactly
like `login-machine`, except that when you enter the `:logged-in` state (on a
`::submit` event), then the `widget-machine` state machine also comes into play.

```clojure
(reset! db (event composed-machine @db queue-atom [] ::success [{:auth-token "awesome"}]))
;=> {:app-db   {:auth {:auth-token "awesome"}}
;    :state-db {:state :logged-in
;               :bureaucracy.core/submachine-db {:state  :viewing
;                                                :widget {:widget-name   "Frozzle"
;                                                         :sprocket-count 5}}}}

(reset! db (event composed-machine @db queue-atom [] ::edit-widget []))
;=> {:app-db   {:auth {:auth-token "awesome"}}
;    :state-db {:state :logged-in
;               :bureaucracy.core/submachine-db {:state  :editing
;                                                :widget {:widget-name   "Frozzle"
;                                                         :sprocket-count 5}}}}

(reset! db (event composed-machine @db queue-atom [] ::update-widget [:widget-name "Blurp"]))
;=> {:app-db   {:auth {:auth-token "awesome"}}
;    :state-db {:state :logged-in
;               :bureaucracy.core/submachine-db {:state  :editing
;                                                :widget {:widget-name    "Blurp"
;                                                         :sprocket-count 5}}}}

(reset! db (event composed-machine @db queue-atom [] ::submit-widget nil))
(reset! db (event composed-machine @db queue-atom [] ::logout nil))
;=> {:app-db   {:saved-widget {:widget-name    "Blurp"
;                              :sprocket-count 5}}
;    :state-db {:state :login-required}}
```

It's worth mentioning at this point that neither `login-machine` nor
`widget-machine` had to change, or be implemented in any special way, to enable
this to happen.

There are only two new things here:

1.  `with-substates` allows you to compose machines, so that a 'child' machine
    applies when the 'parent' machine is in a particular state.

2.  `bureaucracy.core/enter` is a 'special' event recognised by `defmachine`;
    you can use it to specify a transition function which should be run when the
    state machine starts.

(Notice also that to avoid name collisions, I've named the events for the
`widget-machine` with `-widget` suffices, like `::update-widget`.  This name
collision is why it's generally a good idea to namespace-qualify your event
keywords.)

## More composition: peers

There's another form of composition available.  [Harel], in his description of
Statecharts, refers to them as 'components'.  That's already a heavily
overloaded term in web UIs thanks to React, so I've chosen to call them 'peers'
instead.  Basically, a 'peers' machine is a composition of two sibling machines.
For example, on the 'widget' screen, there might be a left sidebar for the
'widget list', which might have its own `widget-list-machine`, and a right
sidebar for the 'widget view', with the `widget-machine` we've already
defined. Those would both be children of the `:logged-in` state, but the
`widget-machine` is not really a child of the `widget-list-machine`.

```clojure
(def composed-machine
  (bcy/with-substates login-machine
    {:logged-in (bcy/peer :widget-list widget-list-machine
                          :widget-view widget-machine)}))
```

Between the 'unit' machine that you get from `defmachine` and the composeability
you get from `with-substates` and `peer`, I think there's enough to model a
complete user interface no matter how complex it is, and to manage all that
run-time completely separately from your view functions.

## Views

> OK, so that was fancy and all but like I said before, I was pretty good at
  writing these forms before ...

Again, fair point.

Only, notice that so far we actually haven't written a login form or a widget
form.  We've implemented the *behaviour* behind the forms — enough that we can
test what our app actually does when users poke it — but we haven't rendered
anything.  (If you've ever had a disagreement with Selenium, now is an
acceptable time to start getting excited.)

In any case, given a map that looks like `{:state :login-required, :username
"sam", :password "pa$$w0rd"}`, I'm pretty confident that we can manage to write
a function that spits out whatever HTML we might want to render for the login
form.  But there's one important question: how does that HTML then do stuff?
When you type your name in the 'username' box, how does that get into the state
machine?  Because, frankly, if it involves an on-change handler that looks
anything like `#(reset!  db (event login-machine @db nil nil ::update
'(:username "sam")))`, then ... thanks, but no thanks.

Of course the answer is that it doesn't.  We want our view functions to produce
on-change and on-click and on-new-javascript-framework-invented handlers that
affect our state machine, but we don't want them to do any more than the bare
minimum.

So what's the bare minimum?  Well, stealing pretty directly from the
aforementionedly excellent [re-frame], how about this for a button's on-click
handler?  `#(dispatch ::submit)`

Now our view doesn't need to know what happens when the button gets clicked.  It
just needs to know that whatever happens goes by the name `::submit`.  For the
username text input's on-change?  `#(dispatch ::update :username %)`

Cool.  We saw `dispatch` in the callback functions we passed to our hypothetical
`ajax-post` function, too.  What the hell is it?  In re-frame, it's a global
`defn`, in `bureaucracy`, it's a function you get given as a parameter whenever
you might need to be able to affect the application's state asynchronously.
More simply, though: it's just a callback.  When we played with the `login`
machine above, we cheated a bit, as you might have guessed from the two `nil`
parameters to `event` that I quietly didn't mention every time we invoked the
state machine.  What you're actually supposed to pass in there are a queue atom
and a path.

```clojure
;; CLJS has #queue -- on the JVM, use (clojure.lang.PersistentQueue/EMPTY).
(def dispatch-queue (atom #queue []))

(reset! db (bcy/event login-machine @db dispatch-queue [] ::submit nil))
```

`bureaucracy` will use `(bcy/make-dispatch-fn dispatch-queue dispatch-path)` to
get a `dispatch` function to give your transition function: as a result, when
the `::submit` event causes the AJAX call to happen, the `(dispatch ::success)`
in the AJAX success callback will put an event on the dispatch queue.

If you run this with an actual AJAX mechanism, you'll see that after the
callback is called, `dispatch-queue` will contain an event, ready for us to read
off and hand to the state machine.  (In practice, we'd do this with
`consume-dispatch-queue`: more on that later.)

Our UI handlers use exactly the same mechanism: we'll see exactly how this works
later on, but for now, it's sufficient to know that when we define a function
that renders a view, we should have it take `dispatch` as a parameter, and
implement all its event handlers by calling `(dispatch ::some-event
&optional-event-args)`.


## View hierarchies

Speaking of which, we *still* haven't actually rendered a damned view yet...

Given that state is out of the way (managed by our state machines), the most
annoying problem I find writing user interface code is cleanly writing the logic
to make sure that the right components get rendered on the page with the right
data.  Writing my view functions is easily: after all, they're trivial.  Making
sure I wire everything up to call them at the right time with the right
parameters — and doing it in a way that I can understand when I come back to
it a week later — is the hard part.

This is where the `bureaucracy.view` namespace comes in.  `bureaucracy.core`
manages our state:  `bureaucracy.view` uses it to decide what views to render.

```clojure
(def the-view
  [{:state [#{:login-required :logging-in :login-failed}]
    :view  login/render}

   {:state    [:logged-in]
    :view     widget/render-list-and-widget
    :subviews {:list   [{:state [:logged-in {:widget-list :*}]
                         :view  widget/render-list}]
               :widget [{:state [:logged-in {:widget-view #{:viewing :editing}}]
                         :view  widget/render-widget}
                        {:state [:logged-in {:widget-view #{:deleting}}]
                         :view  widget/render-widget-delete}]}}])

(bureaucracy.view/render-view the-view composed-machine @db dispatch-queue)
```

TODO Explain view hierarchies, `describe-state`, and `get-state`.
