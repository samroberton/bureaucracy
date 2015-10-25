# Testing

One of the main goals of `bureaucracy` is to make integration testing of UIs
easier and more effective.

Here is a test from a personal project of mine:

```
(deftest view-exercise-test
  (let [{:keys [db dispatcher state-machine]} *system*]
    (login *system*)
    ((dispatcher :nanny.web-client.app-tabs/exercise-library))
    (bcy-test/consume *system*)
    (bcy-test/render *system*)

    (is (not-empty (first (:exercises (:app-db @db)))))
    ((dispatcher :nanny.web-client.exercise-library/select (first (:exercises (:app-db @db)))))
    (bcy-test/consume *system*)
    (bcy-test/render *system*)

    (is (not-empty (first (:exercises (:app-db @db)))))
    ((dispatcher :nanny.web-client.exercise-library/select (first (:exercises (:app-db @db)))))
    (bcy-test/consume *system*)
    (bcy-test/render *system*)

    (is (= (:state-db (bcy/get-path state-machine
                                    (:state-db @db)
                                    [:logged-in
                                     {:user-mode :teacher}
                                     :exercise-library
                                     {:selected-exercise :existing}]))
           {:state :viewing
            :exercise (assoc (first (:exercises (:app-db @db))) :cards [])}))))
```

This test lives in a `.cljc` file.  Although my user interface runs in
ClojureScript, this test runs in the JVM.  Where the ClojureScript version would
make an AJAX call, it uses `ring.mock` to call the actual server code
in-process, running against an actual Postgres database (each test runs in a
single JDBC transaction which is rolled back at the end of the test).

This test simulates a user logging in with a username and password, triggers all
the various AJAX calls that a real user login triggers, selects the 'exercise
library' tab from the page's navigation bar, and selects the first exercise from
the list of exercises that the post-login AJAX call populated into the UI app's
`db` atom.  Note that the `render` calls are actually completely unnecessary for
the functionality being tested: they're there just for the sake of it, to verify
that rendering the page doesn't trigger any exceptions.

The `((dispatcher :nanny.web-client.login/update :username) "unittest")` call in
the test looks like that because it's invoking `dispatcher` in exactly the way
the view could would, when the view wants to produce a JavaScript `on-change`
event handler for the username text field.
