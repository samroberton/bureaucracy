(ns bureaucracy.core-test
  (:require [bureaucracy.core :refer :all]
            [clojure.test :refer :all]))

(defn- queue [] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))

(defn- invoke-dispatch [dispatcher-path event-id & event-args]
  (let [queue-atom (atom (queue))
        dispatch   (make-dispatch-fn queue-atom dispatcher-path)]
    (apply dispatch event-id event-args)
    (first @queue-atom)))

(deftest dispatch
  (let [dispatch (partial invoke-dispatch [])]
    (is (= {:event-id        :my-event
            :event-args      ()
            :dispatcher-path []}
           (dispatch :my-event)))
    (is (= {:event-id        ::foo
            :event-args      ()
            :dispatcher-path []}
           (dispatch {:my-event ::foo} :my-event)))
    (is (= {:event-id        ::bar
            :event-args      ()
            :dispatcher-path []}
           (dispatch {::foo ::bar} {:my-event ::foo} :my-event)))
    (is (= {:event-id        ::bar
            :event-args      [1 2 3]
            :dispatcher-path []}
           (dispatch {::foo ::bar} {:my-event ::foo} :my-event 1 2 3)))))

(deftest dispatch-ignore
  (let [queue-atom (atom (queue))
        dispatch   (fn [event-id & event-args]
                     (apply (make-dispatch-fn queue-atom []) event-id event-args))]
    (is (nil? (dispatch {::my-event nil} ::my-event)))))


(defmachine login-machine
  :logged-out {::submit-login  :logging-in}
  :logging-in {::login-success :logged-in}
  :logged-in  {::logout        :logged-out})

(defmachine menu-machine
  :section-a {::section-b :section-b}
  :section-b {::section-a :section-a})

(defmachine content-machine
  :nothing     {::load-something   [:something (fn [db _]
                                                 (assoc-in db [:state-db :other-data] 42))]}
  :something   {::load-other-thing :other-thing}
  :other-thing {::clear            :nothing})

(defmachine other-thing-machine
  :loading {::loaded :loaded}
  :loaded  {::reload [:loading (fn [db _]
                                 (assoc-in db [:state-db :heinz-varieties] 57))]})

(def composite-machine
  (with-substates login-machine
    {:logged-in (peer :menu    menu-machine
                      :content (with-substates content-machine
                                 {:other-thing other-thing-machine}))}))

(def state-db
  {:state :logged-in
   :bureaucracy.core/submachine-db
   {:state :peer
    :bureaucracy.core/submachine-dbs
    {:menu    {:state :section-b}
     :content {:state      :other-thing
               :other-data 42
               :bureaucracy.core/submachine-db
               {:state           :loading
                :heinz-varieties 57}}}}})

(deftest test-get-state
  (let [dispatch-queue (atom nil)]
    (is (= state-db
           (as-> {} db
             (start composite-machine db dispatch-queue [] ::submit-login [])
             (event composite-machine db dispatch-queue [] ::login-success [])
             (event composite-machine db dispatch-queue [] ::section-b [])
             (event composite-machine db dispatch-queue [] ::load-something [])
             (event composite-machine db dispatch-queue [] ::load-other-thing [])
             (event composite-machine db dispatch-queue [] ::loaded [])
             (event composite-machine db dispatch-queue [] ::reload [])
             (:state-db db)))))
  (is (= state-db
         (get-state login-machine state-db :logged-in)))
  (is (= state-db
         (get-state login-machine state-db :*)))
  (is (= state-db
         (get-state login-machine state-db #{:logged-out :logged-in})))
  (is (= nil
         (get-state login-machine state-db #{:logged-out :logging-in})))
  (is (= {:state :logged-in}
         (get-state composite-machine state-db [:logged-in])))
  (is (= {:state :logged-in}
         (get-state composite-machine state-db [:*])))
  (is (= {:state :logged-in}
         (get-state composite-machine state-db [#{:logging-in :logged-in}])))
  (is (= nil
         (get-state composite-machine state-db [#{:logging-in :logged-out}])))
  (is (= {:state :section-b}
         (get-state composite-machine state-db [:logged-in {:menu :section-b}])))
  (is (= nil
         (get-state composite-machine state-db [:logged-in {:menu :section-a}])))
  (is (= {:state :other-thing, :other-data 42}
         (get-state composite-machine state-db [:logged-in {:content :other-thing}])))
  (is (= {:state :loading, :heinz-varieties 57}
         (get-state composite-machine state-db [:logged-in {:content :other-thing} :loading])))
  (is (= {:state :loading, :heinz-varieties 57}
         (get-state composite-machine state-db [:logged-in {:content :other-thing} :*])))
  (is (= {:state :loading, :heinz-varieties 57}
         (get-state composite-machine state-db [:logged-in {:content :other-thing} #{:loading :loaded}]))))
