;; Copyright © 2022, JUXT LTD.

(ns juxt.time.adapters.csv-adapter-test
  (:require [juxt.time.adapters.csv-adapter :as sut]
            [clojure.test :refer [deftest testing is]]
            [tick.core :as t]
            [tick.alpha.interval :as t.i]))

(defn make-working-day
  [start-time end-time]
  {:juxt.home/beginning-local-time start-time
   :juxt.home/end-local-time end-time})

(def hours-9-10 (make-working-day "09:00" "10:00"))
(def hours-9-11 (make-working-day "09:00" "11:00"))

(defn local-times-to-interval
  [start-local-time end-local-time]
  (t.i/new-interval (t/time start-local-time) (t/time end-local-time)))

(defn sum-working-pattern-durations->hours
  [working-pattern]
  (/
   (t/minutes
    (reduce
     (fn [acc [_ local-times]] (t/+ acc (t/duration (local-times-to-interval
                                                       (:juxt.home/beginning-local-time local-times)
                                                       (:juxt.home/end-local-time local-times)))))
     (t/new-duration 0 :hours)
     working-pattern))
   60.0))

(deftest build-day-working-pattern-test
  (testing "Generates a day starting from 9am until 9am+hours"
    (is (= {"MONDAY" hours-9-10} (sut/build-day-working-pattern "MONDAY" 1)))
    (is (= {"TUESDAY" hours-9-10} (sut/build-day-working-pattern "TUESDAY" 1)))
    (is (= {"MONDAY" hours-9-11} (sut/build-day-working-pattern "MONDAY" 2))))

  (testing "Throws on hours <= 0"
    (is (thrown? Exception (sut/build-day-working-pattern "MONDAY" 0)))
    (is (thrown? Exception (sut/build-day-working-pattern "MONDAY" -1))))

  (testing "Throws on hours >= 15 as this would work past midnight"
    (is (thrown? Exception (sut/build-day-working-pattern "MONDAY" 15)))))


(deftest same-hours-for-five-days-test
  (let [patt-f-map {:pattern sut/same-hours-for-five-days-regex
                    :f sut/match->same-hours-for-five-days}]
    (testing "Given a string with the same number of hours for 5 days, builds as expected"
      (let [pattern (sut/execute-on-pattern-match "5 hours over 5 days" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" hours-9-10
              "WEDNESDAY" hours-9-10
              "THURSDAY" hours-9-10
              "FRIDAY" hours-9-10}
             pattern))
        (is (= 5.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "10 hours over 5 days" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-11
              "TUESDAY" hours-9-11
              "WEDNESDAY" hours-9-11
              "THURSDAY" hours-9-11
              "FRIDAY" hours-9-11}
             pattern))
        (is (= 10.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "5.5 hours over 5 days" patt-f-map)]
        (is (=
             {"MONDAY" (make-working-day "09:00" "10:06")
              "TUESDAY" (make-working-day "09:00" "10:06")
              "WEDNESDAY" (make-working-day "09:00" "10:06")
              "THURSDAY" (make-working-day "09:00" "10:06")
              "FRIDAY" (make-working-day "09:00" "10:06")}
             pattern))
        (is (= 5.5 (sum-working-pattern-durations->hours pattern)))))

    (testing "Given a string which does not match the pattern, returns nil"
      (is (nil? (sut/execute-on-pattern-match "NoMatch" patt-f-map))))))

(deftest same-hours-for-less-than-five-days-test
  (let [patt-f-map {:pattern sut/same-hours-for-less-than-five-days-regex
                    :f sut/match->same-hours-for-less-than-five-days}]
    (testing "Given a string with the same number of hours for less than 5 days, builds as expected"
      (let [pattern (sut/execute-on-pattern-match "3 hours over 3 days (1 hours Mon, Tues, Thurs)" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" hours-9-10
              "THURSDAY" hours-9-10}
             pattern))
        (is (= 3.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "3 hours over 3 days (1 hours Mon, Tues, Fri)" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" hours-9-10
              "FRIDAY" hours-9-10}
             pattern))
        (is (= 3.0 (sum-working-pattern-durations->hours pattern))))

     (let [pattern (sut/execute-on-pattern-match "4 hours over 4 days (1 hours Mon, Tues, Thurs, Fri)" patt-f-map)]
      (is (=
           {"MONDAY" hours-9-10
            "TUESDAY" hours-9-10
            "THURSDAY" hours-9-10
            "FRIDAY" hours-9-10}
           pattern))
      (is (= 4.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "6 hours over 3 days (2 hours Mon, Tues, Thurs)" patt-f-map)]
      (is (=
           {"MONDAY" hours-9-11
            "TUESDAY" hours-9-11
            "THURSDAY" hours-9-11}
           pattern))
      (is (= 6.0 (sum-working-pattern-durations->hours pattern)))))

    (let [pattern (sut/execute-on-pattern-match "3.6 hours over 3 days (1.2 hours Mon, Tues, Thurs)" patt-f-map)]
      (is (=
           {"MONDAY" (make-working-day "09:00" "10:12")
            "TUESDAY" (make-working-day "09:00" "10:12")
            "THURSDAY" (make-working-day "09:00" "10:12")}
           pattern))
      (is (= 3.6 (sum-working-pattern-durations->hours pattern))))

    (testing "Given a string which does not match the pattern, returns nil"
      (is (nil? (sut/execute-on-pattern-match "NoMatch" patt-f-map))))))

(deftest different-hours-for-different-days-test
  (let [patt-f-map {:pattern sut/different-hours-for-different-days-regex
                    :f sut/match->different-hours-for-different-days}]
    (testing "Given a string with a different number of hours for different days, builds as expected"
      (let [pattern (sut/execute-on-pattern-match "6 hours over 3 days (1 Mon, 2 Tues, 3 Thurs)" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" (make-working-day "09:00" "11:00")
              "THURSDAY" (make-working-day "09:00" "12:00")}
             pattern))
        (is (= 6.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "6 hours over 3 days (1 Mon, 2 Tues, 3 Fri)" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" (make-working-day "09:00" "11:00")
              "FRIDAY" (make-working-day "09:00" "12:00")}
             pattern))
        (is (= 6.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "7 hours over 3 days (2 Mon, 2 Tues, 3 Thurs)" patt-f-map)]
        (is (=
             {"MONDAY" (make-working-day "09:00" "11:00")
              "TUESDAY" (make-working-day "09:00" "11:00")
              "THURSDAY" (make-working-day "09:00" "12:00")}
             pattern))
        (is (= 7.0 (sum-working-pattern-durations->hours pattern))))

      (let [pattern (sut/execute-on-pattern-match "10 hours over 4 days (1 Mon, 2 Tues, 3 Thurs, 4 Fri)" patt-f-map)]
        (is (=
             {"MONDAY" hours-9-10
              "TUESDAY" (make-working-day "09:00" "11:00")
              "THURSDAY" (make-working-day "09:00" "12:00")
              "FRIDAY" (make-working-day "09:00" "13:00")}
             pattern))
        (is (= 10.0 (sum-working-pattern-durations->hours pattern))))
      )
    (testing "Given a string which does not match the pattern, returns nil"
      (is (nil? (sut/execute-on-pattern-match "NoMatch" patt-f-map))))))

(deftest working-pattern-str->working-pattern
  (let [patt-f-map [{:pattern #"(Test).*" :f #(identity {:match %})}]]
    (testing "Given a pattern-f pair is found, returns the result of calling f with match"
      (is (= {:match '(["TestPattern" "Test"])} (sut/working-pattern-str->working-pattern "TestPattern" patt-f-map))))

    (testing "Given a pattern-f pair is not found, throws exception"
      (is (thrown? Exception (sut/working-pattern-str->working-pattern "Invalid pattern" patt-f-map))))))
