;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.datetime-utils-test
  (:require [juxt.time.holiday-calculator.datetime-utils :as sut]
            [tick.core :as t]
            [clojure.test :refer [deftest testing is] ]))

(deftest deration-as-map-test
  (testing "Given duration == full time hours / 5 returns 1 day"
    (is (= 1M (get-in (sut/duration-as-map (t/new-duration 8 :hours) 40) [:days :value])))
    (is (= 1M (get-in (sut/duration-as-map (t/new-duration 4 :hours) 20) [:days :value]))))

  (testing "Given duration == full time hours / 10 returns 0.5 day"
    (is (= 0.5M (get-in (sut/duration-as-map (t/new-duration 4 :hours) 40) [:days :value])))
    (is (= 0.5M (get-in (sut/duration-as-map (t/new-duration 2 :hours) 20) [:days :value])))))
