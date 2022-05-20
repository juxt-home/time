;; Copyright © 2022, JUXT LTD.

(ns juxt.time.scenario-test
  (:require  [clojure.test :refer [deftest testing is]]
             [tick.core :as t]
             [juxt.time.holiday-calculator.core :as sut]))

(def FULL_TIME
  {"MONDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "TUESDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "WEDNESDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "THURSDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "FRIDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}})

                                        ; scenario 1
                                        ; Employee started from GB-ENG April 2019
                                        ; Employee moved to DE-NW July 2019
                                        ; Employee contract terminated November 2019

(def scenario-1-employment-started-event {:holidays/employment-change-date #inst "2019-04-16T00:00"
                                          :juxt.home/juxtcode "rpl"
                                          :juxt.home/status "ACTIVE"
                                          :juxt.home/employment-type "EMPLOYEE"
                                          :juxt.home/working-pattern FULL_TIME
                                          :juxt.home/full-time-hours 40
                                          :juxt.home/public-holiday-region "GB-ENG"
                                          :juxt.home/holiday-entitlement 25})

(def scenario-1-change-region-event (merge
                                     scenario-1-employment-started-event
                                     {:holidays/employment-change-date #inst "2019-09-10T00:00"
                                      :juxt.home/public-holiday-region "DE-NW"}))

(def scenario-1-employment-terminated-event {:holidays/employment-change-date #inst "2019-11-30T00:00"
                                             :juxt.home/juxtcode "rpl"
                                             :juxt.home/status "TERMINATED"
                                             :juxt.home/employment-type "EMPLOYEE"})

(def scenario-1-employee-history [scenario-1-employment-started-event
                                  scenario-1-change-region-event
                                  scenario-1-employment-terminated-event])

(def scenario-1-calendar (sut/calendar {:staff-member-record-collection (sut/history->staff-member-record-collection scenario-1-employee-history)
                                        :public-holidays []
                                        :personal-holidays []
                                        :ceiling-year "2020"}))

(deftest scenario-1-test
  (testing "No records are produced from before employment started"
    (is (nil? (sut/get-record-for-date (t/date "2019-04-15") scenario-1-calendar))))

  (testing "Holidays accrue from the first day based on fraction of the year"
    (is (= 0.0685M (:balance (sut/get-record-for-date (t/date "2019-04-16") scenario-1-calendar))))
    (is (= 0.548M (:balance (sut/get-record-for-date (t/date "2019-04-23") scenario-1-calendar))))
    (is (= 1.027M (:balance (sut/get-record-for-date (t/date "2019-04-30") scenario-1-calendar))))
    (is (= 10M (:balance (sut/get-record-for-date (t/date "2019-09-08") scenario-1-calendar))))
    (is (= 15.62M (:balance (sut/get-record-for-date (t/date "2019-11-30") scenario-1-calendar)))))

  (testing "No more holiday days are accrued after employment termination"
    (is (= 15.62M (:balance (sut/get-record-for-date (t/date "2019-12-01") scenario-1-calendar))))
    (is (= 15.62M (:balance (sut/get-record-for-date (t/date "2019-12-31") scenario-1-calendar))))))
