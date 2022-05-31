;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.core-test
  (:require [clojure.java.io :as io]
            [juxt.time.holiday-calculator.core :as sut]
            [tick.core :as t]
            [tick.alpha.ical :as ical]
            [clojure.test :refer [deftest testing is]]))

(def full-time-work-pattern ; 40 hrs
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


(def basic-user-employment-started-event {:juxt.home/employment-change-date #inst "2019-04-01T00:00"
                                          :juxt.home/juxtcode "rpl"
                                          :juxt.home/status "ACTIVE"
                                          :juxt.home/employment-type "EMPLOYEE"
                                          :juxt.home/working-pattern full-time-work-pattern
                                          :juxt.home/full-time-hours 40
                                          :juxt.home/public-holiday-region "GB-ENG"
                                          :juxt.home/holiday-entitlement 25})

(def basic-user-change-region-event (merge
                                     basic-user-employment-started-event
                                     {:juxt.home/employment-change-date #inst "2019-09-10T00:00"
                                      :juxt.home/public-holiday-region "DE-NW"}))

(def basic-user-employment-terminated-event {:juxt.home/employment-change-date #inst "2019-11-30T00:00"
                                             :juxt.home/juxtcode "rpl"
                                             :juxt.home/status "TERMINATED"
                                             :juxt.home/employment-type "EMPLOYEE"})

(def basic-full-time-user-history [basic-user-employment-started-event])


(def christmas-day-holiday {["GB-ENG" #time/date "2019-12-25"] {:date #time/date "2019-12-25" :name "Christmas Day" :region "GB-ENG"}})
(def boxing-day-holiday {["GB-ENG" #time/date "2019-12-26"] {:date #time/date "2019-12-26" :name "Boxing Day" :region "GB-ENG"}})

(def basic-user-personal-holidays [{:juxt.home/beginning-local-date-time "2019-11-08T00:00",
                                    :juxt.home/end-local-date-time "2019-11-09T00:00",
                                    :juxt.home/description "Wedding"}])

(deftest history->staff-member-record-collection-test
   (testing "Given only one event in history, effective from date pulled from the employment-change-date"
     (is (= (-> basic-full-time-user-history
                (assoc-in [0 :juxt.home/effective-from] #inst "2019-04-01T00:00"))
            (sut/history->staff-member-record-collection basic-full-time-user-history))))

  (testing "Given multiple events in history, effective from date is pulled from employment-change-date and effective-to
 is pulled from (n+1) employement-change-date"
    (is (= (-> (conj basic-full-time-user-history basic-user-employment-terminated-event)
               (assoc-in [0 :juxt.home/effective-from] #inst "2019-04-01T00:00")
               (assoc-in [0 :juxt.home/effective-to] #inst "2019-11-30T00:00")
               (assoc-in [1 :juxt.home/effective-from] #inst "2019-11-30T00:00"))
           (sut/history->staff-member-record-collection (conj basic-full-time-user-history basic-user-employment-terminated-event)))))

  (is (= (-> (conj basic-full-time-user-history
                   basic-user-change-region-event
                   basic-user-employment-terminated-event)
             (assoc-in [0 :juxt.home/effective-from] #inst "2019-04-01T00:00")
             (assoc-in [0 :juxt.home/effective-to] #inst "2019-09-10T00:00")
             (assoc-in [1 :juxt.home/effective-from] #inst "2019-09-10T00:00")
             (assoc-in [1 :juxt.home/effective-to] #inst "2019-11-30T00:00")
             (assoc-in [2 :juxt.home/effective-from] #inst "2019-11-30T00:00"))
         (sut/history->staff-member-record-collection (conj basic-full-time-user-history
                                                            basic-user-change-region-event
                                                            basic-user-employment-terminated-event)))))


(deftest calendar-test
  (let [test-calendar (sut/calendar
                       {:staff-member-record-collection (sut/history->staff-member-record-collection basic-full-time-user-history)
                        :public-holidays (merge christmas-day-holiday boxing-day-holiday)
                        :personal-holidays [#:juxt.home {:beginning-local-date-time "2019-10-11T00:00"
                                                         :end-local-date-time "2019-10-17T00:00"
                                                         :description "Other holiday"}
                                            #:juxt.home {:beginning-local-date-time "2019-11-18T00:00"
                                                         :end-local-date-time "2019-11-19T00:00"
                                                         :description "Other holiday"}
                                            #:juxt.home {:beginning-local-date-time "2019-12-23T00:00"
                                                         :end-local-date-time "2019-12-27T00:00"
                                                         :description "Christmas overlay holiday"}]
                        :ceiling-year (t/year "2021")})]

    (testing "Returns a collection of entries with one per day from employment start to ceiling year end"
      (is (= 1006 (count test-calendar))))

    (testing "Public holidays in the assigned region are marked as public holidays"
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-25") test-calendar)))
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-26") test-calendar))))

    (testing "Non-public holidays in the assigned region are not marked as public holidays"
      (is (nil? (:public-holiday (sut/get-record-for-date (t/date "2019-12-23") test-calendar))))
      (is (nil? (:public-holiday (sut/get-record-for-date (t/date "2019-12-24") test-calendar)))))

    (testing "Personal holidays are marked as personal holidays"
      (is (:holidays (sut/get-record-for-date (t/date "2019-11-18") test-calendar)))
      (is (:holidays (sut/get-record-for-date (t/date "2019-12-23") test-calendar)))
      (is (:holidays (sut/get-record-for-date (t/date "2019-12-24") test-calendar))))

    (testing "Personal holiday over public holiday is marked only as public holiday"
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-12-25") test-calendar))))
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-25") test-calendar))))

    (testing "Holiday over non-working day (based on working pattern) are not marked as holidays"
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-10-12") test-calendar))))
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-10-13") test-calendar)))))

    (testing "When user has over 12 months continuous service, Jan 1st, balance is equal to entitlement (plus carry)"
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2021-01-01") test-calendar)))))))

  (let [user-history (assoc-in basic-full-time-user-history [0 :juxt.home/employment-change-date] #inst "2022-01-01")
        jan-1-new-user-calendar (sut/calendar
                                 {:staff-member-record-collection (sut/history->staff-member-record-collection user-history)
                                  :public-holidays []
                                  :personal-holidays []
                                  :ceiling-year (t/year "2022")})]
    (testing "When user has 0 days of continuous service, balance is zero"
      (is (zero? (int (:balance (sut/get-record-for-date (t/date "2022-01-01") jan-1-new-user-calendar))))))

    (testing "After a fraction of a year, balance is the same fraction of entitlement"
      (is (= 1 (int (:balance (sut/get-record-for-date (t/date "2022-01-15") jan-1-new-user-calendar)))))
      (is (= 2 (int (:balance (sut/get-record-for-date (t/date "2022-02-01") jan-1-new-user-calendar)))))
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2022-03-15") jan-1-new-user-calendar)))))
      (is (= 12 (int (:balance (sut/get-record-for-date (t/date "2022-07-01") jan-1-new-user-calendar))))))

    (testing "After year complete, balance is equal to entitlement"
      (is (= 25 (int (:balance (sut/get-record-for-date (t/date "2022-12-31") jan-1-new-user-calendar)))))))

  (let [user-history (assoc-in basic-full-time-user-history [0 :juxt.home/employment-change-date] #inst "2022-06-15")
        jun-15-new-user-calendar (sut/calendar
                                  {:staff-member-record-collection (sut/history->staff-member-record-collection user-history)
                                   :public-holidays []
                                   :personal-holidays []
                                   :ceiling-year (t/year "2023")})]

    (testing "When user has 0 days of continuous service, balance is zero"
      (is (zero? (int (:balance (sut/get-record-for-date (t/date "2022-06-15") jun-15-new-user-calendar))))))

    (testing "No records are produced for dates before start date"
      (is (nil? (sut/get-record-for-date (t/date "2022-06-14") jun-15-new-user-calendar))))

    (testing "After a fraction of a year, balance is the same fraction of entitlement"
      (is (= 1 (int (:balance (sut/get-record-for-date (t/date "2022-06-30") jun-15-new-user-calendar)))))
      (is (= 2 (int (:balance (sut/get-record-for-date (t/date "2022-07-15") jun-15-new-user-calendar)))))
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2022-09-01") jun-15-new-user-calendar)))))
      (is (= 12 (int (:balance (sut/get-record-for-date (t/date "2022-12-15") jun-15-new-user-calendar))))))

    (testing "When user less than 12 months continuous service, Jan 1st, balance is equal to entitlement (plus carry)"
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2023-01-01") jun-15-new-user-calendar))))))))
