;; Copyright © 2022, JUXT LTD.

(ns juxt.time.core-test
  (:require [clojure.java.io :as io]
            [juxt.time.core :as sut]
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

(def part-time-work-pattern ; 20 hrs
  {"MONDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "TUESDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "17:00"}
   "WEDNESDAY"
   {:juxt.home/beginning-local-time "09:00"
    :juxt.home/end-local-time "13:00"}})

(def basic-user-employment-started-event {:holidays/employment-change-date #inst "2019-04-01T00:00"
                                          :juxt.home/juxtcode "rpl"
                                          :juxt.home/status "ACTIVE"
                                          :juxt.home/employment-type "EMPLOYEE"
                                          :juxt.home/working-pattern full-time-work-pattern
                                          :juxt.home/full-time-hours 40
                                          :juxt.home/public-holiday-region "GB-ENG"
                                          :juxt.home/holiday-entitlement 25})

(def basic-user-change-region-event (merge
                                     basic-user-employment-started-event
                                     {:holidays/employment-change-date #inst "2019-09-10T00:00"
                                      :juxt.home/public-holiday-region "DE-NW"}))

(def basic-user-employment-terminated-event {:holidays/employment-change-date #inst "2019-11-30T00:00"
                                             :juxt.home/juxtcode "rpl"
                                             :juxt.home/status "TERMINATED"
                                             :juxt.home/employment-type "EMPLOYEE"})

(def basic-full-time-user-history [basic-user-employment-started-event])

(def basic-full-time-staff-record (sut/history->staff-member-record-collection basic-full-time-user-history))

(def christmas-day-holiday {["GB-ENG" #time/date "2019-12-25"] {:date #time/date "2019-12-25" :name "Christmas Day" :region "GB-ENG"}})
(def boxing-day-holiday {["GB-ENG" #time/date "2019-12-26"] {:date #time/date "2019-12-26" :name "Boxing Day" :region "GB-ENG"}})

(def basic-user-personal-holidays [{:juxt.home/beginning-local-date-time "2019-11-08T00:00",
                                    :juxt.home/end-local-date-time "2019-11-09T00:00",
                                    :juxt.home/description "Wedding"}])

(deftest monthly-holiday-accrual-rate-test
  (testing "Calculates accrual rate for given entitlement, working pattern and full time hours for full time employees"
    (is (= 1 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern full-time-work-pattern :full-time-hours 40})))
    (is (= 1/2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 6 :working-pattern full-time-work-pattern :full-time-hours 40})))
    (is (= 2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern full-time-work-pattern :full-time-hours 20}))))

  (testing "Calculates accrual rate for given entitlement, working pattern and full time hours for part-time employees"
    (is (= 1/2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern part-time-work-pattern :full-time-hours 40})))
    (is (= 1/4 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 6 :working-pattern part-time-work-pattern :full-time-hours 40})))
    (is (= 1 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern part-time-work-pattern :full-time-hours 20})))))

(deftest deduction-as-map-test
  (testing "Given duration == full time hours / 5 returns 1 day"
    (is (= 1M (get-in (sut/duration-as-map (t/new-duration 8 :hours) 40) [:days :value])))
    (is (= 1M (get-in (sut/duration-as-map (t/new-duration 4 :hours) 20) [:days :value]))))

  (testing "Given duration == full time hours / 10 returns 0.5 day"
    (is (= 0.5M (get-in (sut/duration-as-map (t/new-duration 4 :hours) 40) [:days :value])))
    (is (= 0.5M (get-in (sut/duration-as-map (t/new-duration 2 :hours) 20) [:days :value])))))

(deftest calculate-deductions-this-period-test
  (testing "Given the start of period, returns today's deduction"
    (is (zero? (sut/calculate-deductions-this-period {} {:deduction {:days {:value 0}}} true)))
    (is (= 1 (sut/calculate-deductions-this-period {} {:deduction {:days {:value 1}}} true)))
    (is (= 5 (sut/calculate-deductions-this-period {} {:deduction {:days {:value 5}}} true))))

  (testing "Given not the start of period, returns today's deduction plus yesterday's deductions-this-period"
    (is (zero? (sut/calculate-deductions-this-period {:total-of-deductions-this-period 0} {:deduction {:days {:value 0}}} false)))
    (is (= 1 (sut/calculate-deductions-this-period {:total-of-deductions-this-period 1} {:deduction {:days {:value 0}}} false)))
    (is (= 1 (sut/calculate-deductions-this-period {:total-of-deductions-this-period 0} {:deduction {:days {:value 1}}} false)))
    (is (= 3 (sut/calculate-deductions-this-period {:total-of-deductions-this-period 2} {:deduction {:days {:value 1}}} false)))))

(deftest calculate-carry-test
  (testing "Given the start of the year, a balance less than or equal to max-carry (5) and no deductions returns balance"
    (is (zero? (sut/calculate-carry {:balance 0} {:start-of-period? true :start-of-year? true})))
    (is (= 3 (sut/calculate-carry {:balance 3} {:start-of-period? true :start-of-year? true})))
    (is (= 5 (sut/calculate-carry {:balance 5} {:start-of-period? true :start-of-year? true}))))

  (testing "Given the start of the year, a balance more than max-carry (5) and no deductions returns max-carry"
    (is (= 5 (sut/calculate-carry {:balance 6} {:start-of-period? true :start-of-year? true})))
    (is (= 5 (sut/calculate-carry {:balance 7} {:start-of-period? true :start-of-year? true}))))

  (testing "Given the start of period, but not start of year returns yesterday's balance"
    (is (zero? (sut/calculate-carry {:balance 0} {:start-of-period? true :start-of-year? false})))
    (is (= 1 (sut/calculate-carry {:balance 1} {:start-of-period? true :start-of-year? false})))
    (is (= 9 (sut/calculate-carry {:balance 9} {:start-of-period? true :start-of-year? false}))))

  (testing "Given not the start of a period, returns yesterday's carry"
    (is (zero? (sut/calculate-carry {:carry 0} {:start-of-period? false :start-of-year? false})))
    (is (= 1 (sut/calculate-carry {:carry 1} {:start-of-period? false :start-of-year? false})))
    (is (= 9 (sut/calculate-carry {:carry 9} {:start-of-period? false :start-of-year? false})))))

(deftest calculate-balance-test
  (testing "Given no holidays accrued, no deductions and no carry, returns 0"
    (is (zero? (sut/calculate-balance {} 0 0))))

  (testing "Returns the accrual this period, plus the carry, minus the deductions this period"
    (is (= 1 (sut/calculate-balance {} 0 1)))
    (is (= 2 (sut/calculate-balance {} 0 2)))
    (is (= 1 (sut/calculate-balance {} 1 2)))
    (is (zero? (sut/calculate-balance {} 2 2)))
    (is (= 3 (sut/calculate-balance {:closing-holiday-days-accrued-since-period-beginning {:value 2}} 0 1)))
    (is (= 9 (sut/calculate-balance {:closing-holiday-days-accrued-since-period-beginning {:value 6}} 0 3)))
    (is (= 5 (sut/calculate-balance {:closing-holiday-days-accrued-since-period-beginning {:value 6}} 4 3)))))

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

(deftest staff-records->periods-test
  (with-precision 4 :rounding java.math.MathContext/HALF_DOWN
    (testing "Periods begin at start date to end of the year"
      (let [periods (vec (sut/staff-records->periods basic-full-time-staff-record (t/year "2019") [] []))]
        (is (= 1 (count periods)))
        (is (= (t/date-time "2019-04-01T00:00") (:tick/beginning (first periods))))
        (is (= (t/date-time "2020-01-01T00:00") (:tick/end (first periods))))))

    (testing "If ceiling year is > employee history start date, periods are split at year boundary"
      (let [periods (vec (sut/staff-records->periods basic-full-time-staff-record (t/year "2020") [] []))]
        (is (= 2 (count periods)))
        (is (= (t/date-time "2019-04-01T00:00") (:tick/beginning (first periods))))
        (is (= (t/date-time "2020-01-01T00:00") (:tick/end (first periods))))
        (is (= (t/date-time "2020-01-01T00:00") (:tick/beginning (second periods))))
        (is (= (t/date-time "2021-01-01T00:00") (:tick/end (second periods))))))

    (testing "If employee is terminated before ceiling year end, period extends to termination"
      (let [periods (-> basic-full-time-user-history
                        (conj basic-user-employment-terminated-event)
                        (sut/history->staff-member-record-collection)
                        (sut/staff-records->periods (t/year "2019") [] [])
                        vec)]
        (is (= 2 (count periods)))
        (is (= (t/date-time "2019-04-01T00:00") (:tick/beginning (first periods))))
        (is (= (t/date-time "2019-11-30T00:00") (:tick/end (first periods))))
        (is (= (t/date-time "2019-11-30T00:00") (:tick/beginning (second periods))))
        (is (= (t/date-time "2020-01-01T00:00") (:tick/end (second periods))))))))

(deftest calendar-test
  (let [test-calendar (sut/calendar
                       {:staff-member-record-collection (sut/history->staff-member-record-collection basic-full-time-user-history)
                        :public-holidays (merge christmas-day-holiday boxing-day-holiday)
                        :personal-holidays [#:juxt.home {:beginning-local-date-time "2019-11-17T00:00"
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

    (testing "When user has over 12 months continuous service, Jan 1st, balance is equal to entitlement (plus carry)"
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2021-01-01") test-calendar)))))))

  (let [user-history (assoc-in basic-full-time-user-history [0 :holidays/employment-change-date] #inst "2022-01-01")
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

  (let [user-history (assoc-in basic-full-time-user-history [0 :holidays/employment-change-date] #inst "2022-06-15")
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
