;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.calculations-test
  (:require  [clojure.test :refer [deftest testing is]]
             [tick.core :as t]
             [juxt.time.holiday-calculator.calculations :as sut]))

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

(def basic-full-time-staff-record [{:juxt.home/effective-from #inst "2019-04-01T00:00"
                                    :juxt.home/employment-type "EMPLOYEE"
                                    :juxt.home/status "ACTIVE"}])
(def terminated-full-time-staff-record [{:juxt.home/effective-from #inst "2019-04-01T00:00"
                                         :juxt.home/effective-to #inst "2019-11-30T00:00"
                                         :juxt.home/employment-type "EMPLOYEE"
                                         :juxt.home/status "ACTIVE"}
                                        {:juxt.home/effective-from #inst "2019-11-30T00:00"
                                         :juxt.home/employment-type "EMPLOYEE"
                                         :juxt.home/status "TERMINATED"}])

(deftest monthly-holiday-accrual-rate-test
  (testing "Calculates accrual rate for given entitlement, working pattern and full time hours for full time employees"
    (is (= 1 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern full-time-work-pattern :full-time-hours 40})))
    (is (= 1/2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 6 :working-pattern full-time-work-pattern :full-time-hours 40})))
    (is (= 2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern full-time-work-pattern :full-time-hours 20}))))

  (testing "Calculates accrual rate for given entitlement, working pattern and full time hours for part-time employees"
    (is (= 1/2 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern part-time-work-pattern :full-time-hours 40})))
    (is (= 1/4 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 6 :working-pattern part-time-work-pattern :full-time-hours 40})))
    (is (= 1 (sut/monthly-holiday-accrual-rate #:juxt.home {:holiday-entitlement 12 :working-pattern part-time-work-pattern :full-time-hours 20})))))


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
      (let [periods (-> terminated-full-time-staff-record
                        (sut/staff-records->periods (t/year "2019") [] [])
                        vec)]
        (is (= 2 (count periods)))
        (is (= (t/date-time "2019-04-01T00:00") (:tick/beginning (first periods))))
        (is (= (t/date-time "2019-11-30T00:00") (:tick/end (first periods))))
        (is (= (t/date-time "2019-11-30T00:00") (:tick/beginning (second periods))))
        (is (= (t/date-time "2020-01-01T00:00") (:tick/end (second periods))))))))
