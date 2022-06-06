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


(def gb-eng-christmas-day-holiday {["GB-ENG" #time/date "2019-12-25"] {:date #time/date "2019-12-25" :name "Christmas Day" :region "GB-ENG"}})
(def gb-eng-boxing-day-holiday {["GB-ENG" #time/date "2019-12-26"] {:date #time/date "2019-12-26" :name "Boxing Day" :region "GB-ENG"}})
(def gb-sct-summer-holiday {["GB-SCT" #time/date "2019-08-01"] {:date #time/date "2019-08-01" :name "Summer Bank Holiday" :region "GB-SCT"}})
(def gb-sct-christmas-day-holiday {["GB-SCT" #time/date "2019-12-25"] {:date #time/date "2019-12-25" :name "Christmas Day" :region "GB-SCT"}})

(def basic-user-personal-holidays [{:juxt.home/beginning-local-date-time "2019-11-08T00:00"
                                    :juxt.home/end-local-date-time "2019-11-09T00:00"
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

(deftest calendar-generates-records-for-every-date-test
  (let [record-collection (sut/history->staff-member-record-collection basic-full-time-user-history)
        calendar-inputs {:staff-member-record-collection record-collection
                         :ceiling-year (t/year "2021")}
        test-calendar (sut/calendar calendar-inputs)]
    (testing "Records are generated for every day from start date to end of ceiling year"
      (is (= 1006 (count test-calendar)))
      (is (nil? (sut/get-record-for-date (t/date "2019-03-31") test-calendar)))
      (is (= (t/date "2019-04-01") (:date (first test-calendar))))
      (is (= (t/date "2021-12-31") (:date (last test-calendar))))
      (is (nil? (sut/get-record-for-date (t/date "2022-01-01") test-calendar)))

      (let [updated-ceiling-year-test-calendar (sut/calendar (assoc calendar-inputs :ceiling-year (t/year "2020")))]
        (is (= 641 (count updated-ceiling-year-test-calendar)))
        (is (nil? (sut/get-record-for-date (t/date "2019-03-31") updated-ceiling-year-test-calendar)))
        (is (= (t/date "2019-04-01") (:date (first updated-ceiling-year-test-calendar))))
        (is (= (t/date "2020-12-31") (:date (last updated-ceiling-year-test-calendar))))
        (is (nil? (sut/get-record-for-date (t/date "2022-01-01") updated-ceiling-year-test-calendar))))

      (let [updated-staff-history (assoc-in basic-full-time-user-history [0 :juxt.home/employment-change-date] #inst "2019-03-01T00:00")
            updated-start-date-collection (sut/history->staff-member-record-collection updated-staff-history)
            updated-start-date-calendar (sut/calendar (assoc calendar-inputs :staff-member-record-collection updated-start-date-collection))]
        (is (= 1037 (count updated-start-date-calendar)))
        (is (nil? (sut/get-record-for-date (t/date "2019-02-28") updated-start-date-calendar)))
        (is (= (t/date "2019-03-01") (:date (first updated-start-date-calendar))))
        (is (= (t/date "2021-12-31") (:date (last updated-start-date-calendar))))
        (is (nil? (sut/get-record-for-date (t/date "2022-01-01") updated-start-date-calendar)))))))

(deftest calendar-marks-holidays-correctly-test
  (let [record-collection (sut/history->staff-member-record-collection basic-full-time-user-history)
        calendar-inputs {:staff-member-record-collection record-collection
                         :public-holidays (merge gb-eng-christmas-day-holiday
                                                 gb-eng-boxing-day-holiday
                                                 gb-sct-summer-holiday
                                                 gb-sct-christmas-day-holiday)
                         :personal-holidays [#:juxt.home {:beginning-local-date-time "2019-10-11T00:00"
                                                          :end-local-date-time "2019-10-17T00:00"
                                                          :description "Other holiday"}
                                             #:juxt.home {:beginning-local-date-time "2019-11-18T00:00"
                                                          :end-local-date-time "2019-11-19T00:00"
                                                          :description "Other holiday"}
                                             #:juxt.home {:beginning-local-date-time "2019-12-23T00:00"
                                                          :end-local-date-time "2019-12-27T00:00"
                                                          :description "Christmas overlay holiday"}]
                         :ceiling-year (t/year "2021")}]

    (testing "Public holidays in the assigned region are marked as public holidays"
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-26") (sut/calendar calendar-inputs))))
      (let [record-collection-in-gb-sct [(assoc (first record-collection) :juxt.home/public-holiday-region "GB-SCT")]]
        (is (:public-holiday (sut/get-record-for-date
                              (t/date "2019-08-01")
                              (sut/calendar (assoc calendar-inputs :staff-member-record-collection record-collection-in-gb-sct)))))))

    (testing "Public holidays not from assigned region are not marked as public holidays"
      (is (nil? (:public-holiday (sut/get-record-for-date (t/date "2019-08-01") (sut/calendar calendar-inputs))))))

    (testing "Public holidays marked in the assigned region and another are marked as public holidays"
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-25") (sut/calendar calendar-inputs)))))

    (testing "Personal holidays are marked as personal holidays"
      (is (:holidays (sut/get-record-for-date (t/date "2019-11-18") (sut/calendar calendar-inputs))))
      (is (:holidays (sut/get-record-for-date (t/date "2019-12-23") (sut/calendar calendar-inputs))))
      (is (:holidays (sut/get-record-for-date (t/date "2019-12-24") (sut/calendar calendar-inputs)))))

    (testing "Personal holiday over public holiday is marked only as public holiday"
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-12-25") (sut/calendar calendar-inputs)))))
      (is (:public-holiday (sut/get-record-for-date (t/date "2019-12-25") (sut/calendar calendar-inputs)))))

    (testing "Holiday over non-working day (based on working pattern) are not marked as holidays"
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-10-12") (sut/calendar calendar-inputs)))))
      (is (nil? (:holidays (sut/get-record-for-date (t/date "2019-10-13") (sut/calendar calendar-inputs))))))

    (testing "Days that are not holidays are not marked as public or personal holidays"
      (let [test-date (sut/get-record-for-date (t/date "2019-06-03") (sut/calendar calendar-inputs))]
        (is (nil? (:holidays test-date)))
        (is (nil? (:public-holiday (:holidays test-date))))))))

(deftest calendar-calculates-balance-test
  (let [calendar-inputs {:staff-member-record-collection (sut/history->staff-member-record-collection basic-full-time-user-history)
                         :ceiling-year (t/year "2021")}
        test-calendar (sut/calendar calendar-inputs)
        dayc-15-holidays {:juxt.home/beginning-local-date-time "2019-11-01T00:00"
                     :juxt.home/end-local-date-time "2019-11-22T00:00"
                     :description "15 days holiday taken (skipping-weekends)"}
        dayc-18-holidays {:juxt.home/beginning-local-date-time "2019-11-01T00:00"
                     :juxt.home/end-local-date-time "2019-11-27T00:00"
                     :description "18 days holiday taken (skipping weekends"}
        test-calendar-with-15-holidays-taken (sut/calendar (assoc calendar-inputs :personal-holidays [dayc-15-holidays]))
        test-calendar-with-18-holidays-taken (sut/calendar (assoc calendar-inputs :personal-holidays [dayc-18-holidays]))]

    (testing "On the staff-member's first day, balance is 0"
      (is (zero? (int (:balance (sut/get-record-for-date (t/date "2019-04-01") test-calendar))))))

    (testing "Holiday balance accrues throughout the year to (/ entitlement fraction-worked)"
      (is (= 1 (int (:balance (sut/get-record-for-date (t/date "2019-04-15") test-calendar)))))
      (is (= 9 (int (:balance (sut/get-record-for-date (t/date "2019-08-15") test-calendar)))))
      (is (= 18 (int (:balance (sut/get-record-for-date (t/date "2019-12-31") test-calendar))))))

    (testing "Holiday balance is reduced by holidays-taken"
      (is (= 3 (int (:balance (sut/get-record-for-date (t/date "2019-12-31") test-calendar-with-15-holidays-taken)))))
      (is (= 0 (int (:balance (sut/get-record-for-date (t/date "2019-12-31") test-calendar-with-18-holidays-taken))))))

    (testing "Holiday balance can go negative"
      (is (= -1 (int (:balance (sut/get-record-for-date (t/date "2019-11-27") test-calendar-with-18-holidays-taken))))))

    (testing "Jan 1st, balance is equal to 0 + carryover"
      (is (= 5 (int (:balance (sut/get-record-for-date (t/date "2020-01-01") test-calendar)))))
      (is (= 3 (int (:balance (sut/get-record-for-date (t/date "2020-01-01") test-calendar-with-15-holidays-taken)))))
      (is (= 0 (int (:balance (sut/get-record-for-date (t/date "2020-01-01") test-calendar-with-18-holidays-taken))))))

    (testing "By the end of the second year employment, balance should equal (+ entitlement carry)"
      (is (= 30 (int (:balance (sut/get-record-for-date (t/date "2020-12-31") test-calendar)))))
      (is (= 28 (int (:balance (sut/get-record-for-date (t/date "2020-12-31") test-calendar-with-15-holidays-taken)))))
      (is (= 25 (int (:balance (sut/get-record-for-date (t/date "2020-12-31") test-calendar-with-18-holidays-taken))))))))

(comment

  (defn get-start-and-end-date-for-working-day-holiday-count
    [start-date holiday-count]
    (->> (t/range start-date (t/>> start-date (t/new-period (* 2 holiday-count) :days)))
         (remove #(#{(t/day-of-week "SATURDAY") (t/day-of-week "SUNDAY")} (t/day-of-week %)))
         (take holiday-count)
         ((juxt first last))))
  )
