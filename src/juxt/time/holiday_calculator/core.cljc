;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.core
  (:require [clojure.string :as str]
            #?(:clj [clojure.spec.alpha :as s])
            #?(:clj [juxt.time.holiday-calculator.specs :as sp])
            [juxt.time.holiday-calculator.datetime-utils :as dt-util]
            [juxt.time.holiday-calculator.calculations :as calc]
            [tick.core :as t]
            [tick.protocols :as p]
            [tick.alpha.interval :as t.i])
  (:import
   (java.time DayOfWeek LocalDate LocalDateTime LocalTime ZoneId)
   (java.time.format DateTimeFormatter)))

(defn get-record-for-date
  "Returns the calendar details for a given date"
  [date calendar]
  (some #(when (= date (:date %)) %) calendar))

(defn history->staff-member-record-collection
  "Return a collection of a staff member's history, with effective-from and effective-to
  dates for each entry."
  [entity-history]
  (->>
   entity-history
   (sort-by :juxt.home/employment-change-date)
   (partition 2 1 nil)
   (keep (fn [[from to]]
           (when-let [doc from]
             (cond-> (assoc doc :juxt.home/effective-from (:juxt.home/employment-change-date from))
               to (assoc :juxt.home/effective-to (:juxt.home/employment-change-date to))))))))



(defn calendar
  "Generates a collection of records for dates worked with holiday status for each day

  Transforms staff history and holidays into collection of dates between
  beginning of continuous service to end of service or ceiling year with
  records of holidays assoc."
  [{:keys [staff-member-record-collection public-holidays personal-holidays ceiling-year]}]
  (with-precision 4 :rounding java.math.MathContext/HALF_DOWN
    (->
     (let [
           ;; Since some periods are boundless, we have to say the year up to and
           ;; including which we want the report to run.


           ;; Each holiday as an interval, used below to determine if a give date
           ;; intersects one or more holidays.
           holiday-intervals (->> personal-holidays
                                  (map calc/holiday-as-tick-interval)
                                  (sort-by :tick/beginning))

           periods (calc/staff-records->periods staff-member-record-collection ceiling-year public-holidays holiday-intervals)]

       periods

       (vec (calc/generate-calendar-from-periods periods))))))

#?(:clj
   (do

     (s/def ::date t/date?)
     (s/def ::day-of-week t/day-of-week?)
     (s/def ::start-of-year? boolean?)
     (s/def ::start-of-period? boolean?)
     (s/def ::end-of-year? boolean?)
     (s/def ::usual-working-to sp/time-string-spec)
     (s/def ::usual-working-from sp/time-string-spec)
     (s/def ::closing-whole-months-accrued-since-period-beginning nat-int?)
     (s/def ::total-deductions-this-period (s/and (complement neg?) double?))
     (s/def ::value decimal?)
     (s/def ::integer int?)
     (s/def ::closing-holiday-days-accrued-since-period-beginning (s/keys :req-un [::value ::integer]))
     (s/def ::balance decimal?)
     (s/def ::carry double?)
     (s/def ::value decimal?)
     (s/def ::display float?)
     (s/def ::units string?)
     (s/def ::display-with-units string?)
     (s/def ::monthly-holiday-accrual-rate (s/keys :req-un [::value
                                                            ::display
                                                            ::units
                                                            ::display-with-units]))
     (s/def ::year-duration-in-days nat-int?)
     (s/def ::value t/period?)
     (s/def ::value decimal?)
     (s/def ::days (s/keys :req-un [::value ::units ::display-with-units]))
     (s/def ::deduction (s/keys :req-un [::value
                                         ::days]))
     (s/def ::holiday-days-accrued-over-period decimal?)
     (s/def ::period-duration-in-days nat-int?)
     (s/def ::fraction-of-year ratio?)
     (s/def ::period (s/keys :req [:tick/beginning
                                   :juxt.home/public-holiday-region
                                   :juxt.home/status
                                   :juxt.home/juxtcode
                                   :juxt.home/full-time-hours
                                   :juxt.home/employment-change-date
                                   :tick/end
                                   :juxt.home/working-pattern
                                   :juxt.home/effective-from
                                   :juxt.home/holiday-entitlement
                                   :juxt.home/employment-type]
                             :req-un [::monthly-holiday-accrual-rate
                                      ::year-duration-in-days
                                      ::deduction
                                      ::holiday-days-accrued-over-period
                                      ::period-duration-in-days
                                      ::fraction-of-year
                                      ]))
     (s/def ::holiday-result (s/keys :req-un [::date
                                              ::day-of-week
                                              ::start-of-year?
                                              ::start-of-period?
                                              ::usual-working-to
                                              ::closing-whole-months-accrued-since-period-beginning
                                              ::total-deductions-this-period
                                              ::closing-holiday-days-accrued-since-period-beginning
                                              ::end-of-year?
                                              ::balance
                                              ::usual-working-from
                                              ::period
                                              ::carry]))


     (s/def ::staff-member-record (s/coll-of (s/keys :req [:juxt.home/employment-type])))

     (s/def ::staff-member-record-collection (s/coll-of ::staff-member-record))

     (s/def ::personal-holidays (s/coll-of (s/keys :req [:juxt.home/beginning-local-date-time
                                                         :juxt.home/end-local-date-time])))

     (s/def ::date string?)
     (s/def ::name string?)
     (s/def ::region sp/public-holiday-region-spec)
     (s/def ::holiday-region-date (s/tuple ::region ::date))
     (s/def ::holiday-region-date-to-holiday-tuple (s/tuple ::holiday-region-date (s/keys :req-un [::date ::name ::region])))
     (s/def ::public-holidays (s/coll-of ::holiday-region-date-to-holiday-tuple :kind map?))

     ; This is an arbitrary range within the bounds of expectations
     (s/def ::ceiling-year (s/and string? (fn [inp] (some-> inp parse-long (#(< 1970 % 3000))))))


     (s/fdef calendar
       :args (s/keys :req-un [::personal-holidays
                              ::staff-member-record-collection
                              ::ceiling-year])
       :ret (s/coll-of ::holiday-result :kind vector? :distinct true))))
