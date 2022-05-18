;; Copyright © 2022, JUXT LTD.

(ns juxt.time.core
  (:require [clojure.string :as str]
            [tick.core :as t]
            [tick.protocols :as p]
            [tick.alpha.interval :as t.i])
  (:import
   (java.time DayOfWeek LocalDate LocalDateTime LocalTime ZoneId)
   (java.time.format DateTimeFormatter)))

(defn get-record-for-date
  [date calendar]
  (some #(when (= date (:date %)) %) calendar))

(defn format-iso-local-date [local-date]
  (.format
   local-date
   DateTimeFormatter/ISO_LOCAL_DATE))

(defn date->local-date-time
  [inst]
  (assert inst)
  (LocalDateTime/ofInstant
   (.toInstant inst)
   ;; We happen to know that valid-time in XTDB is stored as UTC
   (ZoneId/of "Z")))

(defn date->local-date
  [inst]
  (assert inst)
  (LocalDate/ofInstant
   (.toInstant inst)
   ;; We happen to know that valid-time in XTDB is stored as UTC
   (ZoneId/of "Z")))

(defn date->local-date-exclusive
  "Return the java.time.LocalDate of the given #inst, but if midnight, return the
  previous day."
  [inst]
  (let [res (LocalDateTime/ofInstant
             (.toInstant inst)
             ;; We happen to know that valid-time in XTDB is stored as UTC
           (ZoneId/of "Z"))]
  (cond-> (.toLocalDate res)
    (= (.toLocalTime res) LocalTime/MIDNIGHT) (.minusDays 1))))

(defn history->staff-member-record-collection
  "Return a collection of a staff member's history, with effective-from and effective-to
  dates for each entry."
  [entity-history]
  (->>
   entity-history
   (sort-by :holidays/employment-change-date)
   (partition 2 1 nil)
   (keep (fn [[from to]]
           (when-let [doc from]
             (cond-> (assoc doc :juxt.home/effective-from (:holidays/employment-change-date from))
               to (assoc :juxt.home/effective-to (:holidays/employment-change-date to))))))))

(defn staff-member-record->interval [period ^LocalDateTime ceiling]
  (let [from (date->local-date-time (:juxt.home/effective-from period))
        to (if-let [effective-to (some-> (:juxt.home/effective-to period) date->local-date-time)]
             (if (.isBefore effective-to ceiling) effective-to ceiling) ceiling)]
    (into period (t.i/new-interval from to))))

(defn staff-member-record-collection->start-date [staff-member-record-collection]
  (some->>
   staff-member-record-collection
   ;; Find the time periods in which they were active
   (filter #(= "ACTIVE" (:juxt.home/status %)))
   ;; TODO: We should distinguish between when someone started the first time as
   ;; an EMPLOYEE and subsequent times, for people who join us, leave and then
   ;; rejoin at a later date. See 'Continuous Service Date'
   first ; first, for now.
   ;; We're using valid-time as BUSINESS time, the jury is still out as to
   ;; whether this is a good idea!
   :juxt.home/effective-from
   ;; Convert to local time, since start-dates are dates, not times.
   date->local-date))

(defn staff-member-record-collection->end-date-exclusive [staff-member-record-collection]
  (some->>
   staff-member-record-collection
   (filter #(= "TERMINATED" (:juxt.home/status %)))
   ;; We'll use 'first' for now, but we should really partition by employment-type
   first
   :juxt.home/effective-from
   ;; Convert to local time, since start-dates are dates, not times.
   date->local-date-exclusive))

;; Public functions for GraphQL

;; TODO: Unrestricted access to the database in these functions represents a
;; privilege escalation from 'schema author' to 'superuser'. Move to using the
;; 'function' feature when it is ready. Same applies to any other functions in
;; this namespace.

(defn holiday-duration-in-hours
  [{:keys [object-type object-value field-name argument-values
           db]}]
  (int
   (t/hours
    (t/duration
     (t.i/new-interval
      (t/date-time (:juxt.home/beginning-local-date-time object-value))
      (t/date-time (:juxt.home/end-local-date-time object-value)))))))

(defn working-pattern->duration [pattern]
  (->> pattern
       vals
       (map (fn [{:juxt.home/keys [beginning-local-time end-local-time]}]
              (t.i/new-interval beginning-local-time end-local-time)))
       (map t/duration)
       (apply t/+)))

(defn monthly-holiday-accrual-rate
  "Return the rate, in days, at which holiday is accrued per month.

  If the staff record is missing either holiday entitlement, working pattern or
  full time hours, returns nil.

  Part time workers recieve pro-rata holidays."
  [{:juxt.home/keys [holiday-entitlement working-pattern full-time-hours]}]
  (when (and holiday-entitlement working-pattern full-time-hours)
    (*
     holiday-entitlement
     ;; pro-rata for part-time workers
     (/ (.toHours (working-pattern->duration working-pattern)) full-time-hours)
     ;; divide by 12, to get to the monthly rate
     (/ 1 12))))

(defn ->interval [{:keys [start end]}]
  #:tick{:beginning (:tick/beginning (t.i/bounds (t/date start)))
         :end (:tick/end (t.i/bounds (t/date end)))})

(defn ->holiday-intervals
  "Multirange of users holidays"
  [holidays]
  (map ->interval holidays))

(defn ->year-interval
  "Single year in a multirange"
  [year]
  (t.i/bounds (t/year year)))

(defn get-this-years-holiday
  "Time off in a given period = holidays ∩ year"
  [year-interval holiday-intervals]
  (t.i/intersection [year-interval] holiday-intervals))

(defn get-bank-holidays-in-year
  "Bankholidays falling within the year = bank-holidays ∩ year"
  [year-interval bank-holidays]
  (t.i/intersection [year-interval] (map ->interval bank-holidays)))

(defn weekend?
  "Is the ZonedDateTime during the weekend?"
  [dt]
  (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (p/day-of-week dt)))

(defn get-working-days
  "Working days:
  P = year interval
  B = Bankholidays in P
  E = Weekends in P
  W = P - (E ∪ B)"
  [year-interval bankholidays]
  (let [weekends (map t.i/bounds
                      (filter weekend?
                              (t.i/divide-by t/date
                                             year-interval)))]
    (t.i/difference [year-interval] weekends bankholidays)))

(defn get-actual-years-holidays
  "Holidays taken H = holidays for the year ∩ working days in the year"
  [holiday-for-year working-days]
  (t.i/intersection holiday-for-year working-days))

(defn get-years-of-holiday
  "Calculates the years where there are holidays taken"
  [holiday-intervals]
  (distinct
   (mapcat (fn [{:tick/keys [beginning end]}]
             [(t/year beginning)
              (t/year end)])
           holiday-intervals)))

(defn holiday-as-tick-interval
  [{:juxt.home/keys [beginning-local-date-time end-local-date-time] :as m}]
  (assert beginning-local-date-time)
  (assert end-local-date-time)
  (assoc m
         :tick/beginning (t/date-time beginning-local-date-time)
         :tick/end (t/date-time end-local-date-time)))


(defn round-half-down [dec]
  (.setScale dec 0 java.math.RoundingMode/HALF_DOWN))

(defn days-as-map [value]
  {:value value
   :units "days"
   :display-with-units (format "%02.1f days" (float value))})

(defn duration-as-map
  "Represent the duration in working-days, based up the number of
  working hours in a week (full-time-hours).

  Assumes 5 working days per week."
  [duration full-time-hours]
  {:value duration
   ;; TODO: Should use locale as a dynamic var
   :days (let [value (bigdec (/ (.toHours duration) (/ full-time-hours 5)))]
           (days-as-map value))
   #_:hours #_(let [value (.toHours deduction)]
                {:value value
                 :units "hours"
                 :display-with-units (format "%d hours" value)})})

(defn to-displayable-float [m]
  (float (.round m (java.math.MathContext. 4))))

(defn calculate-carry
  "Calculates the amount of holiday carried over to today.

  When start-of-year? is true, a maximum of 5 days will be carried over"
  [yesterday {:keys [start-of-period? start-of-year?]}]
  (if start-of-period?
    (if start-of-year?
      ;; New year is special, because we strike out the
      ;; balance (except for up to 5 holidays).
      (min (:balance yesterday) 5)
      (:balance yesterday))
    (:carry yesterday)))

(defn calculate-balance
  "Calculates the balance of holidays for 'today'."
  [today deductions-this-period carry]
  (let [accrual-this-period (get-in
                             today
                             [:closing-holiday-days-accrued-since-period-beginning :value]
                             0)]
    (+
     accrual-this-period
     (- deductions-this-period)
     carry)))

(defn calculate-deductions-this-period
  "Calculates the number of deductions since the beginning of this period"
  [yesterday today start-of-period?]
  (let [today-deduction (get-in today [:deduction :days :value] 0)]
    (if start-of-period?
      today-deduction
      (+
       (:total-of-deductions-this-period yesterday)
       today-deduction))))

(defn calculate-details-for-date
  "Generates today's balance details from yesterday's balance details"
  [yesterday {:keys [date start-of-period?] :as today}]
  (let [start-of-year? (= (.getDayOfYear date) 1)
        end-of-year? (= (.getDayOfYear date) (.length (t/year date)))
        start-of-period? (true? start-of-period?)

        todays-total-of-deductions-this-period
       (calculate-deductions-this-period yesterday today start-of-period?)

        carry (calculate-carry yesterday {:start-of-period? start-of-period? :start-of-year? start-of-year?})

        todays-balance (calculate-balance
                        today
                        todays-total-of-deductions-this-period
                        carry)]

    (assoc today
           :total-of-deductions-this-period todays-total-of-deductions-this-period
           :carry carry
           :balance todays-balance
           :start-of-year? start-of-year?
           :end-of-year? end-of-year?
           :start-of-period? start-of-period?)))

(defn generate-calendar-from-periods
  "Generates a collection of records for dates worked with holiday status for each day

  Associate holiday balances by reducing through all the dates."
  [periods]
  (->>
   (for [period periods
         :let [dates (:dates period)]
         date (cons
               (assoc (first dates) :start-of-period? true)
               (next dates))]
     (assoc date :period (dissoc period :dates)))

   (reductions
    calculate-details-for-date

    ;; Initial seed
    {:total-of-deductions-this-period 0
     :balance 0
     :carry 0})

   ;; skip over seed
   next))

(defn staff-records->periods
  "Transform staff-records to periods (adding additional split at year boundaries)"
  [staff-member-record-collection ceiling-year public-holidays holiday-intervals]
  (for [staff-record
        (filter #(= (:juxt.home/employment-type %) "EMPLOYEE") staff-member-record-collection)                       ;; only periods where holiday is a benefit
        :let [staff-record-interval (staff-member-record->interval staff-record (t/end ceiling-year))
              monthly-holiday-accrual-rate (some-> staff-record monthly-holiday-accrual-rate bigdec)
              working-pattern (:juxt.home/working-pattern staff-record)
              full-time-hours (:juxt.home/full-time-hours staff-record)]
        days-in-year (t/range (t/year (t/beginning staff-record-interval))
                              (t/inc (t/year (t/end staff-record-interval))))
        ;; intersect the staff-record period with the year, so that
        ;; staff record periods are split at year boundaries
        period (t.i/intersection [staff-record-interval] [days-in-year])]

    (let [period-duration-in-days (t/days (t/duration period))
          year-duration-in-days (t/days (t/duration days-in-year))
          fraction-of-year (/ period-duration-in-days year-duration-in-days)
          dates
          (for [date (t.i/divide-by t/date period)]
            (let [ ;; How much holiday have we accrued across the year?

                  ;; 'opening' means 'at the beginning of the day',
                  ;; while 'closing' means 'at the end of the day'

                  closing-whole-months-accrued-since-period-beginning
                  (t/months (t/between (t/date (t/beginning period)) (t/end date)))

                  closing-holiday-days-accrued-since-period-beginning
                  (when monthly-holiday-accrual-rate
                    (* monthly-holiday-accrual-rate 12
                       (t/divide (t/duration
                                  (t.i/new-interval (t/date (t/beginning period)) (t/end date)))
                                 (t/duration (t/year date)))))

                  working-hours (get working-pattern (str (.getDayOfWeek date)))
                  working-interval
                  (when working-hours
                    {:tick/beginning (LocalDateTime/of date (LocalTime/parse (:juxt.home/beginning-local-time working-hours)))
                     :tick/end (LocalDateTime/of date (LocalTime/parse (:juxt.home/end-local-time working-hours)))})

                  public-holiday
                  (get public-holidays [(:juxt.home/public-holiday-region staff-record) date])

                  relevant-holidays
                  (when (and (not public-holiday) working-interval)
                    (keep (fn [hol] (seq (t.i/intersection [hol] [working-interval]))) holiday-intervals))

                  deduction
                  (when (seq relevant-holidays)
                    (apply t/+ (map t/duration (apply t.i/union relevant-holidays))))

                  holidays
                  (->> relevant-holidays
                       (mapcat seq)
                       (mapv (fn [x] (dissoc x :tick/beginning :tick/end))))]

              (cond-> {:date date
                       :day-of-week
                                {:value (.getDayOfWeek date)
                                 :display (.getDisplayName
                                           (.getDayOfWeek date) java.time.format.TextStyle/FULL_STANDALONE
                                           ;; TODO: Could make this a dynamic var binding?
                                           (java.util.Locale/getDefault))}

                       :closing-whole-months-accrued-since-period-beginning
                       closing-whole-months-accrued-since-period-beginning

                       :closing-holiday-days-accrued-since-period-beginning
                       (when closing-holiday-days-accrued-since-period-beginning
                         {:value closing-holiday-days-accrued-since-period-beginning
                          :integer (int (round-half-down closing-holiday-days-accrued-since-period-beginning))})}

                deduction (assoc :deduction (duration-as-map deduction full-time-hours))

                public-holiday (assoc :public-holiday (:name public-holiday))

                (and (not public-holiday) working-interval)
                (assoc :usual-working-from (str (.toLocalTime (:tick/beginning working-interval)))
                       :usual-working-to (str (.toLocalTime (:tick/end working-interval))))

                (seq holidays)
                (assoc :holidays (mapv (fn [x] (dissoc x :tick/beginning :tick/end)) holidays)
                       :holiday-description
                       (->> holidays
                            (map :juxt.home/description)
                            (str/join ", "))
                       :holiday-dates
                       (->>
                        (for [h holidays]
                          (format "%s – %s"
                                  (:juxt.home/beginning-local-date-time h)
                                  (:juxt.home/end-local-date-time h)))
                        (str/join ", "))))))]

      (assoc period
             :dates (for [date dates] (assoc date :period period))
             :period-duration-in-days period-duration-in-days
             :year-duration-in-days year-duration-in-days
             :fraction-of-year fraction-of-year
             :holiday-days-accrued-over-period
             (when monthly-holiday-accrual-rate
               (* fraction-of-year monthly-holiday-accrual-rate 12))
             :monthly-holiday-accrual-rate
             (when monthly-holiday-accrual-rate
               {:value monthly-holiday-accrual-rate
                :display (to-displayable-float monthly-holiday-accrual-rate)
                :units "days per month"
                :display-with-units
                (format "%s days per month" (to-displayable-float monthly-holiday-accrual-rate))})
             :deduction (when (:juxt.home/full-time-hours period)
                          (duration-as-map (apply t/+ (keep (comp :value :deduction) dates))
                                           (:juxt.home/full-time-hours period)))))))

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
                                  (map holiday-as-tick-interval)
                                  (sort-by :tick/beginning))

           periods (staff-records->periods staff-member-record-collection ceiling-year public-holidays holiday-intervals)]

       periods

       (vec (generate-calendar-from-periods periods))))))
