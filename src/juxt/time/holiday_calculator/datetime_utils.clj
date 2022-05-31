;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.datetime-utils
  (:require [tick.core :as t]
            [tick.alpha.interval :as t.i])
  (:import
   (java.time DayOfWeek LocalDate LocalDateTime LocalTime ZoneId)
   (java.time.format DateTimeFormatter)))


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

(defn ->interval [{:keys [start end]}]
  #:tick{:beginning (:tick/beginning (t.i/bounds (t/date start)))
         :end (:tick/end (t.i/bounds (t/date end)))})

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
