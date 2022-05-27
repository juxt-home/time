;; Copyright © 2022, JUXT LTD.

(ns juxt.time.adapters.csv-adapter
  (:require [tick.core :as t]
            [tick.alpha.interval :as t.i]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-working-pattern-str
  "40 hours over 5 days")

(def same-hours-for-five-days-regex
  "Regex to match patterns such as '40 hours over 5 days'"
  #"^([\d\.]+) hours over \d+ days$")

(def same-hours-for-less-than-five-days-regex
  "Regex to match patterns such as '15 hours over 3 days (5 hours Mon, Tues, Wed)'\"
  "#"^[\d\.]+ hours over \d days \(([\d\.]+) hours ([\w\s,]+)\)$")

(def different-hours-for-different-days-regex
  "Regex to match patterns such as '15 hours over 3 days (5 Mon, 6 Tues, 4 Wed)'"
  #"([\d\.]+) (\w+)")

(defn build-day-working-pattern
  "Builds a working day pattern from 9am-9am+hours"
  [day hours]
  (let [start-time (t/time "09:00")
        end-time (t/>> start-time (t/new-duration (* 60 hours) :minutes))
        day-value (t.i/new-interval start-time end-time)]
    {day {:juxt.home/beginning-local-time (str (:tick/beginning day-value))
          :juxt.home/end-local-time (str (:tick/end day-value))}}))

(defn build-working-pattern-same-hours-every-day
  "Builds a working pattern where the same hours are applied on every day"
  ([hours-per-day]
   (build-working-pattern-same-hours-every-day ["MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY"] hours-per-day))
  ([days hours-per-day]
   (into {} (map build-day-working-pattern days (repeat hours-per-day)))))

(defn match->same-hours-for-five-days
  "Transforms a regex match for same-hours-for-five-days-regex to a working-pattern"
  [match]
  (let [hours-per-day (/ (-> match first second parse-double) 5)]
    (build-working-pattern-same-hours-every-day hours-per-day)))

(defn match->same-hours-for-less-than-five-days
  "Transforms a regex match for same-hours-for-less-than-five-days-regex to a working-pattern"
  [match]
  (let [[_ hours-str days-csv] (first match)
        days (map t/day-of-week (str/split days-csv #", "))
        day-keys (map str days)
        hours-per-day (parse-double hours-str)]
    (build-working-pattern-same-hours-every-day day-keys hours-per-day)))

(defn match->different-hours-for-different-days
  "Transforms a regex match for different-hours-for-different-days-regex to a working-pattern"
  [match]
  (let [matches (drop 2 match)]
    (into {}
          (map (fn [[_ hours-str day-str]]
                 (build-day-working-pattern
                  (str (t/day-of-week day-str))
                  (parse-double hours-str)))
               matches))))

(def pattern-fns
  "Mapping of regex to transformation-fn"
  [{:pattern same-hours-for-five-days-regex
    :f match->same-hours-for-five-days}

   {:pattern same-hours-for-less-than-five-days-regex
    :f match->same-hours-for-less-than-five-days}

   {:pattern different-hours-for-different-days-regex
    :f match->different-hours-for-different-days}])

(defn execute-on-pattern-match
  "Given the provided pattern patches, executes the provided function on the returned matches"
  [working-pattern-str {:keys [pattern f]}]
  (when-let [matches (re-seq pattern working-pattern-str)]
    (f matches)))

(defn working-pattern-str->working-pattern
  "Executes the fn for the pattern that matches the working pattern string

  Given a working pattern string and a collection of maps from pattern to fn,
  if the pattern matches, executes the mapped fn passing the regex match.

  Throws on no pattern found which matches the pattern string"
  [working-pattern-str pattern-fn-map]
  (or
   (some (partial execute-on-pattern-match working-pattern-str) pattern-fn-map)
   (throw (ex-info "Invalid working pattern string" {:input-str working-pattern-str}))))

(defn read-csv-lines
  [reader options]
  (let [csv-lines (csv/read-csv reader options)
          header-line (first csv-lines)
        header-line-to-keys (map (comp keyword #(str/replace % #"\s" "")) header-line)]
      (map (partial zipmap header-line-to-keys) (rest csv-lines)))
  )

(defn generate-working-pattern
  [working-pattern-str]
  (if (empty? working-pattern-str)
    default-working-pattern-str
    working-pattern-str))

(defn parse-fields
  [loaded-entry]
  {:juxt.home/public-holiday-region (:holiday-region loaded-entry)
   :juxt.home/status (get loaded-entry :status "ACTIVE")
   :juxt.home/working-pattern (working-pattern-str->working-pattern (generate-working-pattern (get loaded-entry :working-pattern default-working-pattern-str)) pattern-fns)
   :juxt.home/juxtcode (:juxtcode loaded-entry)
   :holidays/employment-change-date (.parse (java.text.SimpleDateFormat. "dd/MM/yyyy") (:employment-change-date loaded-entry))
   :juxt.home/holiday-entitlement (parse-double (:holiday-entitlement loaded-entry))
   :juxt.home/employment-type (get loaded-entry :employment-type "EMPLOYEE")
   :juxt.home/full-time-hours (parse-double (get loaded-entry :full-time-hours "40.00"))})

(defn read-data
  [file-path options]
  (with-open [reader (io/reader file-path)]
    (->> (read-csv-lines reader options)
         (mapv parse-fields))))

(defn write-data
  [file-path options data]
  (with-open [writer (io/writer file-path)]
    (apply csv/write-csv (conj (mapcat identity options) data writer))))
