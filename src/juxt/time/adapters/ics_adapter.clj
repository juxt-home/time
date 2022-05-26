(ns juxt.time.adapters.ics-adapter
  (:require [tick.core :as t]
            [tick.alpha.ical :as ical]
            [clojure.java.io :as io]))


(defn read-data
  [file-path options]
  (with-open [reader (io/reader file-path)]
    (for [event (->> reader
                     ical/parse-ical
                     first
                     ical/events)]
      {:date (t/date event)
       :name (ical/property-value event "SUMMARY")})))

(comment

(read-data "/var/tmp/england-and-wales.ics" {})
(read-data "/var/tmp/germany.ics" {})
  )
