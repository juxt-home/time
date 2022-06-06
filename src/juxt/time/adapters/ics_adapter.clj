;; Copyright © 2022, JUXT LTD.

(ns juxt.time.adapters.ics-adapter
  (:require [tick.core :as t]
            [tick.alpha.interval :as t.i]
            [tick.alpha.ical :as ical]
            [clojure.java.io :as io]))


(defmethod ical/coerce-to-value "DURATION" [_ value] (. java.time.Duration parse value))

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
(t.i/interval "PT48H")
(t/new-duration "PT48H")

(. java.time.Duration parse "PT48H")
(t/duration "PT48H")

  )
