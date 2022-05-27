(ns juxt.time.adapters.edn-adapter
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [tick.core :as t]
            ))

(defn parse-holiday-entity
  [e]
  {:juxt.home/juxtcode (:user e)
   :juxt.home/beginning-local-date-time (t/date-time (:beginning e))
   :juxt.home/end-local-date-time (t/date-time (:end e))
   :juxt.home/description (:description e)})

(defn read-personal-holidays-data
  [file-path options]
  (with-open [reader (io/reader file-path)
              pushback-reader (java.io.PushbackReader. reader)]
    (->> (edn/read pushback-reader)
         (map parse-holiday-entity)))
  )
