;; Copyright © 2022, JUXT LTD.

(ns juxt.time.holiday-calculator.converters.personal-holiday
  (:require [tick.core :as t]))


(defn parse-fields
  [e]
  {:juxt.home/juxtcode (:user e)
   :juxt.home/beginning-local-date-time (t/date-time (:beginning e))
   :juxt.home/end-local-date-time (t/date-time (:end e))
   :juxt.home/description (:description e)})
