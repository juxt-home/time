(ns juxt.time.holiday-calculator.specs
  (:require [clojure.spec.alpha :as s]
            [tick.core :as t]))


(def public-holiday-region-spec (s/and string? (partial re-matches #"\w\w-\w{1,3}")))

(s/def :juxt.home/public-holiday-region public-holiday-region-spec)

(s/def :juxt.home/status #{"ACTIVE" "INACTIVE"})

(s/def :juxt.home/juxtcode (s/and string? (partial re-matches #"\w{3}")))

(s/def :juxt.home/employment-change-date inst?)

(s/def :juxt.home/holiday-entitlement (s/and double? (complement neg?)))

(s/def :juxt.home/employment-type (s/and string? #{"EMPLOYEE" "ASSOCIATE"}))

(s/def :juxt.home/full-time-hours (s/and double? pos?))

(def time-string-spec (s/and string? (partial re-matches #"[02]\d:[0-6]\d")))

(s/def :juxt.home/beginning-local-time time-string-spec)
(s/def :juxt.home/end-local-time time-string-spec)

(s/def :juxt.home/beginning-local-date-time t/date-time?)
(s/def :juxt.home/end-local-date-time t/date-time?)


(s/def ::working-pattern-entry (s/tuple
                                #{"MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY"}
                                (s/keys :req [:juxt.home/beginning-local-time
                                              :juxt.home/end-local-time])))

(s/def :juxt.home/working-pattern (s/coll-of ::working-pattern-entry :min-count 1))

(s/def :juxt.home/effective-from inst?)

(s/def :tick/beginning t/date-time?)
(s/def :tick/end t/date-time?)
