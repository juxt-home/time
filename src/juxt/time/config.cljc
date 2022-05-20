;; Copyright © 2022, JUXT LTD.

(ns juxt.time.config
  (:require [aero.core :as aero]))

(defn config
  [path profile]
  (aero/read-config path {:profile profile}))

(defn input-sources
  [config]
  (get-in config [:holiday-entitlement :input-sources]))

(defn filters
  [config]
  (get-in config [:holiday-entitlement :filters]))
