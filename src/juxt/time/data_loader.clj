;; Copyright © 2022, JUXT LTD.

(ns juxt.time.data-loader
  (:require [juxt.time.config :as config]
            [juxt.time.adapters.csv-adapter :as csv-adapter]
            [juxt.time.adapters.edn-adapter :as edn-adapter]
            [juxt.time.adapters.ics-adapter :as ics-adapter]
            [juxt.time.holiday-calculator.converters.staff-history :as staff-hist]
            [juxt.time.holiday-calculator.converters.personal-holiday :as pers-hol]))


(defmulti load-data
  (fn [data-source] (get data-source :form)))

(defmethod load-data :csv
  [data-source]
  (csv-adapter/read-data (:file-path data-source) (get data-source :options {})))

(defmethod load-data :edn
  [data-source]
  (edn-adapter/read-data (:file-path data-source) (get data-source :options {})))

(defmethod load-data nil
  [data-source]
  (throw (ex-info "Unexpected input source form." {:data-source data-source :form (get data-source :form)})))

(defn load-public-holiday-data-from-source
  [data-source]
  (let [region (:region data-source)
        form (:form data-source)]
    (case form
      :ics (map
            #(assoc % :region region)
            (ics-adapter/read-data (:file-path data-source) (get data-source :options {})))
      (throw (ex-info "Unexpected input source form." {:data-source data-source :form form})))))

(defn load-public-holiday-inputs
  [config]
  (let [input-sources (config/input-public-holiday-sources config)]
    (->> input-sources
         (map load-public-holiday-data-from-source)
         (reduce concat))))

(defn load-personal-holiday-data-from-source
  [data-source]
  (map pers-hol/parse-fields (load-data data-source)))

(defn load-personal-holiday-inputs
  [config]
  (let [input-sources (config/input-personal-holiday-sources config)]
    (->> input-sources
         (map load-personal-holiday-data-from-source)
         (reduce concat))))

(defn load-staff-history-from-source
  [data-source]
  (map staff-hist/parse-fields (load-data data-source)))

(defn load-staff-history
  [config]
  (let [input-sources (config/input-sources config)]
    (->> input-sources
         (map load-staff-history-from-source)
         (reduce concat))))
