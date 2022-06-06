;; Copyright © 2022, JUXT LTD.

(ns juxt.time.data-output
  (:require [juxt.time.config :as config]
            [juxt.time.adapters.csv-adapter :as csv-adapter]))

(defn filter-selected-data
  [output-form results]
  (let [selectors (:field-selectors output-form)]
    (->> results
         (map (fn [res] (map (fn [sel] (get-in res sel)) selectors))))))

(defmulti write-out
  (fn [output-form results] (get output-form :form)))

(defmethod write-out :stdout
  [output-form results]
  (doall (map println (filter-selected-data output-form results)))
  results)

(defmethod write-out :csv
  [{:keys [file-path options] :as output-form} results]
  (csv-adapter/write-data file-path (or options {}) (filter-selected-data output-form results))
  results)

(defmethod write-out nil
  [output-form _]
  (throw (ex-info "Unexpected output form." {:output-form output-form :form (get output-form :form)})))

(defn write-output
  [output-form results]
  (write-out output-form results))

(defn generate-output
  [config results]
  (let [outputs (config/outputs config)]
    (doall (map #(write-output % results) outputs))))
