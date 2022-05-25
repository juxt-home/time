;; Copyright © 2022, JUXT LTD.

(ns juxt.time.core
  (:require [juxt.time.config :as config]
            [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [juxt.time.adapters.csv-adapter :as csv-adapter]
            [juxt.time.holiday-calculator.core :as holiday]
            [tick.core :as t]))


(def cli-options
  [["-c" "--config-path PATH" "Path to the configuration file"
    :validate [#(and (seq %) (.exists (clojure.java.io/file %))) "Must be a path to a valid existing file"]]
   ["-p" "--config-profile PROFILE" "A profile to select from the configuration file"
    :validate [seq "Must be a non-empty string"]]
   ["-h" "--help"]])

(defn usage
  [summary-text]
  (->> ["Calculates holiday usage at a given date"
        ""
        "Usage: <program-name> [options] action"
        ""
        "Options:"
        summary-text
        ""
        "Actions:"
        "  holidays-get-status   Gets the status of holidays for staff members"
        ""
        "Please refer to the README.adoc for more information"]
       (str/join \newline)))

(defn error-msg
  [errors]
  (str "The following errors occurred whilse parsing your comment:\n\n"
       (str/join \newline errors)))

(defn validate-holiday-get-status-arguments
  [options action-args]
  (if (:config-path options)
    {:action "holiday-get-status"
     :action-args action-args
     :options options}
    {:exit-message "Configuration path must be provided for holiday-get-status action"
     :exit-code 3}))

(defn validate-args
  [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) {:exit-message (usage summary) :exit-code 0}
      errors {:exit-message (error-msg errors) :exit-code 1}
      (and (>= (count arguments) 1)
           (= "holidays-get-status" (first arguments))) (validate-holiday-get-status-arguments options (rest arguments))
      :else {:exit-message (usage summary) :exit-code 2})))

(defn load-data-from-source
  [data-source]
  (case (:form data-source)
    :csv (csv-adapter/read-data (:file-path data-source) (get data-source :options {}))
    (throw (ex-info "Unexpected input source form." {:data-source data-source :form (:form data-source)}))))

(defn filter-selected-data
  [output-form results]
  (let [selectors (:field-selectors output-form)]
    (->> results
         (map (fn [res] (map (fn [sel] (get-in res sel)) selectors))))))

(defn write-std-out
  [output-form results]
  (doall (map println (filter-selected-data output-form results)))
  results)

(defn write-csv-out
 [{:keys [file-path options] :as output-form} results]
  (csv-adapter/write-data file-path (or options {}) (filter-selected-data output-form results)))

(defn write-output
  [output-form results]
  (case (:form output-form)
    :stdout (write-std-out output-form results)
    :csv (write-csv-out output-form results)
    (throw (ex-info "Unexpected output form." {:output-form output-form :form (:form output-form)}))))

(defn generate-output
  [config results]
  (let [outputs (config/outputs config)]
    (doall (map #(write-output % results) outputs))))

(defn load-inputs
  [config]
  (let [input-sources (config/input-sources config)]
    (->> input-sources
         (map load-data-from-source)
         (reduce concat))))

(defn juxtcode-history-map->holiday-calendar
  [config m]
  (let [ceiling-year (config/ceiling-year config)
        juxtcode-record-collection-map (update-vals m holiday/history->staff-member-record-collection)]
    (map #(holiday/calendar {:staff-member-record-collection (second %)
                             :public-holidays []
                             :personal-holidays []
                             :ceiling-year (t/year ceiling-year)}) juxtcode-record-collection-map)))

(defn holidays-get-status
  [options]
  (let [config (merge (config/config (:config-path options) (:profile options)) options)
        ceiling-year (config/ceiling-year config)
        status-for-day (config/status-for-day config)]
    (->> (load-inputs config)
         (group-by :juxtcode)
         (juxtcode-history-map->holiday-calendar config)
         (map (partial holiday/get-record-for-date (t/date status-for-day)))
         (generate-output config))))

(defn -main [& args]
  (let [{:keys [action action-args options exit-message exit-code]} (validate-args args)]
    (when exit-message
      (println exit-message)
      (System/exit exit-code))
    (holidays-get-status options)))


(comment

  (holidays-get-status  {:config-path "/var/tmp/test-config.edn"})
  )
