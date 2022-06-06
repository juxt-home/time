;; Copyright © 2022, JUXT LTD.

(ns juxt.time.core
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :as cli]
   [juxt.time.config :as config]
   [juxt.time.holiday-calculator.core :as holiday]
   [juxt.time.data-loader :as data-loader]
   [juxt.time.data-output :as data-output]
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


(defn juxtcode-history-map->holiday-calendar
  [ceiling-year public-holidays [juxtcode {:keys [staff-history personal-holidays]}]]
  (holiday/calendar {:staff-member-record-collection (holiday/history->staff-member-record-collection staff-history)
                     :public-holidays public-holidays
                     :personal-holidays personal-holidays
                     :ceiling-year (t/year ceiling-year)}))

(defn load-input-data
  [config]
  (let [status-for-day (config/status-for-day config)
        personal-holidays (data-loader/load-personal-holiday-inputs config)
        filtered-personal-holidays (filter #(t/< (t/date (:juxt.home/end-local-date-time %)) (t/date status-for-day)) personal-holidays)
        personal-holidays-by-juxt-code (group-by :juxt.home/juxtcode filtered-personal-holidays)
        staff-history (data-loader/load-staff-history config)
        filtered-staff-history (filter #(t/< (t/date (:juxt.home/employment-change-date %)) (t/date status-for-day)) staff-history)
        staff-history-by-juxt-code (group-by :juxt.home/juxtcode filtered-staff-history)
        staff-details (merge-with merge
                                  (update-vals staff-history-by-juxt-code #(hash-map :staff-history %))
                                  (update-vals personal-holidays-by-juxt-code #(hash-map :personal-holidays %)))
        filtered-staff-details (filter (fn [[k v]] (:staff-history v)) staff-details)] ; This is required for situations where we have a holiday history but no employment history
    {:ceiling-year (config/ceiling-year config)
     :status-for-day (config/status-for-day config)
     :public-holidays (data-loader/load-public-holiday-inputs config)
     :tmp-staff-details staff-details
     :staff-details filtered-staff-details}))

(defn holidays-get-status
  "Given a map of options, gets the holiday status of all users on the provided date."
  [{:keys [staff-details
           public-holidays
           status-for-day
           ceiling-year]}]
  (for [staff-member-details staff-details
        :let [staff-member-calendar (juxtcode-history-map->holiday-calendar
                                     ceiling-year
                                     public-holidays
                                     staff-member-details)]]
    (holiday/get-record-for-date (t/date status-for-day) staff-member-calendar)))

(defn holidays-process
  [options]
  (let [config (merge (config/config (:config-path options) (:profile options)) options)
        inputs (load-input-data config)]
    (->> inputs
        holidays-get-status
        (data-output/generate-output config))))

(defn -main [& args]
  (let [{:keys [action action-args options exit-message exit-code]} (validate-args args)]
    (when exit-message
      (println exit-message)
      (System/exit exit-code))
    (holidays-process options)))


(comment
  (holidays-process {:config-path "/var/tmp/test-config.edn"})

  (let [{:keys [staff-details public-holidays ceiling-year]} (load-input-data (config/config "/var/tmp/test-config.edn" nil))]
    (juxtcode-history-map->holiday-calendar ceiling-year public-holidays (first staff-details)))

  )
