;; Copyright © 2022, JUXT LTD.

(ns juxt.time.adapters.csv-adapter
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-csv-lines
  [reader options]
  (let [csv-lines (csv/read-csv reader options)
          header-line (first csv-lines)
        header-line-to-keys (map (comp keyword #(str/replace % #"\s" "")) header-line)]
      (map (partial zipmap header-line-to-keys) (rest csv-lines))))

(defn read-data
  [file-path options]
  (with-open [reader (io/reader file-path)]
    (doall (read-csv-lines reader options))))

(defn write-data
  [file-path options data]
  (with-open [writer (io/writer file-path)]
    (apply csv/write-csv (conj (mapcat identity options) data writer))))
