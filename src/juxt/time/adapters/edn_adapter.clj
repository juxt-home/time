;; Copyright © 2022, JUXT LTD.

(ns juxt.time.adapters.edn-adapter
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn read-data
  [file-path options]
  (with-open [reader (io/reader file-path)
              pushback-reader (java.io.PushbackReader. reader)]
    (edn/read pushback-reader)))
