(ns common
  (:require [clojure.java.io :refer [reader resource]]
            [clojure.data.csv :refer [read-csv]]))

(defn read-data
  [path]
  (let [raw-data (-> path resource reader read-csv)]
    (->> raw-data
         (drop 1)
         (map (fn [row]
                (let [[x [y]] (split-at (dec (count row)) row)]
                  {:x (mapv #(Float/parseFloat %) x)
                   :y (Float/parseFloat y)}))))))

(def data (delay (read-data "data10.csv")))
