(ns task1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :refer [reader resource]]
            [clojure.data.csv :refer [read-csv]]
            [lambda-ml.regression :as reg]))

(def α 0.1)
(def λ 0.1)
(def i 500)

(def classes (range 10))
(def data (delay (read-data "pendigits.tra")))

(defn read-data
  [path]
  (let [raw-data (-> path resource reader read-csv)]
    (map (fn [row]
           (let [[x [y]] (split-at (dec (count row)) row)]
             {:x (mapv (comp #(Integer/parseInt %) str/trim) x)
              :y (Integer/parseInt (str/trim y))}))
         raw-data)))

(def random-data-splits (repeatedly #(let [data @data]
                                       (split-at (/ (count data) 2)
                                                 (shuffle data)))))

(defn decompose
  [data classifier]
  (keep (fn [{:keys [x y]}]
          (when-let [class (classifier y)]
            (conj x class)))
        data))

(defn one-vs-rest-decompose
  [data class]
  (decompose data #(if (= % class) 1 0)))

(defn all-pairs-decompose
  [data [class0 class1]]
  (decompose data #(condp = %
                     class0 0
                     class1 1
                     nil)))

(defn log-model
  [data]
  (reg/regression-fit (reg/make-logistic-regression α λ i)
                      data))

(defn one-vs-rest-models
  [data classes]
  (->> classes
       (map (juxt identity (comp log-model (partial one-vs-rest-decompose data))))
       (into {})))

(defn all-pairs-models
  [data classes]
  (->> (combo/combinations classes 2)
       (map (juxt identity (comp log-model (partial all-pairs-decompose data))))
       (into {})))
