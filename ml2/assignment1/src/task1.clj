(ns task1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :refer [reader resource]]
            [clojure.data.csv :refer [read-csv]]
            [lambda-ml.regression :as reg]))

(defn read-data
  [path]
  (let [raw-data (-> path resource reader read-csv)]
    (map (fn [row]
           (let [[x [y]] (split-at (dec (count row)) row)]
             {:x (mapv (comp #(Integer/parseInt %) str/trim) x)
              :y (Integer/parseInt (str/trim y))}))
         raw-data)))

(def α 0.1)
(def λ 0.1)
(def i 1)

(def classes (range 10))
(def data (delay (read-data "pendigits.tra")))

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
       (pmap (juxt identity (comp log-model (partial one-vs-rest-decompose data))))
       (into {})))

(defn all-pairs-models
  [data classes]
  (->> (combo/combinations classes 2)
       (pmap (juxt identity (comp log-model (partial all-pairs-decompose data))))
       (into {})))

(defn one-vs-rest-predict
  [models x]
  (apply max-key
         #(first (reg/regression-predict (models %) [x]))
         (keys models)))

(defn all-pairs-predict
  [models x]
  (let [counts (reduce (fn [counts [c0 c1 :as k]]
                         (let [c1-count (first (reg/regression-predict (models k) [x]))
                               c0-count (- 1 c1-count)
                               add (fnil + 0)]
                           (-> counts
                               (update c0 add c0-count)
                               (update c1 add c1-count))))
                       {} (keys models))]
    (apply max-key counts (keys counts))))

(defn accuracy
  [predictor data]
  (/ (count (keep (comp (partial apply =)
                        (juxt (comp predictor :x) :y))
                  data))
     (count data)))

(defn one-vs-rest-accuracy
  [models data]
  (accuracy (partial one-vs-rest-predict models) data))

(defn all-pairs-accuracy
  [models data]
  (accuracy (partial all-pairs-predict models) data))

(defn one-vs-rest-eval
  [classes train test]
  (one-vs-rest-accuracy (one-vs-rest-models train classes)
                        test))

(defn all-pairs-eval
  [classes train test]
  (all-pairs-accuracy (all-pairs-models train classes)
                      test))

(defn comparison
  [classes data n]
  (let [data-splits (repeatedly n #(split-at (/ (count data) 2)
                                             (shuffle data)))]
    (pmap (fn [i [train test]]
            (let [ovr (one-vs-rest-eval classes train test)
                  ap (all-pairs-eval classes train test)]
              (println "Split " i ": OVR=" ovr ", AP=" ap)
              {:ovr ovr, :ap ap}))
          (range) data-splits)))
