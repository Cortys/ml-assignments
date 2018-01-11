(ns assignment11
  (:require [clojure.java.io :refer [reader resource]]
            [clojure.data.csv :refer [read-csv]]))

(def classes #{1 2 3})
(def bin-count 5)
(def bins (set (range bin-count)))
(def feature-count 16)
(def features (set (range feature-count)))

(defn read-data
  [path]
  (->> path reader read-csv
       (drop 1)
       (map (partial mapv #(Double/parseDouble %)))
       (map #(hash-map :x (subvec % 1 (inc feature-count))
                       :y (% (inc feature-count))))))

(defn feature->equi-frequency-bins
  [bin-count feature]
  (let [bin-freq (/ (count feature) bin-count)]
    (->> feature
         (map-indexed vector)
         (sort-by second)
         (map-indexed (fn [i1 [i2 f]] [i2 (quot i1 bin-freq)]))
         (sort-by first)
         (map second))))

(defn feature->equi-width-bins
  [bin-count feature]
  (let [min-feature (apply min feature)
        max-feature (apply max feature)
        bin-width (/ (- max-feature min-feature) bin-count)]
    (map #(max 0 (min (dec bin-count)
                      (int (quot (- % min-feature) bin-width))))
         feature)))

(defn data->bins
  [binner features]
  (let [fc (count (:x (first features)))
        binned-features (->> (range fc)
                             (map (fn [i] (map #(get (:x %) i) features)))
                             (map (partial binner bin-count)))]
    (apply map (fn [f & x] (assoc f :x (vec x)))
           features binned-features)))

(def data->equi-frequency-bins (partial data->bins feature->equi-frequency-bins))
(def data->equi-width-bins (partial data->bins feature->equi-width-bins))

(defn probs
  [acc vals data]
  (let [c (+ (count data) (count vals))]
    (->> data
         (map acc)
         (frequencies)
         (map (fn [[k v]] [k (Math/log (/ v c))]))
         (into (zipmap vals (repeat (Math/log (/ 1 c))))))))

(def class-probs (partial probs :y classes))

(defn cond-probs-for-class
  [class-data feature-index]
  (probs #((:x %) feature-index)
         bins
         class-data))

(defn cond-probs
  [data]
  (into {} (for [class classes]
             (let [class-data (filter #(== class (:y %)) data)]
               [class (mapv (partial cond-probs-for-class class-data)
                            features)]))))

(defn predict
  ([train-data test-data]
   (predict train-data test-data features))
  ([train-data test-data features]
   (let [class-probs (class-probs train-data)
         cond-probs (cond-probs train-data)]
     (for [d test-data]
       (apply max-key
              (fn [class]
                (apply + (class-probs class)
                       (map #(get-in cond-probs [class % (d %)])
                            features)))
              classes)))))

(defn score
  ([train-data test-data]
   (score train-data test-data features))
  ([train-data test-data features]
   (let [prediction (predict train-data
                             (map :x test-data)
                             features)]
     (/ (count (filter identity
                       (map #(== (:y %1) %2)
                            test-data prediction)))
        (count test-data)))))

(defn cross-validate
  ([n data features]
   (/ (apply + (repeatedly n #(cross-validate data features))) n))
  ([data]
   (cross-validate features))
  ([data features]
   (let [data (shuffle data)
         parts (partition (/ (count data) 10) data)
         results (for [i (range 10)]
                   (let [train-data (apply concat
                                           (keep-indexed #(if (not= %1 i) %2)
                                                         parts))
                         test-data (nth parts i)]
                     (score train-data test-data features)))]
     (/ (apply + results) (count results)))))

(defn select-features
  [data & args]
  (let [early-termination (empty? args)]
    (loop [best-score (cross-validate 10 data #{})
           unused features
           used #{}]
      (if (empty? unused)
        (do
          (println "Used all features.")
          [best-score used])
        (let [new-scores (->> unused
                              (pmap (fn [f]
                                      [f (cross-validate 10 data
                                                         (conj used f))]))
                              (into {}))
              best-feature (apply max-key new-scores (keys new-scores))
              new-best-score (new-scores best-feature)]
          (println (str "Selected feature V" (inc best-feature)
                        ", the new best score is " (float new-best-score)
                        ", the old best score was " (float best-score) "."))
          (if (and early-termination
                   (< new-best-score best-score))
            (do
              (println "Score could not be improved anymore.")
              [best-score used])
            (recur new-best-score
                   (disj unused best-feature)
                   (conj used best-feature))))))))

(def data (delay (read-data (resource "data17.csv"))))
(def freq-data (delay (data->equi-frequency-bins @data)))
(def width-data (delay (data->equi-width-bins @data)))
