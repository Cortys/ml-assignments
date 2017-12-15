(ns task1
  (:require ; [uncomplicate.commons.core :refer [with-release]]
            ; [uncomplicate.clojurecl.core :refer [with-default *command-queue*]]
            [uncomplicate.neanderthal.native :refer [fv fge]]
            [uncomplicate.neanderthal.linalg :refer [sv]]
            [common :refer [data]]))

(defn data->Xy
  [data]
  (let [data-count (count data)
        fc (count (:x (first data)))
        xdata (->> data
                   (map :x)
                   (mapcat #(concat '(1) % (for [i (range fc), j (range i fc)]
                                             (* (% i) (% j))))))
        extended-fc (+ 1 fc (/ (* fc (inc fc)) 2))
        X (fge data-count
               extended-fc
               xdata
               {:layout :row})
        y (fv (map :y data))]
    (println (take 78 xdata))
    [X y]))

(defn ols
  [data]
  (let [[X y] (data->Xy data)]
    (sv X y)))

(def ols-result (delay (ols @data)))
