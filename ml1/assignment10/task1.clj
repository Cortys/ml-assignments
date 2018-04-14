(ns task1)

(def data-raw [[0 0 0 -1 350]
               [0 0 0 +1 150]
               [0 0 1 -1 82]
               [0 0 1 +1 62]
               [0 1 0 -1 28]
               [0 1 0 +1 206]
               [0 1 1 -1 76]
               [0 1 1 +1 70]
               [1 0 0 -1 124]
               [1 0 0 +1 160]
               [1 0 1 -1 306]
               [1 0 1 +1 90]
               [1 1 0 -1 86]
               [1 1 0 +1 220]
               [1 1 1 -1 204]
               [1 1 1 +1 100]])

(def data (->> data-raw
               (map (fn [[x0 x1 x2 y obs]]
                      {:x [x0 x1 x2] :y y :obs obs}))))

(def real-probs (->> data
                     (group-by :x)
                     (map (fn [[x [{y1 :y, obs1 :obs} {y2 :y, obs2 :obs}]]]
                            (let [s (+ obs1 obs2)]
                              [x {y1 (/ obs1 s)
                                  y2 (/ obs2 s)}])))
                     (into {})))

(def class-probs (let [class-counts (->> data
                                         (group-by :y)
                                         (map (fn [[y d]]
                                                [y (apply + (map :obs d))]))
                                         (into {}))
                       class-sum (apply + (vals class-counts))]
                   {+1 (/ (class-counts +1) class-sum)
                    -1 (/ (class-counts -1) class-sum)}))

(def cond-probs (->> data
                     (group-by :y)
                     (map (fn [[y d]]
                            [y (vec (for [i (range 3)]
                                      (let [c (->> d
                                                   (group-by #((:x %) i))
                                                   (map (fn [[x d]]
                                                          [x (->> d
                                                                  (map :obs)
                                                                  (reduce +))]))
                                                   (into {}))
                                            s (apply + (vals c))]
                                        {0 (/ (c 0) s), 1 (/ (c 1) s)})))]))
                     (into {})))

(defn comb-prob
  [y x]
  (apply * (class-probs y) (map get (cond-probs y) x)))

(defn est-prob
  [y x]
  (/ (comb-prob y x)
     (+ (comb-prob -1 x) (comb-prob +1 x))))

(def est-probs (->> (for [x0 [0 1], x1 [0 1], x2 [0 1]
                          :let [x [x0 x1 x2]]]
                      [x {-1 (est-prob -1 x)
                          +1 (est-prob +1 x)}])
                    (into {})))

(defn classes
  [p]
  (->> p
       (map (fn [[k v]] [k (apply max-key v (keys v))]))
       (into {})))

(def real-classes (classes real-probs))
(def est-classes (classes est-probs))

(def class-errors (reduce + (map #(if (= (real-classes %)
                                         (est-classes %))
                                    0 1)
                                 (keys real-classes))))

(defn crossentropy
  [p q]
  (- (apply + (map #(* (p %) (Math/log (q %))) (keys p)))))

(defn entropy
  [p]
  (crossentropy p p))

(defn kl-divergence
  [p q]
  (- (crossentropy p q)
     (entropy p)))

(def avg-entropy (let [k (keys real-probs)]
                   (/ (->> k
                           (map #(entropy (real-probs %)))
                           (reduce +))
                      (* (count k) (Math/log 2)))))

(def avg-divergence (let [k (keys real-probs)]
                      (/ (->> k
                              (map #(kl-divergence (real-probs %)
                                                   (est-probs %)))
                              (reduce +))
                         (* (count k) (Math/log 2)))))

(def cost (/ avg-divergence avg-entropy))

(println (str "Average entropy in bits: " avg-entropy))
(println (str "Average Kullback-Leibler divergence in bits: " avg-divergence))
(println (str "Relative divergence: "
              (float (/ (int (* 100000 cost)) 1000)) "%"))
(println (str "Classification errors: " class-errors "/" (count real-classes)))
