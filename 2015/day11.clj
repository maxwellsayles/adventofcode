(def start "hxbxwxba")

(defn inc-str [s]
  (letfn [(helper
            ([] '(\a))
            ([x & xs]
             (if (= x \z)
               (cons \a (apply helper xs))
               (-> x int inc char (cons xs)))))]
    (->> s reverse (apply helper) reverse (apply str))))

(defn straight? [s]
  (letfn [(helper
            ([] false)
            ([_] false)
            ([_ _] false)
            ([x y z & more] (or (= x (dec y) (- z 2)) (apply helper y z more))))]
    (->> s seq (map int) (apply helper))))

(defn clean? [s]
  (not (or (.contains s "i") (.contains s "l") (.contains s "o"))))

(defn twice? [s]
  (->> s (partition-by identity) (map count) (filter #(> % 1)) count (<= 2)))

(defn valid? [s]
  (and (straight? s) (clean? s) (twice? s)))

(defn next-valid [s]
  (if (valid? s) s (recur (inc-str s))))

(println (next-valid start))
(println (next-valid (inc-str (next-valid start))))
