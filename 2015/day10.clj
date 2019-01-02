(def inp "3113322113")

(defn step [s]
  (->> s
       (partition-by identity)
       (mapcat (fn [xs] [(count xs) (first xs)]))
       (apply str)))

(println (count (nth (iterate step inp) 40)))
(println (count (nth (iterate step inp) 50)))