(defn solve
  ([acc t] (if (zero? t) (list acc) '()))
  ([acc t x & xs]
   (if (<= x t)
     (concat (apply solve (cons x acc) (- t x) xs)
             (apply solve acc t xs))
     (apply solve acc t xs))))

(def input [11 30 47 31 32 36 3 1 5 3 32 36 15 11 46 26 28 1 19 3])

(def solutions (apply solve '() 150 input))

(println (count solutions))

(let [best (apply min (map count solutions))]
  (println (count (filter #(= best (count %)) solutions))))
