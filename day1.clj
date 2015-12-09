(use 'clojure.java.io)

(defn solve1 [s]
  (reduce #(case %2
             \( (inc %1)
             \) (dec %1) 0 s
             %1) 0 s))

(defn solve2 [s]
  (let [xs (reductions #(case %2
                          \( (inc %1)
                          \) (dec %1) 0 s
                          %1) 0 s)]
    (count (take-while #(not= -1 %) xs))))

(with-open [rdr (reader "day1.txt")]
  (doseq [line (line-seq rdr)]
    (println (solve1 line))
    (println (solve2 line))))