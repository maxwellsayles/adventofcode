(def lines (clojure.string/split-lines (slurp "day8.txt")))

(defn space [s]
  (loop [s s, acc 0]
    (cond
      (= s "\"") acc
      (.startsWith s "\\x") (recur (.substring s 4) (inc acc))
      (.startsWith s "\\\\") (recur (.substring s 2) (inc acc))
      (.startsWith s "\\\"") (recur (.substring s 2) (inc acc))
      (.startsWith s "\"") (recur (.substring s 1) acc)
      :else (recur (.substring s 1) (inc acc)))))

(defn encode [s]
  (loop [s s, acc 2]
    (cond
      (empty? s) acc
      (.startsWith s "\\") (recur (.substring s 1) (+ acc 2))
      (.startsWith s "\"") (recur (.substring s 1) (+ acc 2))
      :else (recur (.substring s 1) (inc acc)))))

(def orig-space (reduce + (map count lines)))
(def total-space (reduce + (map space lines)))
(def new-space (reduce + (map encode lines)))

(println (- orig-space total-space))
(println (- new-space orig-space))
