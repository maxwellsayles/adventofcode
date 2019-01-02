(def input (.trim (slurp "day3.txt")))

(defn helper [[acc x y] c]
  (case c
    \^ [(conj acc [x (inc y)]) x (inc y)]
    \v [(conj acc [x (dec y)]) x (dec y)]
    \< [(conj acc [(dec x) y]) (dec x) y]
    \> [(conj acc [(inc x) y]) (inc x) y]
    :else [acc x y]))

(count (first (reduce helper [#{[0 0]} 0 0] input)))

(defn helper2 [[acc x1 y1 x2 y2] [c1 c2]]
  (let [[nx1 ny1] (case c1
                    \^ [x1 (inc y1)]
                    \v [x1 (dec y1)]
                    \< [(dec x1) y1]
                    \> [(inc x1) y1]
                    :else [x1 y1])
        [nx2 ny2] (case c2
                    \^ [x2 (inc y2)]
                    \v [x2 (dec y2)]
                    \< [(dec x2) y2]
                    \> [(inc x2) y2]
                    :else [x2 y2])]
    [(conj acc [nx1 ny1] [nx2 ny2]) nx1 ny1 nx2 ny2]))

(println (count (first (reduce helper [#{[0 0]} 0 0] input))))
(println (count (first (reduce helper2 [#{[0 0]} 0 0 0 0]
                               (partition 2 input)))))
  