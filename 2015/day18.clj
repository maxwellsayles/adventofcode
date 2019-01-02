(def test-data [".#.#.#"
                "...##."
                "#....#"
                "..#..."
                "#.#..#"
                "####.."])

(def data (clojure.string/split-lines (slurp "day18.txt")))

(defn parse [data]
  (let [xs (mapcat (fn [row y]
                     (map (fn [c x] [[x y] (= c \#)]) row (range (count row))))
                   data
                   (range (count data)))]
    (reduce (fn [acc [k v]] (assoc acc k v)) {} xs)))

(defn print-grid [grid w h]
  (doseq [y (range h)]
    (doseq [x (range w)]
      (print (str (if (grid [x y]) \# \.))))
    (println)))

(defn next-state [grid x y]
  (let [vs (map #(grid [(+ x %1) (+ y %2)]) [-1 0 1 -1 1 -1 0 1] [-1 -1 -1 0 0 1 1 1])
        v (grid [x y])
        c (count (filter true? vs))]
    (if v
      (<= 2 c 3)  ; [x y] is on and 2 or 3 neighbors are on.
      (= 3 c))))

(defn next-grid [grid w h]
  (reduce (fn [acc [x y]] (assoc acc [x y] (next-state grid x y)))
          {}
          (for [x (range w) y (range h)] [x y])))

(defn on-corners [grid w h]
  (let [w1 (dec w)
        h1 (dec h)]
    (-> grid
        (assoc [0 0] true)
        (assoc [w1 0] true)
        (assoc [0 h1] true)
        (assoc [w1 h1] true))))

(defn next-grid-on [grid w h]
    (-> (next-grid grid w h) (on-corners w h)))
    
(def test-grid (parse test-data))

(def grid (parse data))

(println
 (count
  (filter true?
          (vals (nth (iterate #(next-grid % 100 100)
                              grid)
                     100)))))

(println
 (count
  (filter true?
          (vals (nth (iterate #(next-grid-on % 100 100)
                              (on-corners grid 100 100))
                     100)))))

