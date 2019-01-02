(def lines (clojure.string/split-lines (slurp "day15.txt")))

(defn parse [s]
  (map read-string
       (rest
        (re-matches
         #"\w+: \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+), \w+ (-?\d+)" s))))

(def input (map parse lines))

(defn amount [& qs]
  (apply *
         (map #(if (neg? %) 0 %)
              (take 4
                    (reduce (partial map +)
                            (map (fn [q xs] (map (fn [x] (* x q)) xs)) qs input))))))

(defn amount-500 [& qs]
  (let [[x1 x2 x3 x4 x5]
        (reduce (partial map +)
                (map (fn [q xs] (map (fn [x] (* x q)) xs)) qs input))]
    (if (not= 500 x5) 0
        (apply * (map #(if (neg? %) 0 %) (list x1 x2 x3 x4))))))

(defn solve [amount & qs]
  (let [remaining (- 100 (apply + qs))]
    (cond
     (> (count qs) (count input)) 0
     (< remaining 0) 0
     (and (= (count qs) (count input)) (zero? remaining)) (apply amount qs)
     :else (max
            (apply solve amount (cons 0 qs))
            (apply solve amount (cons (inc (first qs)) (rest qs)))))))
     
(println (solve amount 0))
(println (solve amount-500 0))
