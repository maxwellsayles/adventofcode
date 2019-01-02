(def input [1 2 3 7 11 13 17 19 23 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113])
;;(def input [1 2 3 4 5 7 8 9 10 11])
(def n (count input))
(def t (atom (/ (reduce + input) 3)))

(def start {:count 0 :sum 0 :prod 1})

(defn append [x xs]
  (let [{:keys [count sum prod]} xs]
    {:count (inc count)
     :sum (+ sum x)
     :prod (*' prod x)}))

(defn best [xs ys]
  (cond (false? xs) ys
        (false? ys) xs
        (< (:count xs) (:count ys)) xs
        (< (:count ys) (:count xs)) ys
        (< (:prod xs) (:prod ys)) xs
        :else ys))

(defn step [bs xs vs]
  (cond
    ;; Exceeds the sum? Then no solution possible.
    (> (:sum xs) @t)
    bs

    ;; Longer than the best so far? Prune.
    (and bs (> (:count xs) (:count bs)))
    bs

    ;; More entangled than the best so far? Prune.
    (and bs (> (:prod xs) (:prod bs)))
    bs

    ;; Out of values, but doesn't weight enough?  No solution possible.
    (and (empty? vs) (< (:sum xs) @t))
    bs

    ;; Out of values and equal to target. Choose best.
    (and (empty? vs) (= (:sum xs) @t))
    (best bs xs)

    ;; We still have values.  Try the next value in the shortest line and elsewhere.
    :else
    (let [v (first vs)
          vs2 (rest vs)
          xs2 (append v xs)]
      (-> bs
          (step xs vs2)
          (step xs2 vs2)))))
               
(println (step false start input))
(reset! t (/ (reduce + input) 4))
(println (step false start input))
