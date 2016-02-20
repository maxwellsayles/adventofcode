(def input [1 2 3 7 11 13 17 19 23 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113])
;;(def input [1 2 3 4 5 7 8 9 10 11])
(def n (count input))
(def t (/ (reduce + input) 3))

(def start {:count 0 :sum 0 :prod 1 :vals '()})

(defn append [x xs]
  (let [{:keys [count sum prod vals]} xs]
    {:count (inc count)
     :sum (+ sum x)
     :prod (*' prod x)
     :vals (cons x vals)}))

(defn best [xs ys]
  (cond (false? xs) ys
        (false? ys) xs
        (< (:count xs) (:count ys)) xs
        (< (:count ys) (:count xs)) ys
        (< (:prod xs) (:prod ys)) xs
        :else ys))

(defn step [bs xs ys zs vs]
  (cond
    ;; Anything exceed the sum? Then no solution possible.
    (or (> (:sum xs) t)
        (> (:sum ys) t)
        (> (:sum zs) t))
    bs

    ;; Everything longer than the best so far? Prune.
    (and bs (let [m (:count bs)]
              (and (> (:count xs) m)
                   (> (:count ys) m)
                   (> (:count zs) m))))
    bs

    ;; Everything more entangled than the best so far? Prune.
    (and bs (let [m (:prod bs)]
              (and (> (:prod xs) m)
                   (> (:prod ys) m)
                   (> (:prod zs) m))))
    bs
              
    ;; Nothing exceeds the sum.  Out of values?  Choose the best.
    (empty? vs)
    (-> bs (best xs) (best ys) (best zs))

    ;; We still have values.  Try the next value in each position.
    :else
    (let [v (first vs)
          vs2 (rest vs)
          xs2 (append v xs)
          ys2 (append v ys)
          zs2 (append v zs)]
      (-> bs
          (step xs2 ys zs vs2)
          (step xs ys2 zs vs2)
          (step xs ys zs2 vs2)))))
               
(println (step false start start start input))
