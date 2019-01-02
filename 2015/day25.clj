(def start 20151125)
(def multiplier 252533)
(def modulus 33554393)
(def row 2978)
(def col 3083)
(def idx (+ col -1 (/ (* (+ row col -2) (+ row col -1)) 2)))

(defn bin-pow-mod [b e m]
  (cond (zero? e) 1
        (odd? e) (mod (* b (bin-pow-mod b (dec e) m)) m)
        :else (mod (bin-pow-mod (mod (* b b) m) (/ e 2) m) m)))

(println (mod (* (bin-pow-mod multiplier idx modulus) start) modulus))
