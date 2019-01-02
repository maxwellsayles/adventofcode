(def n 3400000)

(defn helper [acc i j]
  (cond
    (> i n) acc
    (> j n) (recur acc (inc i) (inc i))
    :else (let [acc2 (assoc acc j (+ (or (acc j) 0) i))]
            (recur acc2 i (+ i j)))))

(def counts (helper {} 1 1))

(defn find-it [i]
 (cond (<= n (counts i)) i
       :else (recur (inc i))))
    
(println (find-it 1))

(defn helper2 [acc i j]
  (cond
    (> i n) acc
    (or (> j (* 50 i)) (> j n)) (recur acc (inc i) (inc i))
    :else (let [acc2 (assoc acc j (+ (or (acc j) 0) i))]
            (recur acc2 i (+ i j)))))

(def counts2 (helper2 {} 1 1))

(defn find-it2 [i]
  (cond (<= n (/ (* 11 (counts2 i)) 10)) i
        :else (recur (inc i))))
    
(println (find-it2 1))      
