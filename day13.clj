(refer 'clojure.set)

(def lines (clojure.string/split-lines (slurp "day13.txt")))

(defn parse [acc x]
  (let [[_ s c p t] (re-matches #"(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)." x)]
    (assoc acc [s t] (({"gain" + "lose" -} c) (read-string p)))))

(def weights (reduce parse {} lines))
(def peeps (set (apply concat (keys weights))))

(defn step [weights sitting to-sit]
  (letfn [(f [x]
            (+ (weights [(first sitting) x])
               (weights [x (first sitting)])
               (step weights (cons x sitting)
                     (clojure.set/difference to-sit #{x}))))]
    (if (empty? to-sit) (let [[f l] [(first sitting) (last sitting)]]
                          (+ (weights [f l]) (weights [l f])))
        (apply max (map f to-sit)))))

(def weights-me
  (letfn [(step [acc x]
            (-> acc
                (assoc ["me" x] 0)
                (assoc [x "me"] 0)))]
    (reduce step weights peeps)))

(println (step weights '("Bob") (clojure.set/difference peeps #{"Bob"})))
(println (step weights-me '("Bob") (clojure.set/difference (conj peeps "me") #{"Bob"})))
