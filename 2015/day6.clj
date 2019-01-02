(def lines (clojure.string/split-lines (slurp "day6.txt")))

(defn ixs [t l b r]
  (do
    (print \.)
    (flush)
    (for [x (range (Integer. l) (inc (Integer. r)))
          y (range (Integer. t) (inc (Integer. b)))]
      [x y])))

(defn turn-on [m t l b r]
  (reduce #(assoc %1 %2 true) m (ixs t l b r)))

(defn turn-off [m t l b r]
  (reduce #(assoc %1 %2 false) m (ixs t l b r)))

(defn toggle [m t l b r]
  (reduce #(assoc %1 %2 (not (%1 %2))) m (ixs t l b r)))

(defn inc-block [m t l b r]
  (reduce #(assoc %1 %2 (inc (or (%1 %2) 0))) m (ixs t l b r)))

(defn dec-block [m t l b r]
  (reduce #(assoc %1 %2 (max 0 (dec (or (%1 %2) 0)))) m (ixs t l b r)))

(defn inc-twice [m t l b r]
  (inc-block (inc-block m t l b r) t l b r))

(defn do-line [f1 f2 f3 m line]
  (let [[_ type1 l1 t1 r1 b1]
        (re-matches #"turn (on|off) (\d+),(\d+) through (\d+),(\d+)" line)
        [_ l2 t2 r2 b2]
        (re-matches #"toggle (\d+),(\d+) through (\d+),(\d+)" line)]
    (cond
     (= type1 "on") (f1 m t1 l1 b1 r1)
     (= type1 "off") (f2 m t1 l1 b1 r1)
     (not (nil? l2)) (f3 m t2 l2 b2 r2))))

(let [f (partial do-line turn-on turn-off toggle)]
  (println
   (count
    (filter true?
            (vals (reduce f {} lines))))))

(let [f (partial do-line inc-block dec-block inc-twice)]
  (println
   (reduce + (vals (reduce f {} lines)))))