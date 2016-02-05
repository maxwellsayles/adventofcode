;; Buy exactly 1
(def weapons [[8 4 0]
              [10 5 0]
              [25 6 0]
              [40 7 0]
              [74 8 0]])

;; Buy exactly 1
(def armor [[0 0 0]
            [13 0 1]
            [31 0 2]
            [53 0 3]
            [75 0 4]
            [102 0 5]])

;; Buy exactly 2
(def rings [[0 0 0]
            [0 0 0]
            [25 1 0]
            [50 2 0]
            [100 3 0]
            [20 0 1]
            [40 0 2]
            [80 0 3]])

;; Boss's stats
(def boss-hp 100)
(def boss-damage 8)
(def boss-armor 2)

(defn round [hp1 damage1 armor1 hp2 damage2 armor2 win?]
  (cond
   (and (<= hp1 0) (<= hp2 0)) (throw (Exception. "Unexpected."))
   (<= hp2 0) win?
   (<= hp1 0) (not win?)
   :else (let [attack (max 1 (- damage1 armor2))
               new-hp2 (- hp2 attack)]
           (recur new-hp2 damage2 armor2 hp1 damage1 armor1 (not win?)))))

(defn third [x] (nth x 2))

(def all-rounds
  (->>
   (for [w weapons]
     (for [a armor]
       (for [r1 rings r2 rings :when (not= r1 r2)]
         (let [cost (reduce + (map first [w a r1 r2]))
               hp1 (reduce + (map second [w a r1 r2]))
               armor1 (reduce + (map third [w a r1 r2]))
               win? (round 100 hp1 armor1 boss-hp boss-damage boss-armor true)]
           [cost win?]))))
   flatten
   (partition 2)))

(println (->> all-rounds
              (filter second)
              (map first)
              (apply min)))

(println (->> all-rounds
              (filter #(not (second %)))
              (map first)
              (apply max)))
  
