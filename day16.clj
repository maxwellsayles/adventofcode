(def lines (clojure.string/split-lines (slurp "day16.txt")))

(defn parse [s]
  (let [[_ n raw] (re-matches #"Sue (\d+): (.*)" s)]
    [n (->> (clojure.string/split raw #", ")
            (map #(re-matches #"(\w+): (\d+)" %))
            (map (fn [[_ a b]] [(keyword a) (read-string b)]))
            (apply concat)
            (apply hash-map))]))
    
(def input (map parse lines))

(def data {:children 3
           :cats 7
           :samoyeds 2
           :pomeranians 3
           :akitas 0
           :vizslas 0
           :goldfish 5
           :trees 3
           :cars 2
           :perfumes 1})

(defn matches? [m] (every? (fn [[k v]] (= v (data k))) m))

(defn matches-twist? [m]
  (letfn [(helper [[k v]]
            (let [x (data k)]
              (cond
                (= :trees k) (> v x)
                (= :cats k) (> v x)
                (= :pomeranians k) (< v x)
                (= :goldfish k) (< v x)
                :else (= v x))))]
    (every? helper m)))

(doseq [[i m] input]
  (when (matches? m) (println i)))

(doseq [[i m] input]
  (when (matches-twist? m) (println i)))
