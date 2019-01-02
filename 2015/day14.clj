(def lines (clojure.string/split-lines (slurp "day14.txt")))

(defn parse [s]
  (let [[_ name speed duration rest]
        (re-matches #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." s)]
    [name (read-string speed) (read-string duration) (read-string rest)]))

(def input (map parse lines))

(defn distance [secs [_ speed duration rest]]
  (let [interval (+ duration rest)
        repetitions (quot secs interval)
        padding-secs (mod secs interval)
        base (* repetitions duration speed)]
    (+ base (* (min padding-secs duration) speed))))

(defn winning-dist [secs]
  (apply max (map (partial distance secs) input)))

(defn winners [secs]
  (let [distances (map (partial distance secs) input)
        winning-dist (apply max distances)
        wins (map #(if (= % winning-dist) 1 0) distances)]
    (zipmap (map first input) wins)))

(def scores
  (reduce (partial merge-with +) {}
          (map winners (range 1 2504))))

(println (winning-dist 2503))
(println (->> scores (apply list) (map second) (apply max)))

  