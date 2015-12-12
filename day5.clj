(def lines (clojure.string/split-lines (slurp "day5.txt")))

(defn vowel-count [s]
  (count (filter #{\a \e \i \o \u} s)))

(defn consecutive? [s]
  (some true? (map #(= %1 %2) s (rest s))))

(defn evil? [s]
  (or (.contains s "ab")
      (.contains s "cd")
      (.contains s "pq")
      (.contains s "xy")))

(defn nice? [s]
  (and (>= (vowel-count s) 3)
       (consecutive? s)
       (not (evil? s))))

(println (count (filter nice? lines)))

(defn has-split? [s]
  (some true?
        (for [i (range (- (count s) 2))] ; exclusive of the end
          (let [[x _ z] (take 3 (drop i s))]
            (= x z)))))

(defn twice-pair? [s]
  (some true?
        (for [i (range (dec (count s)))] ; exclusive of the end
          ; need "\0\0" so that "aabb" doesn't match "ab"
          (.contains (apply str (concat (take i s) "\0\0" (drop (+ i 2) s)))
                     (apply str (take 2 (drop i s)))))))

(defn nice2? [s]
  (and (has-split? s) (twice-pair? s)))

(println (count (filter nice2? lines)))

