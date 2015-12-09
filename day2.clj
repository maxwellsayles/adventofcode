(use 'clojure.java.io)

(defn wrapping [x y z]
  (let [[x y z] (map bigint [x y z])
        [s1 s2 _] (sort [x y z])]
    (+ (* 2 x y) (* 2 y z) (* 2 x z) (* s1 s2))))

(defn ribbon [x y z]
  (let [[x y z] (map bigint [x y z])
        [s1 s2 _] (sort [x y z])]
    (+ s1 s1 s2 s2 (* x y z))))

(defn solve-f [f sides]
  (->> sides
       (map #(clojure.string/split % #"x"))
       (map #(apply f %))
       (apply +)))

(defn solve-wrapping [sides]
  (solve-f wrapping sides))

(defn solve-ribbon [sides]
  (solve-f ribbon sides))

(with-open [rdr (reader "day2.txt")]
  (let [lines (line-seq rdr)]
    (println (solve-wrapping lines))
    (println (solve-ribbon lines))))

