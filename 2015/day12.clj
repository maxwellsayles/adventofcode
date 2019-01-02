(require 'clojure.data.json)

(def input (slurp "day12.txt"))

(defn step [matcher]
  (when-let [m (re-find matcher)]
    (lazy-seq (cons m (step matcher)))))
  

(def solve (step (re-matcher #"-?\d+" input)))

(println (reduce + (map #(Integer. %) solve)))

(def json (clojure.data.json/read-str input))

(defn values [m]
  (->> m (apply list) (map second) vec))

(defn sum-json [n]
  (cond
    (integer? n) n
    (string? n) (if (re-matches #"-?\d+" n) (Integer. n) 0)
    (vector? n) (->> n (map sum-json) (reduce +))
    (map? n) (let [vs (values n)]
               (if (some #(= % "red") vs) 0 (->> vs (map sum-json) (reduce +))))))

(println (sum-json json))

