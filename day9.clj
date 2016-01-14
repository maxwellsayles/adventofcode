(refer 'clojure.set)

(def lines (clojure.string/split-lines (slurp "day9.txt")))

(defn add-to-map [graph line]
  (let [[_ x y d] (re-matches #"(\w+) to (\w+) = (\d+)" line)
        xx (.substring x 0 2)
        yy (.substring y 0 2)
        dd (Integer. d)]
    (-> graph
        (assoc [xx yy] dd)
        (assoc [yy xx] dd)
        (assoc [xx xx] 0)
        (assoc [yy yy] 0))))

(def graph (reduce add-to-map {} lines))
(def nodes (set (map first (keys graph))))

(defn tsp [optfn graph unvisited cache start]
  (cond
    (contains? cache [start unvisited]) cache
    (empty? unvisited) (assoc cache [start unvisited] 0)
    :else (let [drive (fn [cache new-start]
                        (let [new-unvisited (difference unvisited #{new-start})]
                          (tsp optfn graph new-unvisited cache new-start)))

                cache-after-visits (reduce drive cache unvisited)

                distance-to (fn [x]
                              (+ (graph [start x])
                                 (cache-after-visits [x (difference unvisited #{x})])))

                opt-dist (apply optfn (map distance-to unvisited))]
            
            (assoc cache-after-visits [start unvisited] opt-dist))))

(defn solve [optfn]
  (let [final-cache (letfn [(helper [cache x]
                              (tsp optfn graph (difference nodes #{x}) cache x))]
                      (reduce helper {} nodes))

        solve-with-start (fn [x] (final-cache [x (difference nodes #{x})]))]
    
    (apply optfn (map solve-with-start nodes))))

(println (solve min))
(println (solve max))
