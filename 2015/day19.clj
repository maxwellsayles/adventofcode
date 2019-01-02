(def start "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")

(def all-moves (->> (slurp "day19.txt")
                    clojure.string/split-lines
                    (map #(re-matches #"(\w+) => (\w+)" %))
                    (map rest)
                    vec))

(defn mutate [acc start]
  (letfn [(step [acc [before after]]
            (let [n (count before)
                  helper (fn [acc pref s]
                           (if (empty? s) acc
                               (let [pref2 (str pref (first s))
                                     s2 (apply str (rest s))]
                                 (if (.startsWith s before)
                                   (let [sub (apply str pref after (drop n s))
                                         acc2 (conj acc sub)]
                                     (recur acc2 pref2 s2))
                                   (recur acc pref2 s2)))))]
              (helper acc "" start)))]
    (reduce step acc all-moves)))

;; Super fast randomized algorithm.  Just make random moves until you can't do any more,
;; or you get a solution.  I've only seen it produce `nil` or the correct answer, but of
;; course, I suspect this depends greatly on the input.
(defn rand-replacements [moves s c]
  (cond
    (empty? moves) nil
    (= s "e") c
    :else (let [i (rand-int (count moves))
                [dst src] (nth moves i)
                s2 (.replaceFirst s src dst)]
            (if (not= s s2)
              (recur all-moves s2 (inc c))
              (let [new-moves (vec (concat (subvec moves 0 i)
                                           (subvec moves (inc i))))]
                (recur new-moves s c))))))

(println (count (mutate #{} start)))
(loop []
  (let [res (rand-replacements all-moves start 0)]
  (if (nil? res) (recur) (println res))))

