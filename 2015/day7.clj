(def lines (clojure.string/split-lines (slurp "day7.txt")))

(defmacro cond-let [& clauses]
  (when-let [[bindings then & more] clauses]
    (if (= bindings :else) then
        `(if-let ~bindings ~then (cond-let ~@more)))))

(defn build-input [acc s]
  (cond-let
   [[_ x t] (re-matches #"NOT (\w+) -> (\w+)" s)] (assoc acc t [:not x])
   [[_ x y t] (re-matches #"(\w+) AND (\w+) -> (\w+)" s)] (assoc acc t [:and x y])
   [[_ x y t] (re-matches #"(\w+) OR (\w+) -> (\w+)" s)] (assoc acc t [:or x y])
   [[_ x y t] (re-matches #"(\w+) LSHIFT (\d+) -> (\w+)" s)] (assoc acc t [:lsh x (Integer. y)])
   [[_ x y t] (re-matches #"(\w+) RSHIFT (\d+) -> (\w+)" s)] (assoc acc t [:rsh x (Integer. y)])
   [[_ x t] (re-matches #"(\w+) -> (\w+)" s)] (assoc acc t [:val x])
   :else acc))

(defn eval-sym [symbol assigns]
  (if (or (integer? symbol) (re-matches #"\d+" symbol))
    [(Integer. symbol), assigns]
    (let [[op x y] (assigns symbol)]
      (case op
        :val (let [[x1, tmp] (eval-sym x assigns)]
                [x1, (assoc tmp symbol [:val x1])])
        :lsh (let [[x1, tmp] (eval-sym x assigns)
                   x2 (bit-shift-left x1 y)]
               [x2, (assoc tmp symbol [:val x2])])
        :rsh (let [[x1, tmp] (eval-sym x assigns)
                   x2 (bit-shift-right x1 y)]
               [x2, (assoc tmp symbol [:val x2])])
        :not (let [[x1, tmp] (eval-sym x assigns)
                   x2 (bit-not x1)]
               [x2, (assoc tmp symbol [:val x2])])
        :and (let [[x1, tmpx] (eval-sym x assigns)
                   [y1, tmpy] (eval-sym y tmpx)
                   z (bit-and x1 y1)]
               [z, (assoc tmpy symbol [:val z])])
        :or (let [[x1, tmpx] (eval-sym x assigns)
                  [y1, tmpy] (eval-sym y tmpx)
                  z (bit-or x1 y1)]
              [z, (assoc tmpy symbol [:val z])])))))

(def assigns (reduce build-input {} lines))

(def solution1 (first (eval-sym "a" assigns)))

(println solution1)
(println (first (eval-sym "a" (assoc assigns "b" [:val solution1]))))
