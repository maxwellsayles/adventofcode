(def lines (clojure.string/split-lines (slurp "day23.txt")))

(def input (zipmap (iterate inc 0) lines))

(defmacro cond-let [& xs]
  (when-let [[binding body & more] xs]
    (if (= :else binding)
      body
      `(if-let ~binding ~body (cond-let ~@more)))))
      

(defn step [i a b]
  (let [inst (input i)]
    (cond
     (nil? inst) [a b]

     (= inst "hlf a") (recur (inc i) (/ a 2) b)
     (= inst "hlf b") (recur (inc i) a (/ b 2))
     
     (= inst "tpl a") (recur (inc i) (* a 3) b)
     (= inst "tpl b") (recur (inc i) a (* b 3))
     
     (= inst "inc a") (recur (inc i) (inc a) b)
     (= inst "inc b") (recur (inc i) a (inc b))
     
     (.startsWith inst "jmp") (let [[_ d] (re-matches #"jmp ([+\-]\d+)" inst)]
                                (recur (+ i (read-string d)) a b))

     (.startsWith inst "jie") (let [[_ r d] (re-matches #"jie (\w), ([+\-]\d+)" inst)]
                                (cond (and (= r "a") (even? a))
                                      (recur (+ i (read-string d)) a b)

                                      (and (= r "b") (even? b))
                                      (recur (+ i (read-string d)) a b)

                                      :else (recur (inc i) a b)))
     
     (.startsWith inst "jio") (let [[_ r d] (re-matches #"jio (\w), ([+\-]\d+)" inst)]
                                (cond (and (= r "a") (= 1 a))
                                      (recur (+ i (read-string d)) a b)

                                      (and (= r "b") (= 1 b))
                                      (recur (+ i (read-string d)) a b)

                                      :else (recur (inc i) a b))))))

(println (step 0 0 0))
(println (step 0 1 0))

