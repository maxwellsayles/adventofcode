(use 'clojure.java.io)

(defn md5 [s]
  (apply str
   (map (partial format "%02x")
        (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                   .reset
                   (.update (.getBytes s)))))))

(defn match [s x]
  (.startsWith (md5 s) x))

(defn solve5 [i]
  (if (match (str "yzbqklnj" i) "00000")
    i
    (recur (inc i))))

(defn solve6 [i]
  (if (match (str "yzbqklnj" i) "000000")
    i
    (recur (inc i))))

;;(println (solve5 0))
(println (solve6 0))