(def boss-hp 51)
(def boss-damage 9)

(declare your-round)

(defn boss-round [hp mana shield poison recharge boss-hp spent]
  (let [boss-hp (- boss-hp (if (> poison 0) 3 0))
        mana (+ mana (if (> recharge 0) 101 0))
        boss-ap (max 1 (- boss-damage (if (> shield 0) 7 0)))]
    (if (<= boss-hp 0)
      spent ; win!
      (let [hp (- hp boss-ap)]
        (if (<= hp 0)
          false ; lose :(
          (your-round hp
                      mana
                      (max 0 (dec shield))
                      (max 0 (dec poison))
                      (max 0 (dec recharge))
                      boss-hp
                      spent))))))

(defn your-round [hp mana shield poison recharge boss-hp spent]
  (let [boss-hp (- boss-hp (if (> poison 0) 3 0))
        mana (+ mana (if (> recharge 0) 101 0))]
    (if (<= boss-hp 0)
      spent ; win!
      (let [shield (max 0 (dec shield))        
            poison (max 0 (dec poison))
            recharge (max 0 (dec recharge))
            
            res [;; Magic Missile costs 53 mana. It instantly does 4 damage.
                 (if (< mana 53)
                   false
                   (boss-round hp
                               (- mana 53)
                               shield
                               poison
                               recharge
                               (- boss-hp 4)
                               (+ spent 53)))
                 
                 ;; Drain costs 73 mana. It instantly does 2 damage and heals you
                 ;; for 2 hit points.
                 (if (< mana 73)
                   false
                   (boss-round (+ 2 hp)
                               (- mana 73)
                               shield
                               poison
                               recharge
                               (- boss-hp 2)
                               (+ spent 73)))
                 
                 ;; Shield costs 113 mana. It starts an effect that lasts for 6
                 ;; turns. While it is active, your armor is increased by 7.
                 (if (or (> shield 0) (< mana 113))
                   false
                   (boss-round hp
                               (- mana 113)
                               6
                               poison
                               recharge
                               boss-hp
                               (+ spent 113)))
                 
                 ;; Poison costs 173 mana. It starts an effect that lasts for 6
                 ;; turns. At the start of each turn while it is active, it deals
                 ;; the boss 3 damage.
                 (if (or (> poison 0) (< mana 173))
                   false
                   (boss-round hp
                               (- mana 173)
                               shield
                               6
                               recharge
                               boss-hp
                               (+ spent 173)))
                 
                 ;; Recharge costs 229 mana. It starts an effect that lasts for 5
                 ;; turns. At the start of each turn while it is active, it gives you
                 ;; 101 new mana.
                 (if (or (> recharge 0) (< mana 229))
                   false
                   (boss-round hp
                               (- mana 229)
                               shield
                               poison
                               5
                               boss-hp
                               (+ spent 229)))]
            
            candidates (->> res (filter #(not (false? %))))]

        (if (empty? candidates)
          false ; lose :(
          (apply min candidates))))))

;;(println (your-round 10 250 0 0 0 13 0))
(println (your-round 50 500 0 0 0 boss-hp 0))

