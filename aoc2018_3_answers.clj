(ns aoc2018_3_answers)

; On how to use map (using %)
(defn add-x-to-each [ints x] (map #(+ % x) ints))

(defn parse1 [x]
  (map #(clojure.string/split % #" ") x))
(defn parse2 [x]
  (map
    #(assoc-in % [1] (clojure.string/split (last %) #"x"))
    (map
      #(assoc-in % [0] (clojure.string/split (first %) #","))
      (map
        #(assoc-in % [0] (apply str (drop-last (first %))))
        (map
          #(into [] (take-last 2 %)) x)))))



(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn part2-parse2 [x]
  (map
    #(assoc-in % [2] (clojure.string/split (last %) #"x"))
    (map
      #(assoc-in % [1] (clojure.string/split (second %) #","))
      (map
        #(assoc-in % [1] (apply str (drop-last (second %))))
        (map
          #(into [] (drop-nth 1 %)) x)))))


(defn get-coords [x y x' y']
  (for [i (range x (+ x x'))
        j (range y (+ y y'))]
    [i j]))
(defn part2-get-coords [n x y x' y']
  (for [i (range x (+ x x'))
        j (range y (+ y y'))]
    {[i j] n}))

(defn make-flat [x] (map #(flatten %) x))
(defn to-vector [x] (map #(into [] %) x))
(defn to-coords [x]
  (map
    #(let [x (Integer/parseInt (% 0))
           y (Integer/parseInt (% 1))
           x' (Integer/parseInt (% 2))
           y' (Integer/parseInt (% 3))]
       (get-coords x y x' y'))
    x))
(defn merge-coords [x] (reduce (fn [acc x] (concat acc x)) [] x))
(defn filter-coords [x] (filter (fn [[k v]] (> v 1)) x))
(defn get-first-element [x] (map #(first %) x))

(defn part2-to-coords [x]
  (map
    #(let [n (% 0)
           x (Integer/parseInt (% 1))
           y (Integer/parseInt (% 2))
           x' (Integer/parseInt (% 3))
           y' (Integer/parseInt (% 4))]
       (part2-get-coords n x y x' y'))
    x))


(defn get-p [x]
  (get-first-element
    (filter-coords
      (frequencies
        (map #(into [] %) (map flatten (map keys x)))))))

(defn get-p2 [x]
  (filter-coords (frequencies (map #(into [] %) (map flatten (map keys x))))))


(defn filter-p [x]
  (let [plist (get-p x)]
    (map #((x %)) plist)))

;(defn filter-p [x]
;  (let [plist (get-p x)]
;    ()))

(defn get-unique [x] (set (map (fn [x] (x "n")) x)))
(defn test1 [x] (map (fn [x] ((keys x)) x)))
(defn test2 [x] (map #(into [] %) x))
(defn merge-maps [x]
  (reduce
    (fn [acc m]
      (let [k (first (keys m))
            v (first (vals m))]
        (if (nil? (acc k))
          (conj acc m)
          (assoc acc k (str v "," (acc k)))))) x))

(defn merge-maps2 [x]
  (reduce
    (fn [acc m] (conj acc m)) {} x))

(defn final-filter [x]
  (let [plist (get-p x)
        merged (merge-maps x)]
    (map (fn [e] (merged e)) plist)))

(defn dosplit [x] (map #(clojure.string/split % #",") x))
(defn test-include [x] (let [seq (reduce (fn [acc v] (conj acc (str "#" (str v)))) #{} (range 1 1253))] (clojure.set/difference seq x)))


(->> (slurp "/Users/tlonist/Documents/study/joc/src/3.txt")
     (clojure.string/split-lines)
     (parse1)
     (parse2)
     (into [])
     (make-flat)
     (to-vector)
     (to-coords)
     (merge-coords)
     (into [])
     (frequencies)
     (filter-coords)
     (count))



(->> (slurp "/Users/tlonist/Documents/study/joc/src/3.txt")
     (clojure.string/split-lines)
     (parse1)
     (part2-parse2)
     (into [])
     (make-flat)
     (to-vector)
     (part2-to-coords)
     (merge-coords)
     (into [])
     (final-filter)
     (set)
     (into [])
     (dosplit)
     (flatten)
     (set)
     (test-include))
;(get-p-2))
;(keys)
;(frequencies))
;(map into []))
;(filter-p)
;(get-unique))
;(get-answer))
    ;(get-p))
