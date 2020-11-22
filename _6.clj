(ns _6)

(defn get-min-max [x]
  (reduce
    (fn [acc, v]
      (assoc acc
        :xmax (max (v :x) (acc :xmax))
        :xmin (min (v :x) (acc :xmin))
        :ymax (max (v :y) (acc :ymax))
        :ymin (min (v :y) (acc :ymin))))
    {:raw   x
     :xmin  10000
     :xmax  0
     :ymin  10000
     :ymax  0
     :map   (reduce (fn [acc v] (assoc acc (str (v :x) "," (v :y)) (v :id))) {} x)
     :count 0}
    x))

(defn prosper [x y id xmax xmin ymax ymin accmap original]
  (let [delta [[0 1] [0 -1] [1 0] [-1 0]]
        preexist original]
    (reduce
      (fn [acc, v]
        (let [pos (str (+ x (first v)) "," (+ y (second v)))]
          (if (or
                (> (+ x (first v)) xmax)
                (< (+ x (first v)) xmin)
                (> (+ y (second v)) ymax)
                (< (+ y (second v)) ymin)
                (some? (preexist pos)))
            acc
            (if (nil? (acc pos))
              (assoc acc pos id)
              (if (not= (acc pos) id)
                (assoc acc pos \.)
                acc)))))
      accmap
      delta)))

(defn cycle-prosper [x]
  (let [{xmin :xmin, xmax :xmax, ymin :ymin, ymax :ymax accmap :map} x]
    (reduce (fn [acc v] (prosper (v :x) (v :y) (v :id) xmax xmin ymax ymin acc (x :map))) accmap (x :raw))))

(defn update-map [x]
  (let [newVals (cycle-prosper x)
        newPos (into [] (map (fn [k] {:x  (Integer/parseInt (first (clojure.string/split k #",")))
                                      :y  (Integer/parseInt (second (clojure.string/split k #",")))
                                      :id (newVals k)})
                             (into []
                                   (remove nil?
                                           (first
                                             (diff (into #{} (keys newVals)) (into #{} (keys (x :map)))))))))]
    ;(prn ":map->" (x :map) ":raw->" (x :raw))
    ;(prn ":newVal->" newVals ":newPos->" newPos)

    (assoc x :map (conj (x :map) newVals)
             :raw newPos)))


(defn frequency-map [x] (frequencies (vals (x :map))))

(defn find-max-entry [x]
  (reduce (fn [acc [k v]] (if (< (acc :v) v) (assoc acc :k k :v v) acc)) {:k 0 :v 0} (x :map)))

(defn update-10-times [x] (nth (iterate update-map x) 300))

(defn sort-maps [x] (sort (x :map)))

;(->> (slurp "/Users/tlonist/Documents/study/joc/src/6.txt")
;     (clojure.string/split-lines)
;     (map (fn [x] (clojure.string/split x #", ")))
;     (into [])
;     (map-indexed (fn [idx, x] {:x  (Integer/parseInt (first x))
;                                :y  (Integer/parseInt (second x))
;                                :id idx}))
;     (into [])
;     (get-min-max)
;     ;(cycle-prosper)
;     (update-10-times)
;     ;(find-max-entry))
;     (sort-maps)
;     (into [])
;     (map second)
;     (frequencies)
;     (vals)
;     (sort))


(defn abs-dist [a b]
  (+ (if (> (a :y) (b :y)) (- (a :y) (b :y)) (- (b :y) (a :y)))
     (if (> (a :x) (b :x)) (- (a :x) (b :x)) (- (b :x) (a :x)))))

(defn loop-coords [x] (let [{xmin :xmin, xmax :xmax, ymin :ymin, ymax :ymax raw :raw} x]
                        (for [i (range xmin xmax)
                              j (range ymin ymax)]
                          (reduce (fn [acc v] (+ acc (abs-dist v {:x i :y j}))) 0 raw))))



(defn update-test [x] (for [i (range 0 3)
                            j (range 0 3)] (update x :count inc)))



; part-2
(->> (slurp "/Users/tlonist/Documents/study/joc/src/6.txt")
     (clojure.string/split-lines)
     (map (fn [x] (clojure.string/split x #", ")))
     (into [])
     (map-indexed (fn [idx, x] {:x  (Integer/parseInt (first x))
                                :y  (Integer/parseInt (second x))
                                :id idx}))
     (into [])
     (get-min-max)
     (loop-coords)
     (into [])
     (reduce (fn [acc v] (if (> 1000 v) (inc acc) acc)) 0))



