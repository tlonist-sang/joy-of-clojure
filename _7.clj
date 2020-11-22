(ns _7)

(defn split-empty-space [x] (map (fn [v] (clojure.string/split v #" ")) x))
(defn extract [x] (map (fn [v] (conj (conj [] (nth v 1)) (nth v 7))) x))
(defn unique-letters [x] (into #{} (flatten x)))
(defn parse-info [x] (let [uniques (reduce (fn [acc, x] (assoc acc x {:count 0 :visited 0 :to []})) {} (into [] (unique-letters x)))]
                       (reduce
                         (fn [acc v] (let [info1 (acc (first v))
                                           info2 (acc (second v))
                                           to ((acc (first v)) :to)
                                           count ((acc (second v)) :count)]
                                       (assoc acc (first v)
                                                  (assoc info1 :to
                                                               (conj to (second v)))
                                                  (second v)
                                                  (assoc info2 :count (+ 1 count)))))
                         uniques x)))

(defn make-data [x] {:raw x :list []})
(defn make-initial-list [x] {:raw (x :raw)
                             :list
                                  (into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw))))
                             :newItem
                                  (into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw))))})

(defn generate-list [x] (let [raw (x :raw)
                              list (x :list)
                              current ((raw (first (x :newItem))) :to)
                              currentIdx (.indexOf list (first (x :newItem)))
                              newRaw (reduce (fn [acc v] (assoc acc v (update (acc v) :visited inc))) raw current)
                              newList (sort
                                        (into []
                                              (clojure.set/difference
                                                (into #{} (reduce (fn [acc [k v]] (if (= (v :count) (v :visited)) (conj acc k) acc)) [] newRaw))
                                                (into #{} list))))]
                             (prn (reduce (fn [acc v] (str acc v)) "" list))
                          {:raw  newRaw
                           :list (into []
                                       (flatten
                                            (conj (subvec list 0 (inc currentIdx))
                                                  (sort
                                                     (into [] (flatten (conj
                                                                        (subvec list (inc currentIdx)) newList)))))))
                           :newItem
                                 (into [] (sort (flatten (conj newList (into [] (rest (x :newItem)))))))}))

(defn generate-nth [x] (loop [input x]
                            (when (> (count (input :newItem)) 0)
                                 (recur (generate-list input)))))


(->> (slurp "/Users/tlonist/Documents/study/joc/src/7.txt")
     (clojure.string/split-lines)
     (split-empty-space)
     (into [])
     (extract)
     (parse-info)
     (make-data)
     (make-initial-list)
     (generate-nth))


;part2
(->> (slurp "/Users/tlonist/Documents/study/joc/src/7.txt")
     (clojure.string/split-lines)
     (split-empty-space)
     (into [])
     (extract)
     (parse-info)
     (make-data)
     (make-initial-list)
     (generate-nth))