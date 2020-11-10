(ns _4)

(defn separate [x] (map (fn [y] [(subs y 6 17) (subs y 19)]) x))
(defn create-schedule [x]
  (reduce
    (fn [acc v] (let [state (nth v 1)]
                  (cond (clojure.string/starts-with? state "Guard")
                        (let [num (nth (clojure.string/split state #" ") 1)]
                          (if (nil? (acc num))
                            (assoc (conj acc {num (vec (repeat 60 0))}) "curr" num)
                            (assoc acc "curr" num)))
                        (clojure.string/starts-with? state "falls")
                        (let [start (Integer/parseInt (subs (nth (clojure.string/split (nth v 0) #" ") 1) 3 5))]
                          (assoc acc "start" start))
                        (clojure.string/starts-with? state "wakes")
                        (let [start (acc "start")
                              end (Integer/parseInt (subs (nth (clojure.string/split (nth v 0) #" ") 1) 3 5))]
                          (assoc acc (acc "curr")
                                     (into [] (doall (map-indexed
                                                       (fn [idx x] (if (and (>= idx start) (< idx end)) (inc x) x))
                                                       (acc (acc "curr"))))))))))
    {} x))

(defn remove-unnecessary-keys [x] (dissoc (dissoc x "start") "curr"))
(defn parse [x] {"raw" x "freq" (reduce
                                  (fn [acc [k v]]
                                    (conj acc {k (reduce + v)}))
                                  {}
                                  x)})

(defn get-max-freq-id [x] (assoc x "freq" (key (last (sort-by second (into {} (x "freq")))))))
(defn get-max-time [x]
    {"maxtime" (reduce
                 (fn [acc, [k v]] (conj acc {k (.indexOf v (apply max v))}))
                 {}
                 (x "raw"))
     "freq" (x "freq")})
(defn solve [x] (* ((x "maxtime") (x "freq")) (Integer/parseInt (subs (x "freq") 1))))


(defn parse-part2 [x] {"raw" x "freq" (reduce
                                        (fn [acc [k v]]
                                          (conj acc {k {(.indexOf v (apply max v)) (apply max v)}}))
                                        {}
                                        x)})



(->> (slurp "/Users/tlonist/Documents/study/joc/src/4.txt")
     (clojure.string/split-lines)
     (sort)
     (separate)
     (into [])
     (create-schedule)
     (remove-unnecessary-keys)
     (parse)
     (get-max-freq-id)
     (get-max-time)
     (solve))

(->> (slurp "/Users/tlonist/Documents/study/joc/src/4.txt")
     (clojure.string/split-lines)
     (sort)
     (separate)
     (into [])
     (create-schedule)
     (remove-unnecessary-keys)
     (parse-part2))