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
(defn make-initial-list [x] {:raw    (x :raw)
                             :list
                                     (into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw))))
                             :newItem
                                     (into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw))))
                             :worker 5
                             :time   (last (into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw)))))})

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


;(->> (slurp "/Users/tlonist/Documents/study/joc/src/7.txt")
;     (clojure.string/split-lines)
;     (split-empty-space)
;     (into [])
;     (extract)
;     (parse-info)
;     (make-data)
;     (make-initial-list) 
;     (generate-nth))


;part2
(defn get-min-entry [x] (reduce (fn [acc [k v]] (if (> (acc :val) v) (assoc acc :c k :val v) acc)) {:c "" :val 999999999} x))
(defn string-to-int [x] (+ 60 (inc (- (int (first (char-array x))) (int (first (char-array "A")))))))
(defn make-initial-list2 [x] {:raw        (reduce (fn [acc [k v]]
                                                    (assoc acc k {
                                                                  :count   (v :count)
                                                                  :visited (v :visited)
                                                                  :to      (into [] (sort (v :to)))
                                                                  :t       (string-to-int k)})) {} (x :raw))
                              :list       []
                              ;(into [] (sort (reduce (fn [acc [k v]] (if (= (v :count) 0) (conj acc k) acc)) [] (x :raw))))
                              :status     {}
                              ;(reduce (fn [acc [k v]] (if (= (v :count) 0) (assoc acc k (string-to-int k)) acc)) {} (x :raw))
                              :num_worker 0
                              ;(count (keys (reduce (fn [acc [k v]] (if (= (v :count) 0) (assoc acc k (string-to-int k)) acc)) {} (x :raw))))
                              :checked    []})



;x 에서 count와 visited가 같은 요소 리턴 {"B" 4 "C" 5}
(defn get-candidates [x] (reduce (fn [acc [k v]]
                                   (if (= (v :count) (v :visited))
                                     (assoc acc k (v :t))
                                     acc))
                                 {}
                                 x))

;candidate에서 가장 작은 entry 리턴 (이때 checked는 제외)
(defn get-candidates-without-checked [x checked] (reduce (fn [acc v] (dissoc acc v)) (get-candidates x) checked))
(defn get-candidates-without-checked-without-status [x checked status] (reduce (fn [acc [k v]] (dissoc acc k)) (get-candidates-without-checked x checked) status))
(defn select-checked [x checked status dissocMinStatus mintime]
  (get-min-entry
   (conj dissocMinStatus (zipmap (keys (get-candidates-without-checked-without-status x checked status))
                                 (map #(+ mintime %) (vals (get-candidates-without-checked-without-status x checked status)))))))


(defn select-n-available [x worker addedTime checked status] (reduce
                                                               (fn [acc v] (assoc acc (first v) (+ addedTime (second v))))
                                                               {} (take (- 5 worker) (sort-by val (get-candidates-without-checked-without-status x checked status)))))

(defn step-forward [x] (let [raw (x :raw)
                             list (x :list)
                             status (x :status)
                             checked (x :checked)

                             zero (get-candidates raw)
                             one (get-candidates-without-checked raw checked)
                             two (get-candidates-without-checked-without-status raw checked status)

                             minimumTime (if (= ((get-min-entry status) :c) "") 0 ((get-min-entry status) :val))
                             minEntry ((get-min-entry status) :c)
                             dissocMinimumFromStatus (dissoc status ((get-min-entry status) :c))
                             numWorker (dec (x :num_worker))

                             newStatus (conj dissocMinimumFromStatus (reduce (fn [acc [k v]] (if (some? (acc k)) (assoc acc k v) acc)) (select-n-available raw numWorker minimumTime checked status) status))

                             checkedEntry (select-checked raw checked status dissocMinimumFromStatus minimumTime)
                             checkedEntryTo ((raw (checkedEntry :c)) :to)
                             newChecked (conj checked (checkedEntry :c))
                             newRaw (reduce (fn [acc v] (assoc acc v (update (acc v) :visited inc))) raw checkedEntryTo)]


                         (prn "========================================================================")
                         (prn "candidates" zero)
                         (prn "checked" checked)
                         (prn "status" status)
                         (prn "candidates minus (checked)" one)
                         (prn "candidates minus (checked and status)" two)
                         (prn)

                         (prn "entry add to checked" checkedEntry)
                         (prn "entries connected to above(visited)" checkedEntryTo)

                         (prn "status" status)
                         (prn "new status" newStatus)

                         (prn "minEntry" minEntry)
                         (prn "original checked" checked)
                         (prn "new checked" newChecked)

                         (prn "worker" (count (keys newStatus)))
                         (prn "goal" (count newRaw))
                         (prn "current" (count newChecked))
                         (prn)

                         {
                          :raw newRaw
                          :checked newChecked
                          :status newStatus
                          :num_worker (count (keys newStatus))}))





;(prn "newRaw" newRaw)))

(defn step-nth [x] (loop [input x]
                     (when (> (count (keys (input :raw))) (count (input :checked)))
                       (recur (step-forward input)))))

(->> (slurp "/Users/tlonist/Documents/study/joc/src/7.txt")
     (clojure.string/split-lines)
     (split-empty-space)
     (into [])
     (extract)
     (parse-info)
     (make-data)
     (make-initial-list2)
     (step-nth))
;     (time-taken))


;(time-taken))
















