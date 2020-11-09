(ns aoc2018-5-answers)


(defn comp-func [a b] (if (nil? a) false (= 32 (Math/abs (- (int a) (int b))))))
(defn comp-func2 [a b] (if (or (= (- (int a) (int b)) 0) (= (- (int a) (int b)) 32)) true false))

(defn react [x]
  (reduce (fn [acc v]
            (if (comp-func (last acc) v)
              (pop acc)
              (conj acc v)))
          [] x))

; part 1
(->> (slurp "/Users/tlonist/Documents/study/joc/src/5.txt")
     (char-array)
     (seq)
     (into [])
     (react)
     (apply str)
     (count))

(defn remove-specific [x]
  (for [i (range 97 123)]
    (reduce (fn [acc v]
              (if (comp-func2 (char i) v)
                acc
                (conj acc v))) [] x)))

(defn react-multiple [x] (map (fn [v] (apply str (react v))) x))

; part 2
(->> (slurp "/Users/tlonist/Documents/study/joc/src/5.txt")
     (char-array)
     (seq)
     (into [])
     (remove-specific)
     (into [])
     (react-multiple)
     (map count)
     (apply min))