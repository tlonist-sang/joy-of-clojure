(ns _2)

;2-1
; string으로 이루어진 vector x에서, 각 string의 character의 빈도수를 기록하는 array 리턴
(defn count-frequency [x]
  (reduce
    (fn [acc x] (update-in acc [x] inc))
    (vec (repeat 26 0))
    (map (fn [x] (- x (int \a))) (map int x))))


;2번 나온 것의 개수와 3번 나온 개수를 세기 위한 twos, threes
(def twos 0)
(def threes 0)

(defn count2 [x]
  (if (some? (some #{2} x))
    (def twos (+ twos 1))
    twos))

(defn count3 [x]
  (if (some? (some #{3} x))
    (def threes (+ threes 1))
    threes))

(->> (slurp "/2.txt")
     (clojure.string/split-lines)
     (map char-array) ;string은 character의 collection
     (map count-frequency)
     (map count3)) ;reduce로 바꿀 수 있음


;2-2
;String x와 y에 대해 그 둘의 차이를 제거한 string을 리턴함 ex) "abcf" "abzf" => "abf"
(defn getIdenticalStringWithoutNil [x y] (remove nil? (last (diff x y))))

;길이가 26으로 정해져 있기 때문에, getIdenticalStringWithoutNil의 길이가 25이면 정확히 하나가 다름
(defn getStringDifferByOneCharacter [x]
  (for [i (range (count x))
        j (range (count x))
        :when (> j i)
        :let [v (getIdenticalStringWithoutNil (nth x i) (nth x j))]]
    (if (= 25 (count v))
      v)))

(->> (slurp "/2.txt")
     (clojure.string/split-lines)
     (map char-array)
     (getStringDifferByOneCharacter)
     (remove nil?)
     (flatten)
     (into [])
     (apply str))