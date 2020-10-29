(ns aoc2018_1_answers)
;aoc2018-1
(map (fn [x] (+ x 2)) [1 2 3])                              ; => (3 4 5)
(reduce (fn [acc x] (+ acc x)) [1 2 3 4 5])                 ; => 15, acc에 값이 축적
(first [1 2 3 4])                                           ; 1
(last [1 2 3 4])                                            ;4
(next [1 2 3 4])                                            ;[2 3 4]
(rest [1 2 3 4])                                            ;[2 3 4]. rest는 next보다 더 lazy

;1-1
(->> (slurp "/1.txt")                                    ;후위 연산자
     (clojure.string/split-lines)                           ;(clojure.string/split-lines x)와 동일
     (map #(Integer/parseInt %))                            ;#()는 익명함수, %는 인자(parameter)
     (reduce +))

;1-2
(->> (slurp "/1.txt") ;후위 연산자
     (clojure.string/split-lines)                           ;(clojure.string/split-lines x)와 동일
     (map #(Integer/parseInt %))                            ;#()는 익명함수, %는 인자(parameter)
     (def x))

;vector 사용
(defn solve [n s x h]
  (if (nil? (some #{s} h))
    (recur (inc n) (+ s (nth x (mod n (count x)))) x (conj h s))
    (println s)))

;solve2, vector 대신 map 사용, 시간 단축
(defn solve2 [n s x m]
  (if (m s)
    s
    (let [n' (inc n)
          s' (+ s (nth x (mod n (count x))))
          m' (assoc m s true)]
      (recur n' s' x m'))))


;solve3, arr 에 (cycle x) 사용
(defn solve3 [sum arr m]
  (if (m sum)
    sum
    (let [sum' (+ sum (first arr))                          ;let 을 쓰고자 하는 범위는 ()로 명시.
          m' (assoc m sum true)]                        ;map에 {sum: true}인 key-value 쌍을 삽입
      (recur sum' (next arr) m'))))                         ;map에서 키가 sum인것을 찾아 리턴, 없으면 nil. clojure에서 nil은 false와 동치
;recur는 항상 tail(마지막 evaluate)에 와야하며, 정의된 함수와 같은 parameter를 가진다.



