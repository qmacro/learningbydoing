(ns learningbydoing.core
  (:gen-class))

; Project Euler #2 - Even Fibonacci Numbers
; -----------------------------------------

; loop/recur - still thinking mechanically & procedurally
; (Don't do this!)

(comment
(loop [a 0
       b 1
       n 0]
  (let [c (+ a b)]
    (if (< n 10)
      (do
          (print c)
          (recur b c (inc n))))))
)

; Gratuitous recursion - and losing sight of the problem
; (Don't do this!)

(defn nth-fib
  "Returns the nth Fibonacci sequence value."
  [n]
  (if (or (= 0 n)
          (= 1 n))
    n
    (+ (nth-fib (- n 1))
       (nth-fib (- n 2)))))

(comment (nth-fib 10))
(comment (map nth-fib (range 10)))


; Calmness - simple function & building sequences

(defn next-fib-pair
  "Given a pair of consecutive values in the Fibonacci
  sequence, return the next overlapping pair."
  [[a b]]
  [b (+ a b)])

(comment (next-fib-pair [0 1]))
(comment (take 10 (iterate next-fib-pair [0 1])))
(comment (map first (take 10 (iterate next-fib-pair [0 1]))))


; Rearranging - for reuse and understanding

(comment (map first (take 10 (iterate next-fib-pair [0 1]))))

(comment (take 10 (map first (iterate next-fib-pair [0 1]))))

(comment (->> (iterate next-fib-pair [0 1])
              (map first)
              (take 10)))


; Process chain - adding to the pipeline

(def fibs (map first (iterate next-fib-pair [0 1])))

(comment (take 10 fibs))

(comment (->> fibs
              (take-while #(< % 4000000))))

(comment (->> fibs
              (take-while #(< % 4000000))
              (filter even?)))


; Simplicity - a final reduce gets us to the solution

(comment (->> fibs
              (take-while #(< % 4000000))
              (filter even?)
              (reduce +)))


; Project Euler #22 - Names Scores
; --------------------------------

; Marshall the data - let the dog see the rabbit

(def names
  (->> (slurp "resources/names.txt")
       (re-seq #"\"(\w+)\"")
       (map second)
       (sort)))

(comment
(take 3 names)
)

; Helper - calculate the value of a char (A=1, B=2 etc)

(comment
(seq "ABC")
(int \A)
)

(defn char-val
  [char]
  (- (int char)
     64))

(comment
(map char-val "ABC")
)

; Helper - sum of char values times position in list

(defn name-score
  [name pos]
  (* (inc pos)
     (reduce + (map char-val name))))

(comment
(map char-val "ABE")
(name-score "ABE" 1)
)

; Solution - the sum of all the name scores

(comment
(reduce
  +
  (map name-score
       names
       (range)))
)


; 4Clojure #21 - Nth Element
; --------------------------


; Comparing Solutions - recursive head/tail approach

(defn my-nth
  [s n]
  (if (= 0 n)
    (first s)
    (my-nth (rest s) (dec n))))

(comment
(= (my-nth '(4 5 6 7) 2) 6)
)

; Comparing Solutions - minimal list-oriented approach

(comment
(= (#(last (take (inc %2) %1)) '(4 5 6 7) 2) 6)
)


; Triangular Numbers via recursion

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(comment
(def tri (tri*))
(take 10 tri)
)

; Triangular Numbers via reductions

(comment
(take 10 (reductions + (range)))
)


; Advent of Code Day 10 - Elves Look, Elves Say

; Say what you see - first attempt
; (Not Ideal!)

(defn say
  [saying]
  (let [digits (partition-by identity saying)]
    (->> digits
         (map (fn [d] (vector (count d) (first d))))
         (apply concat)
         (apply str))))

(comment
(say "3113322113")
)

; Clean input makes for clean processing

(defn char-to-digit [c] (- (int c) 48))

(comment
(char-to-digit \5)
)

(defn string-to-digits [s] (map char-to-digit s))

(comment
(string-to-digits "3113322113")
)

; With cleaner input you can work more calmly


(def myinput (string-to-digits "3113322113"))

(comment
(partition-by identity myinput)

(->> (partition-by identity myinput)
     (map #(vector (count %) (first %))))

(->> (partition-by identity myinput)
     (map #(vector (count %) (first %)))
     flatten)
)

; Now we can iterate and get to the solution

(defn say
  [digits]
  (->> (partition-by identity digits)
       (map #(vector (count %) (first %)))
       (flatten)))

(comment
(count (last (take 41 (iterate say myinput))))
)

