(ns euler-2.core
  (:gen-class))
  ;;* Author: eoncarlyle
  ;;* Date: 8 January 2019
  ;;* Description:
  ;;*   This program calculates the sum of all even Fibonacci numbers that
  ;;*   do not exceed 4 million, and is a solution to the second problem in
  ;;*   the Project Euler collection.

(defn fib [n]
  ;;* fib returns the nth Fibonacci number.

  (cond
    (or (= n 2) (= n 1)) n
    :else (+ (fib (- n 2)) (fib (- n 1)))
  )
)

(defn fib_max_n [fib_max n]
  ;;* fib_max_n returns the value of n such that the (n + 1)th Fibonacci number
  ;;* is greater than fib_max but the nth Fibonacci number is not.

  ([fib_max]
    (cond
      (and (< (fib 1) fib_max) (not (< (fib 2) fib_max))) n
      :else (fib_max_n fib_max 2)
    )
  )
  ([fib_max n]
    (cond
      (and (< (fib n) fib_max) (not (< (fib (+ n 1)) fib_max))) n
      :else (fib_max_n fib_max (+ n 1))
    )
  )
)

(defn fib_vectorizer [n]
  ;;* fib_vectorizer creates a vector of Fibonacci numbers, which
  ;;* begins at 1 and ends at the nth Fibonacci number.

  (def fib_n_value (fib n))
  (cond
    (= n 1) [(fib n)]
    :else (conj (fib_vectorizer (- n 1)) (fib n))
  )
)

(defn sum_even_fib [fib_max]
  ;;* sum_even_fib adds all even Fibonacci numbers that are lower than fib_max.

  (def fib_vector (fib_vectorizer (fib_max_n fib_max)))
  (reduce + ( filter #(= (mod % 2) 0) fib_vector))
)

(defn -main
  [& args]
  (println (sum_even_fib 4000000))
)
