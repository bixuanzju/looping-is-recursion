(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [x acc]
                 (if (= x 0) acc
                     (recur (dec x) (* acc base))))]
    (helper exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [x]
                 (cond
                  (empty? x) nil
                  (empty? (rest x)) (first x)
                  :else (recur (rest x))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                  (and (empty? s1) (empty? s2)) true
                  (zero? (* (count s1) (count s2))) false
                  :else (if (= (first s1) (first s2))
                          (recur (rest s1) (rest s2))
                          false)))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ss a-seq
         n 0]
    (cond
     (empty? ss) nil
     (pred (first ss)) n
     :else (recur (rest ss) (inc n)))))

(defn avg [a-seq]
  (loop [sum 0
         ss a-seq]
    (if (empty? ss) (/ sum (count a-seq))
        (recur (+ sum (first ss)) (rest ss)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         ss a-seq]
    (if (empty? ss) acc
        (recur (toggle acc (first ss)) (rest ss)))))

(defn fast-fibo [n]
  (loop [fn1 0
         fn2 1
         x 2]
    (cond
     (= n 0) fn1
     (= n 1) fn2
     (= x n) (+ fn1 fn2)
     :else (recur fn2 (+ fn1 fn2) (inc x)))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         acc []
         ss a-seq]
    (cond
     (or (empty? ss) (a-set (first ss))) acc
     :else (recur (conj a-set (first ss))
                  (conj acc (first ss))
                  (rest ss)))))

