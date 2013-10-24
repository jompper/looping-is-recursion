(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b e n]
                 (if (zero? e)
                   n
                   (recur b (dec e) (* b n))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-set]
                 (if (or (empty? (rest a-set)) (empty? a-set))
                   (first a-set)
                   (recur (rest a-set))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                  (and (empty? a-seq) (empty? b-seq)) true
                  (or (empty? a-seq) (empty? b-seq)) false
                  (== (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
                  :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [s a-seq i 0]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (rest s) (inc i)))))

(defn avg [a-seq]
  (loop [s 0 c 0 a a-seq]
    (if (empty? a)
      (/ s c)
      (recur (+ s (first a)) (inc c) (rest a)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [t #{} a a-seq]
    (if (empty? a) t
      (recur (toggle t (first a)) (rest a))))))

(defn fast-fibo [n]
  (loop [i n e 0 c 1]
    (if (zero? i) e (recur (dec i) c (+ e c)))))

(defn cut-at-repetition [a-seq]
    (loop [c [] a a-seq u #{}]
    (if
      (or (empty? a) (contains? u (first a)))
      c
      (recur (conj c (first a)) (rest a) (conj u (first a))))))

