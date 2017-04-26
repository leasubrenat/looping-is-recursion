(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1 exp exp]
    (if (zero? exp)
      acc
      (recur (* base acc) (dec exp)))))

(defn last-element [a-seq]
 (if (empty? a-seq)
    nil
    (if (not (empty? (rest a-seq)))
      (last-element (rest a-seq))
      (first a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (= a-seq b-seq)
      true
    :else
      false))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         n 0]
    (if (not (empty? acc))
      (if (pred (first acc))
        n
        (recur (rest acc) (inc n)))
      nil)))

(defn avg [a-seq]
  (loop [acc a-seq
         sum 0
         n 0]
    (if (empty? acc)
      (/ sum n)
      (recur (rest acc) (+ sum (first acc)) (inc n)))))

(defn parity [a-seq]
  (set
   (keep
    (fn [[k v]]
      (when (odd? v) k))
    (frequencies a-seq))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         acc n]
    (if (= acc 0)
      a
      (recur (+ a b) a (dec acc)))))

(defn cut-at-repetition [a-seq]
  (loop [r []
         rest-seq a-seq
         acc (count a-seq)]
    (if (= acc 0)
      r
      (if (contains? (set r) (first rest-seq))
          r
          (recur (conj r (first rest-seq)) (rest rest-seq) (dec acc))))))

