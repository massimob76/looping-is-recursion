(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (if (zero? exp)
                   acc
                   (recur base (dec exp) (* acc base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [rst (rest a-seq)]
    (if (empty? rst)
      (first a-seq)
      (recur rst))))

(defn seq= [seq1 seq2]
  (let [rst1 (rest seq1)
        rst2 (rest seq2)]
    (cond
      (and (empty? rst1) (empty? rst2)) true
      (or (empty? rst1) (empty? rst2)) false
      (= (first seq1) (first seq2)) (recur rst1 rst2)
      :else false)))

(defn find-first-index [pred a-seq]
  (loop [s a-seq idx 0]
    (cond
      (empty? s) nil
      (pred (first s)) idx
      :else (recur (rest s) (inc idx)))))

(defn avg [a-seq]
  (loop [s a-seq
         total 0
         idx 0]
    (cond
      (empty? s) (/ total idx)
      :else (recur (rest s) (+ total (first s)) (inc idx)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{}
         s a-seq]
    (cond
      (empty? s) result
      :else (recur (toggle result (first s)) (rest s)))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    :else (loop [n-loop 1
                 f-n  1
                 f-n1 0]
            (if (= n-loop n)
              f-n
              (recur (inc n-loop) (+ f-n f-n1) f-n)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         result []]
    (cond
      (empty? s) result
      (some #{(first s)} result) result
      :else (recur (rest s) (conj result (first s))))))
