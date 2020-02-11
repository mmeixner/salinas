(ns salinas.math)

(defn abs [x] (if (neg? x) (- x) x))

(def pow
  #? (:clj  #(Math/pow %1 %2)
      :cljs #(.pow js/Math %1 %2)))

(def pow2
  (partial pow 2))

(def log
  #?(:clj  #(Math/log %)
     :cljs #(.log js/Math %)))

(def log2
  #?(:clj  #(/ (log %) (log 2))
     :cljs #(.log2 js/Math %)))

(defn distance [x y]
  (abs (- x y)))

;;;; TODO cljs version
(defn round
  "Round `x` to `precision` number of decimal places.
  If `precision` is not supplied, rounds to integer."
  ([x] (Math/round x))
  ([precision x]
   (->> x
        (* (pow 10 precision))
        Math/round
        (* (pow 10 (- precision))))))

;;;; Ratios
;;;; NB: Since cljs has no notion of a rational number, we spin our own implementation
;;;; A ratio is a vector of two integers, for example [3 2] ("3/2").

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn simplify
  "Returns simplified ratio.
  If p is a single number it is returned unchanged."
  [p]
  (if (number? p)
    p
    (mapv #(/ % (apply gcd p)) p)))

(defn multiply-ratios
  ([] [1 1])              ;; with no argument returns ratio [1 1] (neutral element)
  ([& ps]
   (vector (apply * (map first  ps))
           (apply * (map second ps)))))
