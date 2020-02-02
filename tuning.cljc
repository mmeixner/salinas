(ns intervals.clj)

(defn abs [x]
  (Math/abs x))

(defn log2
  "Log with base 2"
  [x]
  (/ (Math/log x) (Math/log 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An 'interval' (which can also define a pitch, if it is measurd from a fixed
;; origin) can have one of two forms:
;; 1) a ratio, written as vector [a b].
;;    If a is greater than b, the interval is rising, otherwise falling.
;; 2) a number is interpreted as a cent value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cents
  "Takes a ratio (a vector of two numbers) and returns the interval in cents.
   If 'interval' is just a number it is considered to be a cent value already and returned unchanged."
  [interval]
  (if (number? interval)
    interval
    (* 1200 (log2 (apply / interval)))))

(defn multiply-ratios
  "Multiply ratios. With no argument returns [1 1] (neutral element)"
  ([]   [1 1])
  ([& ps]
   (vector (apply * (map first  ps))
           (apply * (map second ps)))))

(defn chain [& intervals]
  "Calculate product of chain of 'intervals'. If all intervals are ratios (vectors),
  returns ratio, otherwise cents value."
  (if (every? vector? intervals)
    (multiply-ratios intervals)
    (apply + (map cents intervals))))

;; two helper functions: direction up/down
(defn interval-up
  "Returns upward version of interval: a ratio if given a ratio, a number otherwise (cent value)."
  [interval]
  (if (vector? interval)
    (sort > interval)
    (abs interval)))

(defn interval-down
  "Returns downward version of interval: a ratio if given a ratio, a number otherwise (cent value)."
  [interval]
  (if (vector? interval)
    (sort < interval)
    (- (abs interval))))

(defn gcd
  "Greatest common denominator."
  [a b
        (if (zero? b)
          a
          (recur b (mod a b)))])

(defn simplify
  "Returns simplified ratio.
  If interval is a number it is considered a cent value and returned unchanged."
  [interval]
  (if (number? interval)
    interval
    (mapv #(/ % (apply gcd ratio)) ratio)))

(defn up
  "Returns one or more intervals of the same type upwards in a row (for example a series of fifths).
  '(up <interval>)' is interpreted as  '(up 1 <interval>)'."
  ([interval] (up 1 interval))
  ([n interval] (vec (apply chain (repeat n (interval-up interval))))))

(defn down
  "Returns one or more intervals of the same type downwards in a row (for example a series of fifths).
  '(down <interval>)' is interpreted as  '(down 1 <interval>)'."
  ([interval] (down 1 interval))
  ([n interval] (vec (apply chain (repeat n (interval-down interval))))))

(defn de-oct
  "Reduces an interval (vector of two integers) to a ratio smaller than an octave (2:1),
  and simplifies the ratio, if possible.
  If interval is a number, it is considered a cent value and reduced to under an octave.
  The direction of the interval is maintained."
  [interval]
  (if (number? interval)
    (rem interval 1200)

    (let [a (first interval)
          b (second interval)]
      (if (<= 0.5 (/ a b) 2.0)
        (simplify [a b])
        (if (> a b)
          (de-oct [a (* 2 b)])
          (de-oct [(* 2 a) b]))))))



(defn between
  "Distance between two pitches.
  If both pitches are ratios, a ratio is returned, otherwise a cent value."
  [p1 p2]
  (if (every? vector? [p1 p2])
    (simplify [(* (second p1) (first p2))
               (* (first p1)  (second p2))])
    (- (cents p2) (cents p1))))

;; Example:
;; (def nat-third [5 4])
;; (def pythogorean-third [81 64])
;; (between nat-third pythagorean-third)
;; => [81 80]


(defn tune-note
  "Adds a tuning step to a given note. The new note has it's tuning history in it's :chain."
  [note interval]
  (assoc note :chain (conj (:chain note) interval)))

(defn near?
  "Returns true, if the interval between p1 and p2 is equal or smaller than 'cent'.
  Beware: the direction of the intervals matters!"
  [cent p1 p2]
  (>= cent (Math/abs (- (cents p1) (cents p2)))))

;; Do we need this one?:
(defn assoc-ratio ; name?
  [note]
  (assoc note :ratio
              (simplify (apply chain (:chain note)))))


;; little helper to start tuning a scale:
(defn first-note [] {:chain [[1 1]]})
;; example usage:
;; (def c (first-note))
;; => {:chain [[1 1]]}


(defn distance
  "Distance between to numbers."
  [x y] (abs (- x y)))



;;; TODO: decide which to keep (or both?) Use cases?
;; #1
(defn nearest
  "Find nearest in coll (of pitches).
  Internally all values are converted to cent values."
  [pitch pitches]
  (first (sort-by #(distance x %) (map cents pitches))))

;;#2
(defn nearest-pitch
  "Find"
  [pitch pitches]
  (let [i (->> (map-indexed #(vector %1 (distance (cents %2) (cents pitch))) pitches)
               (sort-by second)
               ffirst)]
    (get pitches i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  (def octave [2 1])
  (def fifth  [3 2])
  (def fourth [4 3])
  (def third  [5 4])

  (def great-WT (de-oct (up 2 fifth)))
  (def small_WT (simplify (chain (up third)
                                 (down great-WT))))

  (def pyth-comma (simplify (de-oct (up 12 fifth))))
  (def pyth-third (simplify (de-oct (up 4 fifth))))
  (def synt-comma (chain (up pyth-third) (down third)))

  (cents pyth-third)
  (cents pyth-comma)
  (cents synt-comma))
