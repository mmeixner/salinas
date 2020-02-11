(ns salinas.tuning
  (:require [salinas.math :as m]))

(defn cents
  "Returns cent value of <interval> `p`.
  If given a single number it is considered a <cent> value already and returned unaltered."
  [p]
  (if (number? p)
      p
      (* 1200 (m/log2 (apply / p)))))

(defn cents->freq
  "Returns frequency of note `cent` away from `base-freq`."
  [base-freq cent]
  (* base-freq (m/pow2 (/ cent 1200))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An <interval> (which can also define a pitch, if it is applied from a fixed
;; pitch origin) is a map of key/values. It can be either
;; A <ratio> of two integers {:ratio [a b]}, where (a >= b), and the
;; corresponding :cent value:
;; {:ratio [2 1], :cents 1200}
;; or just a :cents value:
;; {:cents 700}
;; Each interval will have a :cents key.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Maybe TODO: spec for this? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interval
  "Creates an <interval> from either:
    - a single number -> {:cents ...}
    - two integers    -> {:ratio [...] :cents ...} *)

  *) The order of the two integers doesn't matter, the :ratio
  is stored in 'up' position (rising interval)."
  ([x] {:cents (m/abs x)})
  ([a b]
   (let [r (vec (m/simplify (sort > [a b])))]
    {:ratio r
     :cents (cents r)})))

(def unison
  "In the context of musical intervals this is the neutral element."
  (interval 1 1))

(defn down
  "Returns downward version of interval `p`.
  If the source <interval> includes a <ratio>, the returned <interval> will also have one.
  With no argument supplied returns a unison."
  ([] unison)
  ([p]
   (if (:ratio p)
     (assoc p :ratio (vec (reverse (:ratio p)))
              :cents (- (cents (:ratio p))))
     (update p :cents -))))

(defn chain
  "Calculate intervallic 'sum' of `ps`.
  Returns a new <interval>
  With no arguments supplied returns a unison."
  ([] unison)
  ([& ps]
   (if (every? :ratio ps)
     (let [r (->> ps
                  (map :ratio)
                  (apply m/multiply-ratios)
                  m/simplify)]
       {:ratio r :cents (cents r)})
     {:cents (apply + (map (:cents ps)))})))

(defn between
  "Distance between two pitches. Returns <interval>"
  [p1 p2]
  (chain p2 (down p1)))

  ;; Example:
  ;; (def nat-third (interval 5 4))
  ;; (def pythogorean-third (interval 81 64))
  ;; (between nat-third pythagorean-third)
  ;; => {:cents 21.50628959671485}
  ;; (which is the syntonic comma, btw)


(defn frac ;; ?TODO: build in 'history' like as in 'detune'
  "Returns interval `p` equally divided by `divider.
   Since an equal division of an interval most probably yields an irrational number,
   the returned <interval> only has a `:cents` key.
   If no `divider` is supplied, returns `p` unaltered.
   With no args supplied returns unison <interval>."
  ([] unison)
  ([p] p)
  ([p divider] (interval (/ (:cents p) divider))))
;; Example:
;; (def fourth-of-synt-comma (frac (interval 81 80) 4))
;; => {:cents 5.376572399178695}

;; helper function(s): direction up/down
;; (defn up)
;; not really needed, because we store all intervals in "up" position already.

(defn detune
  "Detunes `p` by `det` cents. If `p` had a key `:ratio`, it is
  replaced by `:ratio*`, so following calculations will stay irrational
  (`:cents` only).
  This function is useful for tempering intervals.
  You can derive the 'history' of the resulting <interval> from `:ratio-detuned`
  and `:detuned-by` keys."
  [p det]
  (if (:ratio p)
    (-> p
        (dissoc :ratio)
        (assoc  :ratio* (:ratio p))
        (assoc  :detuned-by det)
        (update :cents + det))
    (update p :cents + det)))

(defn normalize
  "Reduces interval `p` to be smaller than an octave (for example to add it to
  a scale definition). The direction of the interval is maintained."
  [p]
  (if-let [r (:ratio p)]
    (loop [[a b] r
           d (apply / r)]
      (cond
        (<= 0.5 d 2.0) (interval a b)
        (< 2.0 d) (recur [a (* 2 b)] (/ a (* b 2)))
        (> 0.5 d) (recur [(* 2 a) b] (/ (* a 2) b))))
    (assoc p :cents (rem (:cents p) 1200))))

(defn chain-n
  "Convenience function: chain <intervals> and normalize the result.
  With no args supplied returns unison."
  [& intervals]
  (normalize (apply chain intervals)))

;; Example:
;; (chain-n fifth fifth third)
;; => {:ratio [45 32], :cents 590.2237155956096}


;; There is a function, which does just this in one go,
;; when only one type of generator interval is involved:
(defn stack
  "Convenience function. Stacks interval `p` `n` times, and reduces
  the tower to an interval <= octave ('normalizing' it).
  With no args supplied returns a unison, with one arg return `p` unaltered."
  ([] unison)
  ([p] p)
  ([n p] (apply chain-n (repeat n p))))
;; Example:
;; (def pythagorean-third (stack fifth 4))
;; => {:ratio [81 64], :cents 407.8200034615497}

(defn temper
  "This function expects a `target` interval and `ch`, a coll of intervals
  (representing tuning steps). It returns the chain with every interval
  in it detuned to match the `target`, in original order."
  [target ch]
  (let [n (count ch)
        c (apply chain ch)
        d (- (:cents target)
             (:cents c))
        corr (/ d n)]
    (mapv
     #(if (pos? (:cents %))
        (detune % corr)
        (detune % (- corr)))
     ch)))

;; Examples:
;; (def pythagorean-comma (stack (interval 2 3) 12))
;; (def equal-tempered-triton (stack (interval 100) 6))

(defn stretch
  "Proportionally stretch (or compress) all <intervals> of coll `ch`
  such that their sum meets the `target` <interval>.
  Returns vector of altered <intervals>."
  [target ch]
  (let [s (/ (:cents target) (:cents (apply chain ch)))]
    (mapv #(hash-map :cents (* s (:cents %))) ch)))
;; use for octave stretching etc.

(defn nearest-x
  "Find nearest value in coll."
  [coll x]
  (apply min-key (partial distance x) coll))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example data: pure intervals
; (def octave (interval 2 1))
; (def fifth  (interval 3 2))
; (def fourth (interval 4 3))
; (def third  (interval 5 4))
;;;;;;;;;;;;;;;;;;;;

;; Ideas:

;; (defn near
;;   "Returns the pitches from `scale` (a coll of pitches), sorted from nearest upwards
;;   from `p`.
;;   Additionally a key `:dev`is added, which gives the deviation of `p`from the scale pitch.
;;   `p`must be given in cents, the pitches in `scale` must have a `:cents` key.
;;   If the third argument `tolerance` (also in cents) is supplied, returns only those items,
;;   which lie inside this tolerance frame."
;;   ([scale p]
;;    (->> scale
;;          (sort-by #(distance p (:cents %)))
;;          (mapv #(assoc % :dev (- p (:cents %))))
;;        ([scale p tolerance
;;          (->> (near scale p
;;                 (filterv #(> tolerance (abs (:dev %))))))]))))
;;
;; (defn nearest
;;   [scale p]
;;   (first (near scale p)))
