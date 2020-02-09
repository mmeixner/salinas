(ns salinas.tuning)

;; math functions
(defn abs [x] (if (neg? x) (- x) x))

(def floor
  #?(:clj  #(Math/floor %)
     :cljs #(.floor js/Math %)))

(def ceil
  #?(:clj  #(Math/ceil %)
     :cljs #(.ceil js/Math %)))

(def pow
  #? (:clj  #(Math/pow %1 %2)
      :cljs #(.pow js/Math %1 %2)))

(defn pow2 [x]
  (pow 2 x))

(def log
  #?(:clj  #(Math/log %)
     :cljs #(.log js/Math %)))

(def log2
  #?(:clj  #(/ (log %) (log 2))
     :cljs #(.log2 js/Math %)))

(defn distance [x y] (abs (- x y)))

(defn nearest-x
  "Find nearest value in coll."
  [coll x]
  (apply min-key (partial distance x) coll))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An 'interval' (which can also define a pitch, if it is applied from a fixed
;; pitch origin) is a map of key/values:
;; 1) A ratio of two integers {:ratio [a b]}, where (a >= b).
;; 2) A cent value: {:cents 700}
;; (Each interval should also have a :cents key incorporated, like so:
;; {:ratio [2 1], :cents 1200}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO spec for this? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cents
  "Takes a ratio (vector of two numbers) and returns the cent value of the ratio.
   If p is a single number it is considered to be a cent value already and returned unchanged."
  [p]
  (if (number? p)
      p
      (* 1200 (log2 (apply / p)))))

(defn cents->freq
  "Returns frequency of note `cent` away from `base-freq`."
  [base-freq cent]
  (* base-freq (pow2 (/ cent 1200))))


(defn frac
  "Returns interval `p` equally divided by `divider.
   Since an equal division of an interval yields most probably an irrational number,
   the calculation and return value is in cents.
   `p` is a map with key `:cents`.
   If no `divider` is supplied, returns `p` unaltered."
  ([p] p)
  ([p divider] {:cents (/ (:cents p) divider)}))

;; Example:
;; (def fourth-of-synt-comma (frac (interval 81 80) 4))
;; => {:cents 5.376572399178695}

(defn- gcd ;; greatest common denominator.
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn- simplify
  ;;Returns simplified ratio.
  ;;If p is a single number it is considered a cent value and returned unchanged.
  [p]
  (if (number? p)
    p
    (mapv #(/ % (apply gcd p)) p)))

(defn interval
  "Create an interval from either:
    - a single number -> {:cents ...}
    - two integers    -> {:ratio [...] :cents ...}.

  The order of the two integers doesn't matter, the :ratio
  is stored in 'up' position (rising interval)."
  ([x] {:cents x})
  ([a b]
   (let [r (vec (simplify (sort > [a b])))]
    {:ratio r
     :cents (cents r)})))

(defn- multiply-ratios
  ([] [1 1]) ;; with no argument returns [1 1] (neutral element)
  ([& ps]
   (vector (apply * (map first  ps))
           (apply * (map second ps)))))

(defn chain
  "Calculate 'sum' of chain of intervals.
  Returns a new `interval`:
   - a map of either {:ratio […], :cents …}, if all source intervals were ratios
   - or a map {:cents …}."
  [& ps]
  (if (every? :ratio ps)
    (let [r (simplify (apply multiply-ratios (map :ratio ps)))]
      {:ratio r
       :cents (cents r)})
    {:cents (apply + (map :cents ps))}))


;; helper function(s): direction up/down
;; (defn up)
;; not really needed, because we store all intervals in "up" position already.

(defn detune
  "Detunes `p` by `det` cents. If `p` had a key `:ratio`, it is
  replaced by `:ratio-detuned`, so it is not seen by the following
  calculations with `:ratio` (because `p` is no longer rational!)
  Together with key `:detuned` it keeps the 'history' of the interval.
  This function is useful for tempering intervals."
  [p det]
  (if (:ratio p)
    (-> p
        (dissoc :ratio)
        (assoc  :ratio-detuned (:ratio p))
        (assoc  :detuned det)
        (update :cents + det))
    (update p :cents + det)))


(defn down
  "Returns downward version of interval.
  If the source interval has a key `:ratio` returns a map of `{:ratio …, :cents …}`,
  otherwise `{:cents …}`."
  [p]
  (cond
    (:ratio p) (assoc p :ratio (vec (reverse (:ratio p)))
                        :cents (- (cents (:ratio p))))
    (:cents p) (update p :cents -)))

(defn normalize
  "Reduces an interval (vector of two integers) to a ratio smaller than an octave [2:1],
  and simplifies the ratio, if possible.
  If interval is a number, it is considered a cent value and reduced to under an octave.
  The direction of the interval is maintained."
  [p]
  (if-let [r (:ratio p)]
    (loop [a (first  r)
           b (second r)]
      (let [d (/ a b)]
        (cond
          (<= 0.5 d 2.0) {:ratio (simplify [a b])
                          :cents (cents [a b])}
          (<  2.0 d) (recur a (* 2 b))
          (>  0.5 d) (recur (* 2 a) b))))
    (assoc p :cents (rem (:cents p) 1200))))

(defn chain-n
  "Convenience function: chain intervals and normalize
  (i.e. create interval <= octave)."
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
  With no args returns a unison, with one arg return `p` unaltered."
  ([] (interval 1 1))
  ([p] p)
  ([n p] (apply chain-n (repeat n p))))
;; Example:
;; (def pythagorean-third (stack fifth 4))
;; => {:ratio [81 64], :cents 407.8200034615497}


(defn temper
  "Give this function a `target` interval and `ch`, a chain of intervals
  (representing tuning steps). It returns the chain with every interval
  in it detuned to match the `target`, in original order.
  These can be extracted afterwards to store them as tuning intervals."
  [target ch]
  (let [n (count ch)
        c (apply chain ch)
        d (- (:cents target)
             (:cents c))
        corr (/ d n)]
    (mapv
     #(if (pos? (:cents %)) (detune % corr)
          (detune % (- corr)))
     ch)))

;; Examples:
;; (def pythagorean-comma (stack (interval 2 3) 12))
;; (def equal-tempered-triton (stack (interval 100) 6))

(defn stretch
  "Proportionally stretch (or compress) all intervals in `ch` such that their
  sum meets `target` interval. All intervals in `ch` are returned as maps as usual."
  [target ch]
  (let [s (/ (:cents target) (:cents (apply chain ch)))]
    (map #(hash-map :cents (* s (:cents %))) ch)))
;; use for octave stretching etc.

(defn between
  "Distance between two pitches.
  If both pitches have :ratio keys, a map with :ratio and :cents is returned,
  otherwise a map with :cents only."
  [p1 p2]
  (if (every? :ratio [p1 p2])
    (let [[a b] (:ratio p1)
          [c d] (:ratio p2)
          r (simplify [(* b c) (* a d)])]
      {:ratio r :cents (cents r)})
    {:cents (- (:cents p2) (:cents p1))}))

;; Example:
;; (def nat-third (interval 5 4))
;; (def pythogorean-third (interval 81 64))
;; (between nat-third pythagorean-third)
;; => {:cents 21.50628959671485}
;; (which is the syntonic comma, bw...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example data: pure intervals
(def octave (interval 2 1))
(def fifth  (interval 3 2))
(def fourth (interval 4 3))
(def third  (interval 5 4))
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
