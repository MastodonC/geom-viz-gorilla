(ns geom-viz-gorilla.plot-parameters
  (:require [thi.ng.geom.svg.core :as svg]))

(defn calc-width-of-bar
  [x-data]
  (cond
    (<= (count x-data) 5) 70
    (and (> (count x-data) 5) (<= (count x-data) 20)) 20
    :else 10))

(defn calc-y-range [plot-height]
  [(- plot-height 40) 20])

(defn calc-x-domain [x-data]
  (let [numeric? (every? number? x-data)
        lower-x  (if numeric?
                   (let [min-x (reduce min x-data)]
                     (if (zero? min-x) 0 (dec min-x)))
                   0)
        upper-x  (if numeric?
                   (inc (reduce max x-data))
                   (inc (count x-data)))]
    [lower-x upper-x]))

(defn calc-y-domain [y-data]
  (let [lower-y (let [min-y (reduce min y-data)]
                  (if (neg? min-y) (+ min-y (* 0.2 min-y)) 0))
        upper-y (let [max-y (reduce max y-data)] (+ (* 0.2 max-y) max-y))]
    [lower-y upper-y]))

(defn calc-flip-point
  [flip-point plot-height y-data]
  (let [[top-y bottom-y] (calc-y-range plot-height)
        [lower-y upper-y] (calc-y-domain y-data)
        fp-ratio (- 1 (/ (- flip-point lower-y) (- upper-y lower-y)))
        range-ratio-applied (+ (* (- top-y bottom-y) fp-ratio) bottom-y)]
    range-ratio-applied))

(defn shape [flip-point flip-point-height]
  (fn [[ax ay :as a] [bx by :as b] [domain-x domain-y]]
    (if (> flip-point domain-y)
      (let [diff (- ay flip-point-height)
            new-a [ax flip-point-height]
            new-b [bx (+ flip-point-height diff)]]
        (svg/line new-a new-b))
      (svg/line a [bx flip-point-height]))))

(defn major
  "finding the power of 10 that is the unit we want just below the given number"
  [x]
  (reduce
   (fn [acc el]
     (if (and (<= el x) (> (* 10 x) el))
       el
       acc))
   1
   (map #(Math/pow 10 %) (range 0 10))))

(defn calc-major-minor
  [lower upper]
  (let [major (major (max (Math/abs lower) (Math/abs upper)))]
    [major (/ major 2)]))
