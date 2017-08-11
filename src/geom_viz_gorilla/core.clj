(ns geom-viz-gorilla.core
  (:require [gorilla-renderable.core :as render]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core :as m]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]))


(defn export-viz
  [viz path] (->> viz (svg/svg {:width 600 :height 320}) (svg/serialize) (spit path)))

(defn calc-width-of-bar
  [x-data]
  (cond
    (<= (count x-data) 5) 70
    (and (> (count x-data) 5) (<= (count x-data) 20)) 20
    :else 10))

(defn calc-y-range [plot-height]
  [(- plot-height 40) 20])

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

(defn bar-spec
  [data num width flip-point flip-point-height]
  (fn [idx col]
    {:values     data
     :attribs    {:stroke       col
                  :stroke-width (str (dec width) "px")}
     :shape (shape flip-point flip-point-height)
     :layout     viz/svg-bar-plot
     :interleave num
     :bar-width  width
     :offset     idx}))

(defn viz-spec
  [x-data y-data plot-width plot-height]
  (let [lower-x (let [min-x (reduce min x-data)]
                  (if (zero? min-x) 0 (dec min-x)))
        upper-x (inc (reduce max x-data))]
    {:x-axis (viz/linear-axis
              {:domain [lower-x upper-x]
               :range  [50 (- plot-width 20)]
               :major  1
               :pos    (- plot-height 40) ;;lower-y
               :label  (viz/default-svg-label int)})
     :y-axis (viz/linear-axis
              {:domain      (calc-y-domain y-data)
               :range       (calc-y-range plot-height)
               :major       10
               :minor       5
               :pos         50
               :label-dist  15
               :label-style {:text-anchor "end"}})
     :grid   {:minor-y true}}))

(defn bar-chart
  ([x-values y-values]
   (bar-chart x-values y-values {}))
  ([x-values y-values {:keys [plot-color plot-width plot-height]}]
   (println x-values)
   (let [x-min (reduce min x-values)
         x-max (reduce max x-values)
         all-data (map vector x-values y-values)
         bar-width (calc-width-of-bar x-values)
         color-plot (or plot-color "#3D325A")
         width-plot (or plot-width 600)
         height-plot (or plot-height 320)
         flip-point 0 ;; All bars start at 0 (even when y values negative)
         flip-point-height (calc-flip-point flip-point height-plot y-values)
         flip-line {:values [[(dec x-min) 0] [(inc x-max) 0]]
                    :attribs {:fill "none" :stroke "#4c4a4a"}
                    :layout viz/svg-line-plot}
         plot ((bar-spec all-data 1 ;; Plot 1 data series for now
                         bar-width flip-point
                         flip-point-height) 0 ;; Index of data series is 0
               color-plot)
         ;; If y values negative, add line at y = 0
         plot-data (if (neg? (reduce min y-values)) [plot flip-line] [plot])]
     {:plot (-> (viz-spec x-values y-values width-plot height-plot)
                (assoc :data plot-data)
                (viz/svg-plot2d-cartesian))
      :width width-plot
      :height height-plot})))

(defn plot-svg
  [svg-spec width height]
  (->> svg-spec
       (svg/svg {:width width :height height})
       (svg/serialize)))

(defn plot-bar-chart
  [x-values y-values options]
  (let [{:keys [plot width height]} (bar-chart x-values y-values options)]
    (plot-svg plot width height)))

(defrecord GeomViewBarChart [x-values y-values options])

(extend-type GeomViewBarChart
  render/Renderable
  (render [self]
    {:type :html :content (plot-bar-chart (:x-values self) (:y-values self) (:options self)) :value (pr-str self)}))

(defn view-bar-chart
  [x-values y-values options]
  (GeomViewBarChart. x-values y-values options))
