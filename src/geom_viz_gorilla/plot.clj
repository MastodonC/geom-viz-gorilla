(ns geom-viz-gorilla.plot
  (:require [geom-viz-gorilla.plot-parameters :refer :all]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]))

;; bar plot

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

(defn bar-viz-spec
  [x-data y-data plot-width plot-height {:keys [vertical-x-labels x-major x-minor y-major y-minor]}]
  (let [numeric? (every? number? x-data)
        lower-x (if numeric?
                  (let [min-x (reduce min x-data)]
                    (if (zero? min-x) 0 (dec min-x)))
                  0)
        upper-x (if numeric?
                  (inc (reduce max x-data))
                  (inc (count x-data)))
        [lower-y upper-y] (calc-y-domain y-data)
        [major-y minor-y] (calc-major-minor lower-y upper-y)
        numeric-fn int
        text-fn (fn [x] (str (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) "")))
        label-fn (if numeric? numeric-fn text-fn)
        label-string-fn (fn [x] (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) ""))
        vertical-label-fn (fn [f] (fn [p x] [:g {:writing-mode "tb-rl" :transform (str "translate(0,"  (* 2 (count (label-string-fn x)))  ")")} (svg/text p (str (label-string-fn x)))]))]
    {:x-axis (viz/linear-axis
              {:domain [lower-x upper-x]
               :range  [50 (- plot-width 20)]
               :major  (or x-major 1)
               :minor (or x-minor 1)
               :pos    (- plot-height 40) ;;lower-y
               :label  (if vertical-x-labels
                         (vertical-label-fn label-fn)
                         (viz/default-svg-label label-fn))})
     :y-axis (viz/linear-axis
              {:domain      [lower-y upper-y]
               :range       (calc-y-range plot-height)
               :major       (or y-major (if (> major-y 1) (int major-y) major-y))
               :minor       (or y-minor (if (> minor-y 1) (int minor-y) minor-y))
               :pos          50
               :label-dist  15
               :label-style {:text-anchor "end"}})
     :grid   {:minor-y true}}))

(defn bar-chart
  ([x-values y-values]
   (bar-chart x-values y-values {}))
  ([x-values y-values {:keys [plot-color plot-width plot-height vertical-x-labels svg-height] :as options}]
   (let [x-numeric? (every? number? x-values)
         x-min (if x-numeric?
                 (reduce min x-values)
                 0)
         x-max (if x-numeric?
                 (reduce max x-values)
                 (inc (count x-values)))
         all-data (map vector (if x-numeric?
                                x-values
                                (range 1 (inc (count x-values)))) y-values)
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
     {:plot (-> (bar-viz-spec x-values y-values width-plot height-plot options)
                (assoc :data plot-data))
      :width width-plot
      :height (or svg-height height-plot)})))

;; line plot

(defn viz-line
  [x-data y-data {:keys [vertical-x-labels]}]
  (let [numeric? (every? number? x-data)
        lower-x (if numeric?
                  (let [min-x (reduce min x-data)]
                    (if (zero? min-x) 0 (dec min-x)))
                  0)
        upper-x (if numeric?
                  (inc (reduce max x-data))
                  (inc (count x-data)))
        numeric-fn int
        text-fn (fn [x] (str (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) "")))
        label-fn (if numeric? numeric-fn text-fn)
        label-string-fn (fn [x] (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) ""))
        vertical-label-fn (fn [f] (fn [p x] [:g {:writing-mode "tb-rl" :transform (str "translate(0,"  (* 2 (count (label-string-fn x)))  "`)")} (svg/text p (str (label-string-fn x)))]))
        lower-y (let [min-y (reduce min y-data)]
                  (if (zero? min-y) 0 (dec min-y)))
        upper-y (inc (reduce max y-data))]
    {:x-axis (viz/linear-axis
              {:domain [lower-x upper-x]
               :range  [50 500]
               :major  (int (Math/floor (/ upper-x 3)))
               :minor  (/ PI 4)
               :pos    250
               :label-dist  30
               :label  (if vertical-x-labels
                         (vertical-label-fn label-fn)
                         (viz/default-svg-label label-fn))})
     :y-axis (viz/linear-axis
              {:domain      [lower-y upper-y]
               :range       [250 20]
               :major       (int (Math/floor (/ upper-y 3)))
               :minor       1
               :pos         50
               :label-dist  15
               :label-style {:text-anchor "end"}})
     :grid   {:attribs {:stroke "#caa"}
              :minor-y true}
     :data [{:values (->> (interleave  (if numeric? x-data (range 1 (inc (count x-data)))) y-data)
                          (partition 2)
                          (map vec))
             :attribs {:fill "none" :stroke "#0af"}
             :layout viz/svg-line-plot}]}))

;; scatter plot

(defn viz-scatter
  [x-data y-data {:keys [vertical-x-labels]}]
  (let [numeric? (every? number? x-data)
        lower-x (if numeric?
                  (let [min-x (reduce min x-data)]
                    (if (zero? min-x) 0 (dec min-x)))
                  0)
        upper-x (if numeric?
                  (inc (reduce max x-data))
                  (inc (count x-data)))
        numeric-fn int
        text-fn (fn [x] (str (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) "")))
        label-fn (if numeric? numeric-fn text-fn)
        label-string-fn (fn [x] (if (and (> x lower-x) (< x upper-x)) (nth x-data (dec x)) ""))
        vertical-label-fn (fn [f] (fn [p x] [:g {:writing-mode "tb-rl" :transform (str "translate(0,"  (* 2 (count (label-string-fn x)))  "`)")} (svg/text p (str (label-string-fn x)))]))
        lower-y (let [min-y (reduce min y-data)]
                  (if (zero? min-y) 0 (dec min-y)))
        upper-y (inc (reduce max y-data))]
    {:x-axis (viz/linear-axis
              {:domain [lower-x upper-x]
               :range  [50 500]
               :major  (int (Math/floor (/ upper-x 3)))
               :minor  (/ PI 4)
               :pos    250
               :label-dist  30
               :label  (if vertical-x-labels
                         (vertical-label-fn label-fn)
                         (viz/default-svg-label label-fn))})
     :y-axis (viz/linear-axis
              {:domain      [lower-y upper-y]
               :range       [250 20]
               :major       (int (Math/floor (/ upper-y 3)))
               :minor       1
               :pos         50
               :label-dist  15
               :label-style {:text-anchor "end"}})
     :grid   {:attribs {:stroke "#caa"}
              :minor-y true}
     :data [{:values (->> (interleave  (if numeric? x-data (range 1 (inc (count x-data)))) y-data)
                          (partition 2)
                          (map vec))
             :attribs {:fill "none" :stroke "#0af"}
             :shape   (viz/svg-triangle-down 6)
             :layout viz/svg-scatter-plot}]}))

;; svg utils

(defn plot-svg
  [svg-spec width height]
  (->> svg-spec
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width width :height height})
       (svg/serialize)))
