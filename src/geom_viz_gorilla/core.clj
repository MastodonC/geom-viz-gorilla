;; gorilla-repl.fileformat = 1

;; **
;;; # Data viz
;;; We are trying to improve our data viz.
;; **

;; **
;;; ### Bar 
;; **

;; @@
(ns geom-viz-gorilla.core
  (:require [gorilla-renderable.core :as render]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core :as m]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]))

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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/view-bar-chart</span>","value":"#'geom-viz-gorilla.core/view-bar-chart"}
;; <=

;; **
;;; ### Line plot
;;; I have followed the example in https://github.com/thi-ng/geom/blob/master/geom-viz/src/core.org#line--area-plot and tried to edit it to suit our needs.
;;; 
;;; ** Test 1** worked. I haven't managed to display the values for **Test 2**.
;; **

;; **
;;; #### Generate dummy-values
;; **

;; @@
(defn dummy-values
  []
  {:x (range 0 21)
   :y (take 20 (repeatedly #(rand-int 11)))})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/dummy-values</span>","value":"#'geom-viz-gorilla.core/dummy-values"}
;; <=

;; @@
; note that y-domain has been calculated for bar charts. I only had to add a function for x-domain
(defn calc-x-domain [x-data]
  (let [lower-x (let [min-x (reduce min x-data)]
                  (if (neg? min-x) (+ min-x (* 0.2 min-x)) 0))
        upper-x (let [max-x (reduce max x-data)] (+ (* 0.01 max-x) max-x))]
    [lower-x upper-x]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/calc-x-domain</span>","value":"#'geom-viz-gorilla.core/calc-x-domain"}
;; <=

;; **
;;; #### Test 1
;; **

;; @@
(def viz-spec-line
  {:x-axis (viz/linear-axis
            {:domain (calc-x-domain (:x (dummy-values)))
             :range  [50 500]
             :major  (int (Math/floor (/ (reduce max (:x (dummy-values))) 3)))
             :minor  (/ PI 4)
             :pos    250})
   :y-axis (viz/linear-axis
            {:domain      (calc-y-domain (:y (dummy-values)))
             :range       [250 20]
             :major       (int (Math/floor (/ (reduce max (:y (dummy-values))) 3)))
             :minor       1
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-y true}
   :data   [{:values  (map vec (partition 2 (interleave (:x (dummy-values)) (:y (dummy-values)))))
             :attribs {:fill "none" :stroke "#0af"}
             :layout  viz/svg-line-plot}]})

(defn line-plot-viz
  []
  (->> viz-spec-line
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 320})
       (svg/serialize)))

(defn render
  [_ _] (line-plot-viz))

(defrecord GeomView [plot-command options])

(extend-type GeomView
  render/Renderable
  (render [self]
    {:type :html :content (render (:plot-command self) (:options self)) :value (pr-str self)}))

(defn view
  ([plot-command] (view plot-command {}))
  ([plot-command options]
   (GeomView. plot-command options)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/view</span>","value":"#'geom-viz-gorilla.core/view"}
;; <=

;; @@
(view viz-spec-line)
;; @@
;; =>
;;; {"type":"html","content":"<?xml version=\"1.0\"?>\n<svg height=\"320\" version=\"1.1\" width=\"600\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns=\"http://www.w3.org/2000/svg\"><g><g stroke-dasharray=\"1 1\" stroke=\"#caa\"><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"183.66\" x2=\"183.66\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"317.33\" x2=\"317.33\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"450.99\" x2=\"450.99\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"230.83\" y2=\"230.83\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"211.67\" y2=\"211.67\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"173.33\" y2=\"173.33\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"154.17\" y2=\"154.17\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"115.83\" y2=\"115.83\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"96.67\" y2=\"96.67\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"58.33\" y2=\"58.33\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"39.17\" y2=\"39.17\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"250.00\" y2=\"250.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"192.50\" y2=\"192.50\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"135.00\" y2=\"135.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"77.50\" y2=\"77.50\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"20.00\" y2=\"20.00\" /></g><polyline fill=\"none\" points=\"50.00,154.17 72.28,154.17 94.55,173.33 116.83,58.33 139.11,135.00 161.39,192.50 183.66,154.17 205.94,192.50 228.22,135.00 250.50,230.83 272.77,230.83 295.05,77.50 317.33,250.00 339.60,250.00 361.88,230.83 384.16,173.33 406.44,135.00 428.71,58.33 450.99,192.50 473.27,230.83\" stroke=\"#0af\" /><g stroke=\"black\"><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"183.66\" x2=\"183.66\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"317.33\" x2=\"317.33\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"450.99\" x2=\"450.99\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"67.50\" x2=\"67.50\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"84.99\" x2=\"84.99\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"102.49\" x2=\"102.49\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"119.99\" x2=\"119.99\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"137.48\" x2=\"137.48\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"154.98\" x2=\"154.98\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"172.48\" x2=\"172.48\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"189.97\" x2=\"189.97\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"207.47\" x2=\"207.47\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"224.96\" x2=\"224.96\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"242.46\" x2=\"242.46\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"259.96\" x2=\"259.96\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"277.45\" x2=\"277.45\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"294.95\" x2=\"294.95\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"312.45\" x2=\"312.45\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"329.94\" x2=\"329.94\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"347.44\" x2=\"347.44\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"364.94\" x2=\"364.94\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"382.43\" x2=\"382.43\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"399.93\" x2=\"399.93\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"417.43\" x2=\"417.43\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"434.92\" x2=\"434.92\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"452.42\" x2=\"452.42\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"469.92\" x2=\"469.92\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"487.41\" x2=\"487.41\" y1=\"250.00\" y2=\"255.00\" /><g fill=\"black\" font-family=\"Arial, sans-serif\" font-size=\"10\" stroke=\"none\" text-anchor=\"middle\"><text x=\"50.00\" y=\"270.00\">0.00</text><text x=\"183.66\" y=\"270.00\">6.00</text><text x=\"317.33\" y=\"270.00\">12.00</text><text x=\"450.99\" y=\"270.00\">18.00</text></g><line x1=\"50.00\" x2=\"500.00\" y1=\"250.00\" y2=\"250.00\" /></g><g stroke=\"black\"><line x1=\"50.00\" x2=\"40.00\" y1=\"250.00\" y2=\"250.00\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"192.50\" y2=\"192.50\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"135.00\" y2=\"135.00\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"77.50\" y2=\"77.50\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"20.00\" y2=\"20.00\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"230.83\" y2=\"230.83\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"211.67\" y2=\"211.67\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"173.33\" y2=\"173.33\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"154.17\" y2=\"154.17\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"115.83\" y2=\"115.83\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"96.67\" y2=\"96.67\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"58.33\" y2=\"58.33\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"39.17\" y2=\"39.17\" /><g fill=\"black\" font-family=\"Arial, sans-serif\" font-size=\"10\" stroke=\"none\" text-anchor=\"end\"><text x=\"35.00\" y=\"250.00\">0.00</text><text x=\"35.00\" y=\"192.50\">3.00</text><text x=\"35.00\" y=\"135.00\">6.00</text><text x=\"35.00\" y=\"77.50\">9.00</text><text x=\"35.00\" y=\"20.00\">12.00</text></g><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"20.00\" /></g></g></svg>","value":"#geom_viz_gorilla.core.GeomView{:plot-command {:x-axis {:scale #function[thi.ng.geom.viz.core/linear-scale$fn--21629], :major-size 10, :pos 250, :major (0 6 12 18), :label-dist 20, :attribs {:stroke \"black\"}, :label #function[thi.ng.geom.viz.core/default-svg-label$fn--21607], :label-style {:fill \"black\", :stroke \"none\", :font-family \"Arial, sans-serif\", :font-size 10, :text-anchor \"middle\"}, :minor (0.0 0.7853981633974483 1.5707963267948966 2.356194490192345 3.141592653589793 3.9269908169872414 4.71238898038469 5.497787143782138 6.283185307179586 7.0685834705770345 7.853981633974483 8.63937979737193 9.42477796076938 10.210176124166829 10.995574287564278 11.780972450961727 12.566370614359176 13.351768777756625 14.137166941154074 14.922565104551524 15.707963267948973 16.493361431346422 17.27875959474387 18.06415775814132 18.84955592153877 19.63495408493622), :domain [0 20.2], :minor-size 5, :visible true, :range [50 500]}, :y-axis {:scale #function[thi.ng.geom.viz.core/linear-scale$fn--21629], :major-size 10, :pos 50, :major (0 3 6 9 12), :label-dist 15, :attribs {:stroke \"black\"}, :label #function[thi.ng.geom.viz.core/default-svg-label$fn--21607], :label-style {:fill \"black\", :stroke \"none\", :font-family \"Arial, sans-serif\", :font-size 10, :text-anchor \"end\"}, :minor (1 2 4 5 7 8 10 11), :domain [0 12.0], :minor-size 5, :visible true, :range [250 20]}, :grid {:attribs {:stroke \"#caa\"}, :minor-y true}, :data [{:values ([0 5] [1 5] [2 4] [3 10] [4 6] [5 3] [6 5] [7 3] [8 6] [9 1] [10 1] [11 9] [12 0] [13 0] [14 1] [15 4] [16 6] [17 10] [18 3] [19 1]), :attribs {:fill \"none\", :stroke \"#0af\"}, :layout #function[thi.ng.geom.viz.core/svg-line-plot]}]}, :options {}}"}
;; <=

;; **
;;; #### Test 2
;; **

;; @@
(defn viz-line
  [x-data y-data]
  (let [lower-x (let [min-x (reduce min x-data)]
                  (if (zero? min-x) 0 (dec min-x)))
        upper-x (inc (reduce max x-data))]
    (let [lower-y (let [min-y (reduce min y-data)]
                  (if (zero? min-y) 0 (dec min-y)))
        upper-y (inc (reduce max y-data))]
  {:x-axis (viz/linear-axis
            {:domain [lower-x upper-x]
             :range  [50 500]
             :major  (int (Math/floor (/ upper-x 3)))
             :minor  (/ PI 4)
             :pos    250})
   :y-axis (viz/linear-axis
            {:domain      [lower-y upper-y]
             :range       [250 20]
             :major       (int (Math/floor (/ upper-y 3)))
             :minor       1
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-y true}})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/viz-line</span>","value":"#'geom-viz-gorilla.core/viz-line"}
;; <=

;; @@
(defn line-plot
  [x-values y-values]
  (->> (viz-line x-values y-values)
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 320})
       (svg/serialize)))

(defn render
  [x-values y-values] (line-plot x-values y-values ))

(defrecord GeomViewLine [x-values y-values])

(extend-type GeomViewLine
  render/Renderable
  (render [self]
    {:type :html :content (line-plot (:x-values self) (:y-values self)) :value (pr-str self)}))

(defn view-line
  [x-values y-values]
   (GeomViewLine. x-values y-values))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geom-viz-gorilla.core/view-line</span>","value":"#'geom-viz-gorilla.core/view-line"}
;; <=

;; @@
(view-line (:x (dummy-values)) (:y (dummy-values)))
;; @@
;; =>
;;; {"type":"html","content":"<?xml version=\"1.0\"?>\n<svg height=\"320\" version=\"1.1\" width=\"600\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns=\"http://www.w3.org/2000/svg\"><g><g stroke-dasharray=\"1 1\" stroke=\"#caa\"><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"200.00\" x2=\"200.00\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"350.00\" x2=\"350.00\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"500.00\" x2=\"500.00\" y1=\"250.00\" y2=\"20.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"229.09\" y2=\"229.09\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"208.18\" y2=\"208.18\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"166.36\" y2=\"166.36\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"145.45\" y2=\"145.45\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"103.64\" y2=\"103.64\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"82.73\" y2=\"82.73\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"40.91\" y2=\"40.91\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"20.00\" y2=\"20.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"250.00\" y2=\"250.00\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"187.27\" y2=\"187.27\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"124.55\" y2=\"124.55\" /><line x1=\"50.00\" x2=\"500.00\" y1=\"61.82\" y2=\"61.82\" /></g><g stroke=\"black\"><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"200.00\" x2=\"200.00\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"350.00\" x2=\"350.00\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"500.00\" x2=\"500.00\" y1=\"250.00\" y2=\"260.00\" /><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"66.83\" x2=\"66.83\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"83.66\" x2=\"83.66\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"100.49\" x2=\"100.49\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"117.32\" x2=\"117.32\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"134.15\" x2=\"134.15\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"150.98\" x2=\"150.98\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"167.81\" x2=\"167.81\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"184.64\" x2=\"184.64\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"201.47\" x2=\"201.47\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"218.30\" x2=\"218.30\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"235.13\" x2=\"235.13\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"251.96\" x2=\"251.96\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"268.79\" x2=\"268.79\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"285.62\" x2=\"285.62\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"302.45\" x2=\"302.45\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"319.28\" x2=\"319.28\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"336.11\" x2=\"336.11\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"352.94\" x2=\"352.94\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"369.77\" x2=\"369.77\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"386.60\" x2=\"386.60\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"403.43\" x2=\"403.43\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"420.26\" x2=\"420.26\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"437.09\" x2=\"437.09\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"453.92\" x2=\"453.92\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"470.75\" x2=\"470.75\" y1=\"250.00\" y2=\"255.00\" /><line x1=\"487.58\" x2=\"487.58\" y1=\"250.00\" y2=\"255.00\" /><g fill=\"black\" font-family=\"Arial, sans-serif\" font-size=\"10\" stroke=\"none\" text-anchor=\"middle\"><text x=\"50.00\" y=\"270.00\">0.00</text><text x=\"200.00\" y=\"270.00\">7.00</text><text x=\"350.00\" y=\"270.00\">14.00</text><text x=\"500.00\" y=\"270.00\">21.00</text></g><line x1=\"50.00\" x2=\"500.00\" y1=\"250.00\" y2=\"250.00\" /></g><g stroke=\"black\"><line x1=\"50.00\" x2=\"40.00\" y1=\"250.00\" y2=\"250.00\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"187.27\" y2=\"187.27\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"124.55\" y2=\"124.55\" /><line x1=\"50.00\" x2=\"40.00\" y1=\"61.82\" y2=\"61.82\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"229.09\" y2=\"229.09\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"208.18\" y2=\"208.18\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"166.36\" y2=\"166.36\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"145.45\" y2=\"145.45\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"103.64\" y2=\"103.64\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"82.73\" y2=\"82.73\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"40.91\" y2=\"40.91\" /><line x1=\"50.00\" x2=\"45.00\" y1=\"20.00\" y2=\"20.00\" /><g fill=\"black\" font-family=\"Arial, sans-serif\" font-size=\"10\" stroke=\"none\" text-anchor=\"end\"><text x=\"35.00\" y=\"250.00\">0.00</text><text x=\"35.00\" y=\"187.27\">3.00</text><text x=\"35.00\" y=\"124.55\">6.00</text><text x=\"35.00\" y=\"61.82\">9.00</text></g><line x1=\"50.00\" x2=\"50.00\" y1=\"250.00\" y2=\"20.00\" /></g></g></svg>","value":"#geom_viz_gorilla.core.GeomViewLine{:x-values (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20), :y-values (3 0 3 7 2 10 10 1 1 7 5 10 9 10 5 0 4 3 2 5)}"}
;; <=

;; @@

;; @@
