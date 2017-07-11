(ns geom-viz-gorilla.core
  (:require [gorilla-renderable.core :as render]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core :as m]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]))

(defn test-equation
  [t] (let [x (m/mix (- PI) PI t)] [x (* (Math/cos (* 0.5 x)) (Math/sin (* x x x)))]))

(defn export-viz
  [viz path] (->> viz (svg/svg {:width 600 :height 320}) (svg/serialize) (spit path)))

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [(- PI) PI]
             :range  [50 580]
             :major  (/ PI 2)
             :minor  (/ PI 4)
             :pos    250})
   :y-axis (viz/linear-axis
            {:domain      [-1 1]
             :range       [250 20]
             :major       0.2
             :minor       0.1
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-y true}
   :data   [{:values  (map test-equation (m/norm-range 200))
             :attribs {:fill "none" :stroke "#0af"}
             :layout  viz/svg-line-plot}]})

(defn line-plot
  []
  (->> viz-spec
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 320})
       (svg/serialize)))

(defn render
  [_ _] (line-plot))

(defrecord GeomView [plot-command options])

(extend-type GeomView
  render/Renderable
  (render [self]
    {:type :html :content (render (:plot-command self) (:options self)) :value (pr-str self)}))

(defn view
  ([plot-command] (view plot-command {}))
  ([plot-command options]
   (GeomView. plot-command options)))
