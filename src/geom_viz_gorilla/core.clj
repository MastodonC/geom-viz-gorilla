(ns geom-viz-gorilla.core
  (:require [geom-viz-gorilla.plot :refer :all]
            [gorilla-renderable.core :as render]))

;; bar chart

(defn bar-chart-plot
  [x-values y-values options]
  (let [{:keys [plot width height]} (bar-chart x-values y-values options)]
    (plot-svg plot width height)))

(defrecord GeomViewBarChart [x-values y-values options])

(extend-type GeomViewBarChart
  render/Renderable
  (render [self]
    {:type :html :content (bar-chart-plot (:x-values self) (:y-values self) (:options self)) :value (pr-str self)}))

(defn view-bar-chart
  ([x-values y-values]
   (view-bar-chart x-values y-values {}))
  ([x-values y-values options]
   (GeomViewBarChart. x-values y-values options)))

;; line plot

(defn line-plot
  [x-values y-values options]
  (-> (viz-line x-values y-values options)
      (plot-svg-cartesian 600 320)))

(defrecord GeomViewLine [x-values y-values options])
(extend-type GeomViewLine
  render/Renderable
  (render [self]
    {:type :html :content (line-plot (:x-values self) (:y-values self) (:options self)) :value (pr-str self)}))

(defn view-line
  ([x-values y-values]
   (view-line x-values y-values {}))
  ([x-values y-values options]
   (GeomViewLine. x-values y-values options)))


;; scatter plot

(defn scatter-plot
  [x-values y-values options]
  (-> (viz-scatter x-values y-values options)
      (plot-svg-cartesian 600 320)))

(defrecord GeomViewScatter [x-values y-values options])
(extend-type GeomViewScatter
  render/Renderable
  (render [self]
    {:type :html :content (scatter-plot (:x-values self) (:y-values self) (:options self)) :value (pr-str self)}))

(defn view-scatter
  ([x-values y-values]
   (view-scatter x-values y-values {}))
  ([x-values y-values options]
   (GeomViewScatter. x-values y-values options)))
