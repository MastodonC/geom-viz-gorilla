;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla Repl thi.ng plots
;;; 
;; **

;; @@
(ns teal-dusk
  (:require [geom-viz-gorilla.core :refer [view-bar-chart view-line]]
            #_[java-time :as time]))

;; @@

;; @@
(defn dummy-values
  []
  {:x (range 0 21)
   :y (take 20 (repeatedly #(rand-int 11)))})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;teal-dusk/dummy-values</span>","value":"#'teal-dusk/dummy-values"}
;; <=

;; **
;;; ##Bar Plot
;; **

;; @@
(view-bar-chart (:x (dummy-values)) (:y (dummy-values)) {})
;; @@

;; @@
(view-bar-chart ["red" "green" "blue"] (take 3 (repeatedly #(rand-int 11))))
;; @@

;; @@
(view-bar-chart ["long label red" "long label green" "long label blue"] (take 3 (repeatedly #(rand-int 11))) {:vertical-x-labels true :svg-height 380})
;; @@

;; **
;;; ##Line Plot
;; **

;; @@
(view-line (:x (dummy-values)) (:y (dummy-values)))
;; @@

;; **
;;; ##Timeseries
;; **

;; @@
(defn dummy-time-series
  []
  {:x ["2012-01" "2012-02" "2012-03"]
   :y [4 20 16]})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;teal-dusk/dummy-time-series</span>","value":"#'teal-dusk/dummy-time-series"}
;; <=

;; @@
(view-line (:x (dummy-time-series)) (:y (dummy-time-series)))
;; @@

;; @@
(:y (dummy-time-series))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"[4 20 16]"}
;; <=

;; @@
(->> (interleave (:x (dummy-time-series)) (:y (dummy-time-series)))
                          (partition 2)
                          (map vec))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;2012-01&quot;</span>","value":"\"2012-01\""},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[\"2012-01\" 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;2012-02&quot;</span>","value":"\"2012-02\""},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"}],"value":"[\"2012-02\" 20]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;2012-03&quot;</span>","value":"\"2012-03\""},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"[\"2012-03\" 16]"}],"value":"([\"2012-01\" 4] [\"2012-02\" 20] [\"2012-03\" 16])"}
;; <=

;; @@

;; @@
