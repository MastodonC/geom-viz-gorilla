;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla Repl thi.ng plots
;;; 
;; **

;; @@
(ns teal-dusk
  (:require [geom-viz-gorilla.core :refer [view]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn dummy-values
  []
  {:x (range 0 21)
   :y (take 20 (repeatedly #(rand-int 11)))})



;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)</span>","value":"(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)"}],"value":"[:x (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"}],"value":"(2 0 4 4 9 2 1 1 1 7 0 1 5 4 7 2 0 5 2 10)"}],"value":"[:y (2 0 4 4 9 2 1 1 1 7 0 1 5 4 7 2 0 5 2 10)]"}],"value":"{:x (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20), :y (2 0 4 4 9 2 1 1 1 7 0 1 5 4 7 2 0 5 2 10)}"}
;; <=

;; @@

;; @@
