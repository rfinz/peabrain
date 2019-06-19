(ns peabrain.examples.learner
  "Learns a single value output from a 3 column csv."
  (:require [peabrain.core :as pb]
            [peabrain.util :as pu]
            [peabrain.math :as pm]
            [uncomplicate.neanderthal.core :as nc]
            [clojure.java.io :as io]))

(def data (with-open [reader (io/reader "src/peabrain/examples/data/cars.csv")]
   (pu/csv->mat reader))) ;; repl session starts in project home
(pu/shuffle data)

(def validation-size 300)
(def learning-rate 0.01)

(def training-data (nc/submatrix data 0 0 (- (nc/mrows data) validation-size) 3))
(def training-values (nc/submatrix data 0 3 (- (nc/mrows data) validation-size) 1))

(def validation-data (nc/submatrix data (- (nc/mrows data) validation-size) 0 validation-size 3))
(def validation-values (nc/submatrix data (- (nc/mrows data) validation-size) 3 validation-size 1))

(def layers [
             (pu/bottom-vals (pu/random-matrix 2 1) 0.1)
             (pu/bottom-vals (pu/random-matrix 3 2) 0.1)
             (pu/bottom-vals (pu/random-matrix 3 3) 0.1)
             ])

(dotimes [n 100]
  (let [gradients (pb/back-prop
                   (conj layers training-data)
                   training-values
                   pm/tanh
                   pm/tanh-prime
                   )]
    (def layers
      (mapv
       (fn [layer gradient]
         (pu/map-mat -
                     layer
                     (nc/scal! (* learning-rate (/ 1.0 (nc/mrows training-values))) gradient))
         )
       layers gradients)
      )
    )
  (when (== 0 (mod n 100))
    (println "Mean Absolute Error: " (/ (nc/sum (pu/map-mat pm/abs (pu/map-mat - (pb/compute-guess (conj layers training-data) pm/tanh) training-values))) (nc/mrows training-values)))
    )
  )

(println "Mean Absolute Error on Validation Set: " (/ (nc/sum (pu/map-mat pm/abs (pu/map-mat - (pb/compute-guess (conj layers validation-data) pm/tanh) validation-values))) (nc/mrows validation-values)))

