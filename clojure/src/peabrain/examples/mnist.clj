(ns peabrain.examples.mnist
  "Classify MNIST digits with 3 full connected layers."
  (:require [peabrain.core :as pb]
            [peabrain.util :as pu]
            [peabrain.math :as pm]
            [uncomplicate.neanderthal.core :as nc]
            [uncomplicate.neanderthal.native :as nn]
            [clojure.java.io :as io]))

(def data (with-open [reader (io/reader "src/peabrain/examples/data/mnist_train.csv")]
   (pu/csv->mat reader))) ;; repl session starts in project home
(pu/shuffle data)

(def validation-size 500)
(def learning-rate 0.05)

(def training-data (nc/submatrix data 0 1 (- (nc/mrows data) validation-size) 784))
(def training-values (nn/dge (map (fn [r] (pu/one-hot 10 (int (nc/entry r 0)))) (nc/rows (nc/submatrix data 0 0 (- (nc/mrows data) validation-size) 1)))))

(def validation-data (nc/submatrix data (- (nc/mrows data) validation-size) 1 validation-size 784))
(def validation-values (nn/dge (map (fn [r] (pu/one-hot 10 (int (nc/entry r 0)))) (nc/rows (nc/submatrix data (- (nc/mrows data) validation-size) 0 validation-size 1)))))

(def layers [
             (pu/bottom-vals (pu/random-matrix 12 10) 0.1)
             (pu/bottom-vals (pu/random-matrix 12 12) 0.1)
             (pu/bottom-vals (pu/random-matrix 784 12) 0.1)
             ])

(dotimes [m 100]
  (dotimes [n 180]
   (let [batch-data (nc/submatrix training-data (* n 50) 0 100 784)
         batch-values (nc/submatrix training-values (* n 50) 0 100 10)
         gradients (pb/back-prop
                    (conj layers batch-data)
                    batch-values
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
   )
  (println "Mean Absolute Error: " (/ (nc/sum (pu/map-mat pm/abs (pu/map-mat - (pb/compute-guess (conj layers training-data) pm/tanh) training-values))) (nc/mrows training-values)))
  )

(println "Mean Absolute Error on Validation Set: " (/ (nc/sum (pu/map-mat pm/abs (pu/map-mat - (pb/compute-guess (conj layers validation-data) pm/tanh) validation-values))) (nc/mrows validation-values)))
