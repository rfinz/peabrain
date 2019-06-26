(ns peabrain.util
  "Utilities for padding matrices, random initialization, and other ML adjacent activities."
  (:require [uncomplicate.neanderthal.core :as nc]
            [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.auxil :as na]
            [clojure.data.csv :as csv]))

(defn hadamard
  "Entrywise product of `m1` and `m2`."
  [m1 m2]
  (nn/dge (map (fn [r1 r2] (map * r1 r2)) (nc/rows m1) (nc/rows m2)))
  )

(defn random-matrix
  "Initialize random matrix of height `r` and width `c`."
  [r c]
  (nn/dge r c (repeatedly (* r c) rand))
  )

(defn shuffle
  "Shuffle the rows of matrix `m`. Shuffling is done in place."
  [m]
  (doseq [i (reverse (range 1 (nc/mrows m)))]
    (na/swap-rows! m (nn/iv (rand-int i) i))
    )
  )

(defn right-ones
  "Add a 1 to the end of every row in matrix `m`."
  ([m]
   (nn/dge (vec (map (fn [r] (conj (vec (map (fn [i] i) r)) 1.0)) (nc/rows m))))
   )
  )

(defn right-shrink
  "Remove a column from the right hand side of matrix `m`."
  [m]
  (nc/submatrix m (nc/mrows m) (- (nc/ncols m) 1))
  )

(defn bottom-ones
  "Add a row of 1s to the bottom of matrix `m`."
  [m]
  ;; forcing intermediate result to vec allows conj to append to end
  (nn/dge (conj (vec (map (fn [r] (map (fn [i] i) r)) (nc/rows m))) (repeat (nc/ncols m) 1.0)))
  )

(defn bottom-vals
  "Add a row of `v` to the bottom of matrix `m`."
  [m v]
  (nn/dge (conj (vec (map (fn [r] (map (fn [i] i) r)) (nc/rows m))) (repeat (nc/ncols m) v)))
  )

(defn map-mat
  "Apply function `f` to every item in matrix `m`."
  ([f m]
   (nn/dge (map (fn [r] (map f r)) (nc/rows m))))
  ([f m1 m2]
   ;; this could maybe be adapted to work with an arbitrary number of matrices
   (nn/dge (map (fn [r1 r2] (map f r1 r2)) (nc/rows m1) (nc/rows m2))))
  )

(defn csv->mat
  "Read CSV into native neanderthal matrix."
  [reader & options]
  (nn/dge
   (vec (map
     (fn [r] (map Double/parseDouble r)) (rest (apply csv/read-csv reader options)))))
  )
