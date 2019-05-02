(ns peabrain.util
  "Utilities for padding matrices, random initialization, and other ML adjacent activities."
  (:require [uncomplicate.neanderthal.core :as nc]
            [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.vect-math :as nv]))

(defn hadamard
  "Entrywise product of M1 M2."
  [m1 m2]
  (nn/dge (map (fn [r1 r2] (map * r1 r2)) (nc/rows m1) (nc/rows m2)))
  )

(defn random-matrix
  "Initialize random matrix of height R and width C."
  [r c]
  (nn/dge r c (repeatedly (* r c) rand))
  )

(defn shuffle
  "Shuffle the rows of matrix M. Shuffling is done in place."
  [m]
  )

(defn right-ones
  "Add a 1 to the end of every row in matrix M. Map OPTIONS is passed to matrix creation method."
  ([m]
   (nn/dge (vec (map (fn [r] (conj (vec (map (fn [i] i) r)) 1.0)) (nc/rows m))))
   )
  )

(defn right-shrink
  "Remove a column from the right hand side of matrix M."
  [m]
  )

(defn bottom-ones
  "Add a row of 1s to the bottom of matrix M."
  [m]
  )

(defn bottom-vals
  "Add a row of val to the bottom of matrix M."
  [m]
  )
