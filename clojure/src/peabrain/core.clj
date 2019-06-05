(ns peabrain.core
  (:require [uncomplicate.neanderthal.core :as nc]
            [uncomplicate.neanderthal.math :as nm :refer [tanh]]
            [uncomplicate.neanderthal.native :as nn]
            [peabrain.util :as pb]))

(defn compute-guess
  "Compute guess (mat) for LAYERS using F as the activation function.
LAYERS should look like '(W3 W2 W1 INPUTS)"
  [layers f]
  (if (== 1 (count layers))
    (first layers)
    (pb/apply-mat f (nc/mm (pb/right-ones (compute-guess (rest layers) f)) (first layers)))))

(defn back-prop
  "I don't do a lot either."
  [x]
  (println x "Hello OtherWorld!")
  )
