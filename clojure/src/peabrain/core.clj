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
    (pb/map-mat f (nc/mm (pb/right-ones (compute-guess (rest layers) f)) (first layers)))))

(defn back-prop
  "Produce matrix of slopes for list of matrices LAYERS and expected VALUES.
  Derivative of activation function F-PRIME and activation function F. LAYERS should look like '(W3 W2 W1 INPUTS)"
  ([layers values f f-prime] (println x "Hello OtherWorld!"))
  ([layers values f f-prime previous] (println x "Hello OtherWorld!"))
  )
