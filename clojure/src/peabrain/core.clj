(ns peabrain.core
  (:require [uncomplicate.neanderthal.core :refer :all]
            [peabrain.util :as pb]))

(defn compute-guess
  "Compute guess (mat) for LAYERS using F as the activation function.
LAYERS should look like '(W3 W2 W1 INPUTS)"
  [layers f]
  (if (== 1 (length layers))
    (first layers)
    (apply f (mm (pb/right-ones (compute-guess (rest layers) f) (first layers))))))

(defn back-prop
  "I don't do a lot either."
  [x]
  (println x "Hello OtherWorld!")
  )
