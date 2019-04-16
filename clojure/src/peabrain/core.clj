(ns peabrain.core
  (:require [uncomplicate.neanderthal.core :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

(defn compute-guess
  "I don't do a whole lot."
  [layers f]
  (apply f (mm (compute-guess ((rest layers) f) (first layers))))
  )

(defn back-prop
  "I don't do a lot either."
  [x]
  (println x "Hello OtherWorld!")
  )
