(ns peabrain.core
  (:require [uncomplicate.neanderthal.core :as nc]
            [peabrain.util :as pb]))

(defn compute-guess
  "Compute guess (mat) for `layers` using `f` as the activation function.
  `layers` should look like [W3 W2 W1 INPUTS]"
  [layers f]
  (if (= 1 (count layers))
    (first layers)
    (pb/map-mat f (nc/mm (pb/right-ones (compute-guess (rest layers) f)) (first layers)))))

(defn back-prop
  "Produce matrix of slopes for list of matrices `layers` and expected `values`.
  Derivative of activation function `f-prime` and activation function `f`. `layers` should look like [W3 W2 W1 INPUTS]"
  ([layers values f f-prime] (back-prop layers values f f-prime nil))
  ([layers values f f-prime previous]
   (if (< 1 (count layers))
     (let [x (if-not previous
               ;; first time entering the function, do this:
               (pb/hadamard
                (pb/map-mat - values (compute-guess layers f))
                (nc/scal! -1.0 (pb/map-mat f-prime (nc/mm (pb/right-ones (compute-guess (rest layers) f)) (first layers)))))
               ;; if value from a previous calculation is available do this:
               (pb/hadamard
                (nc/mm
                 (if (= 1 (:loopnum previous)) (:derivative previous) (pb/right-shrink (:derivative previous)))
                 (nc/trans (:weights previous)))
                (pb/right-ones (pb/map-mat f-prime (nc/mm (pb/right-ones (compute-guess (rest layers) f)) (first layers))))))]
       (cons
        (nc/mm
         (pb/bottom-ones (nc/trans (compute-guess (rest layers) f)))
         (if previous (pb/right-shrink x) x)) ; insert gradient
        (back-prop
         (rest layers)
         values
         f
         f-prime
         {
          :derivative x
          :weights (first layers)
          :loopnum (if previous (+ 1 (:loopnum previous)) 1)}))))))
