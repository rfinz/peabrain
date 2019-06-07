(ns peabrain.math
  "Activation functions, their derivatives, and more."
  (:require [uncomplicate.neanderthal.math]
            [potemkin :as pk]))

(pk/import-vars [uncomplicate.neanderthal.math
                 asinh
                 log
                 acos
                 floor
                 atan2
                 erfc-inv
                 lgamma
                 hypot
                 tanh
                 frac
                 cdf-norm
                 ceil
                 pi
                 atan
                 f<=
                 expm1
                 cdf-norm-inv
                 cos
                 log10
                 erfc
                 tan
                 cbrt
                 f>
                 sqrt
                 pow
                 exp
                 acosh
                 f>=
                 cosh
                 erf
                 erf-inv
                 sqr
                 magnitude
                 log1p
                 trunc
                 asin
                 round
                 round?
                 f<
                 abs
                 sinh
                 atanh
                 sin
                 f=
                 signum
                 gamma
                 pow-of-2?])

(defn atanh-prime
  [x]
  (- 1 (sqr (tanh x)))
  )
