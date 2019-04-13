;; peabrain.el -- lisp for neural networks in emacs -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; This is probably a waste of life but I do not regret it.
;;;
;;; ---
;;;     Lisp programmers know the value of everything and the cost of nothing
;;; ---

;;; Code:
(require 'calc)
(require 'calc-ext)
(calc-load-everything)

(declare-function math-swap-rows "calc" (m r1 r2))
(declare-function math-random-float "calc" ())

(defun pb/hadamard (m1 m2)
  "Entrywise product of M1 M2."
  (math-map-vec-2 (lambda (r1 r2) (math-map-vec-2 'math-mul r1 r2)) m1 m2)
)

(defun pb/random-matrix (r c)
  "Initialize random matrix of height R and width C."
  (math-map-vec
   (lambda (each)
     (math-map-vec
      (lambda (item)
        (math-mul item (math-random-float)))
      each))
   (calcFunc-cvec 1 r c))
  )

(defun pb/shuffle (m)
  "Shuffle the rows of matrix M.
shuffling is done in place."
  (dolist (i (reverse (number-sequence 2 (calcFunc-vlen m))) m)
    (math-swap-rows m (+ 1 (random i)) i)
    )
  )

(defun pb/right-ones (m)
  "Add a 1 to the end of every row in matrix M."
  (math-map-vec (lambda (r) (calcFunc-vexp (calcFunc-cvec 1 (+ 1 (calcFunc-vlen r))) r)) m)
  )

(defun pb/right-shrink (m)
  "Remove a column from the right hand side of matrix M."
  (math-map-vec (lambda (r) (calcFunc-vexp (calcFunc-cvec 1 (- (calcFunc-vlen r) 1)) r)) m)
  )

(defun pb/bottom-ones (m)
  "Add a row of 1s to the bottom of matrix M."
  (calcFunc-append m (calcFunc-cvec 1 1 (calcFunc-vlen (calcFunc-mrow m 1))))
  )

(defun pb/bottom-points (m)
  "Add a row of .1s to the bottom of matrix M."
  (calcFunc-append m (calcFunc-cvec (math-make-float 1 -1) 1 (calcFunc-vlen (calcFunc-mrow m 1))))
  )

(defun pb/sqr-mat (m)
  "Apply sqr to every unit in a matrix M."
  (math-map-vec (lambda (r) (math-map-vec 'calcFunc-sqr r)) m)
  )

(defun pb/tanh-mat (m)
  "Apply tanh to every unit in a matrix M."
  (math-map-vec (lambda (r) (math-map-vec 'calcFunc-tanh r)) m)
  )

(defmath pb/tanh-prime (f)
  "Apply tanh-1 to F."
  (- 1 (sqr (tanh f)))
  )

(defun pb/tanh-mat-prime (m)
  "Apply tanh' to every unit in a matrix M."
  (math-map-vec (lambda (r) (math-map-vec 'calcFunc-pb/tanh-prime r)) m)
  )

(defun pb/compute-guess (layers f)
  "Compute guess (mat) for LAYERS using F as the activation function.
LAYERS should look like '(W3 W2 W1 INPUTS)"
  (if (= 1 (length layers))
      (car layers) ; if LAYERS is just INPUTS then just return the element
    (funcall f  (math-mul-mats (pb/right-ones (pb/compute-guess (cdr layers) f)) (car layers)))
    )
  )

(defun pb/back-prop (layers values f f-prime &rest previous)
  "Produce matrix of slopes for list of matrices LAYERS and expected VALUES.
Derivative of activation function F-PRIME and activation function F.
LAYERS should look like '(W3 W2 W1 INPUTS)"
  ;; (message "layers length: %d" (length layers))
  (if (< 1 (length layers))
      (let ((x (if (not previous)
                   ;; first time entering the function do this:
                   (pb/hadamard
                    (math-sub values (pb/compute-guess layers f))
                    (calcFunc-neg (funcall f-prime (math-mul-mats (pb/right-ones (pb/compute-guess (cdr layers) f)) (car layers))))
                    )
                 ;; if value from a previous calculation is available do this:
                 (pb/hadamard
                  (math-mul-mats
                   (if (= 1 (plist-get previous :loopnum)) (plist-get previous :derivative) (pb/right-shrink (plist-get previous :derivative)))
                   (calcFunc-trn (plist-get previous :weights))
                   )
                  (pb/right-ones (funcall f-prime (math-mul-mats (pb/right-ones (pb/compute-guess (cdr layers) f)) (car layers))))
                  )
                 )))
        (cons
         (math-mul-mats (pb/bottom-ones (calcFunc-trn (pb/compute-guess (cdr layers) f))) (if previous (pb/right-shrink x) x)) ; insert gradient
         (pb/back-prop
          (cdr layers)
          values
          f
          f-prime
          :derivative x
          :weights (car layers)
          :loopnum (if previous (+ 1 (plist-get previous :loopnum)) 1)
          ) ; recurse to calculate deeper layers
         )
        )
    nil
    )
  )

(provide 'peabrain)
;;; peabrain.el ends here
