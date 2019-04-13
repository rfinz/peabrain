;; learner.el -- test case learning -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;; Why

;;; Code:

(require 'peabrain)
(require 'calc)
(require 'calc-ext)
(calc-load-everything)

(defvar learn-layers)
(setq learn-layers (list (pb/bottom-points (pb/random-matrix 2 1))
                         (pb/bottom-points (pb/random-matrix 3 2))
                         (pb/bottom-points (pb/random-matrix 3 3))))

(defvar data-matrix (calcFunc-pack 0 '(vec)))
(setq data-matrix (calcFunc-pack 0 '(vec)))
(with-temp-buffer
  (insert-file-contents "data.csv")
  (goto-char 1)
  (forward-line 1)
  (while (< (point) (point-max))
    (beginning-of-line 1)
    (setq data-matrix (calcFunc-append
                       data-matrix
                       (calcFunc-cvec
                        (append '(vec)
                                (mapcar
                                 (lambda (s) (math-read-number s))
                                 (split-string (buffer-substring (point) (progn (end-of-line) (point))) "," )))
                        1)))
    (forward-line 1)
    )
  )

(pb/shuffle data-matrix)

(defvar training-matrix)
(defvar training-values)
(defvar learning-rate)
(defvar test-data)

(setq test-data (calcFunc-subvec data-matrix 1 (round (* (calcFunc-vlen data-matrix) .005))))
(setq training-matrix (calcFunc-mcol test-data '(vec 1 2 3)))
(setq training-values (calcFunc-mcol test-data 4))
(setq learning-rate (math-make-float 1 -2))

(defvar l-update)
(dotimes (n 1000)
  (let ((gradients (pb/back-prop
                    (append learn-layers (list training-matrix))
                    training-values
                    'pb/tanh-mat
                    'pb/tanh-mat-prime)))
    (setq l-update (list))
    (setq learn-layers (dolist (i (reverse (number-sequence 0 (1- (length learn-layers)))) l-update)
                         (setq l-update (cons (math-sub (nth i learn-layers)
                                                        (math-mul learning-rate
                                                                  (nth i gradients)))
                                              l-update))
       ))
    (when (= 0 (mod n 100))
      (message "Mean Absolute Error: %s"
               (math-div (calcFunc-vsum
                 (calcFunc-abs
                  (math-sub (pb/compute-guess (append learn-layers (list training-matrix)) 'pb/tanh-mat)
                            training-values)))
                         (calcFunc-vlen training-values)
                         )
               ))
    )
  )

(provide 'learn)
;;; learner.el ends here
