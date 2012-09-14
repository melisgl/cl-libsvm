(defpackage :cl-libsvm
  (:nicknames #:libsvm)
  (:use :common-lisp :cffi)
  (:export #:libsvm-error
           #:sparse-index-error
           ;; Problem
           #:problem
           #:make-problem
           #:problem-size
           #:problem-target
           #:map-problem-input
           #:save-problem
           #:load-problem
           ;; Parameter
           #:parameter
           #:svm-type
           #:kernel-type
           #:make-parameter
           #:check-parameter
           #:parameter-error
           ;; Model
           #:model
           #:save-model
           #:load-model
           #:train
           #:n-classes
           #:get-labels
           #:predict
           #:predict-values
           #:predict-probabilities
           #:model-w2s
           #:distances-from-hyperplane
           #:predict-distances
           #:value-for-subsvm
           ;; Normalizer
           #:normalizer
           #:make-normalizer
           #:save-normalizer
           #:load-normalizer
           #:map-normalized-input)
  (:documentation "Wrapper for the libsvm support vector machine library."))
