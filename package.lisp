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
           #:predict
           ;; Normalizer
           #:normalizer
           #:make-normalizer
           #:save-normalizer
           #:load-normalizer
           #:map-normalized-input)
  (:documentation "Wrapper for the libsvm support vector machine library."))
