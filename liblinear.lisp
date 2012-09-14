(in-package :liblinear)

(defparameter *liblinear-dir*
  (make-pathname :name nil :type nil
                 :defaults (asdf:component-pathname
                            (asdf:find-system :cl-liblinear))))

(defparameter *liblinear-lib-dir*
  (merge-pathnames (make-pathname :directory '(:relative "lib")
                                  :name "liblinear" :type "so")
                   *liblinear-dir*))

(define-foreign-library liblinear
  (:darwin "liblinear.dylib")
  (:unix (:or "/home/mega/lisp/liblinear-1.91/liblinear.so.1" "liblinear.so"))
  (:windows (:or "liblinear.dll" "svmc.dll"))
  (t (:default "liblinear")))

(use-foreign-library liblinear)

(define-condition liblinear-error () ())


;;;; Wrapped pointers

(defvar *wrappers*
  (tg:make-weak-hash-table :weakness :value)
  "An address to wrapper map.")

(defclass wrapper ()
  ((pointer
    :initarg :pointer :reader pointer
    :documentation "A foreign pointer that is destroyed when its
wrapper is garbage collected.")
   (ctype
    :initarg :ctype :reader ctype
    :documentation "The foreign type of POINTER.")
   (references
    :initarg :references :accessor references
    :documentation "A list of lisp objects reachable from POINTER.")))

(defgeneric reachable-objects (pointer ctype)
  (:method (pointer ctype)
    (declare (ignore pointer ctype))
    '())
  (:documentation "Return a list of objects reachable from POINTER of
CTYPE. Used to initialize REFERNCES of a wrapper."))

(defgeneric ctype->wrapper-class (ctype)
  (:method (ctype)
    (declare (ignore ctype))
    'wrapper)
  (:documentation "Return the designator of the class that is to be
instantiated when a pointer of CTYPE is being wrapped."))

(defgeneric wrap (pointer ctype)
  (:method (pointer ctype)
    ;; FIXME: lock
    (or (gethash (pointer-address pointer) *wrappers*)
        (setf (gethash (pointer-address pointer) *wrappers*)
              (make-instance (ctype->wrapper-class ctype)
                             :pointer pointer :ctype ctype
                             :references (reachable-objects pointer ctype))))))

(defgeneric destroy-wrapped-pointer (pointer ctype)
  (:method (pointer ctype)
    (declare (ignore ctype))
    (unless (null-pointer-p pointer)
      (foreign-free pointer)))
  (:documentation "Free foreign resources associated with POINTER of CTYPE."))

(defmethod initialize-instance :after ((wrapper wrapper)
                                       &key &allow-other-keys)
  (let ((pointer (pointer wrapper))
        (ctype (ctype wrapper)))
    (tg:finalize wrapper
                 (lambda ()
                   (remhash (pointer-address pointer) *wrappers*)
                   (destroy-wrapped-pointer pointer ctype)))))

(defmacro define-wrapped-pointer (ctype class)
  `(progn
     (defmethod ctype->wrapper-class ((ctype ,ctype))
       ',class)
     (defmethod translate-from-foreign (pointer (ctype ,ctype))
       (wrap pointer ctype))
     (defmethod translate-to-foreign ((wrapper ,class) (ctype ,ctype))
       (pointer wrapper))))

;;;; Utilities

(defun foreign-slot-value* (object pointer-ctype ctype slot-name)
  "A type safe variant of FOREIGN-SLOT-VALUE that first convert the
lisp OBJECT to POINTER-CTYPE and than returns the value of its slot."
  (foreign-slot-value (convert-to-foreign object pointer-ctype)
                      ctype slot-name))

(defun convert-to-double (x)
  (coerce x 'double-float))

(defctype auto-double (:wrapper :double :to-c convert-to-double))

(defcstruct node
  (index :int)
  (value auto-double))

(define-foreign-type double-vector ()
  ()
  (:actual-type :pointer)
  (:simple-parser double-vector))

(define-foreign-type sparse-vector ()
  ()
  (:actual-type :pointer)
  (:simple-parser sparse-vector))

(define-foreign-type temporary-sparse-vector (sparse-vector)
  ()
  (:simple-parser temporary-sparse-vector))

(define-foreign-type sparse-vector-vector ()
  ()
  (:actual-type :pointer)
  (:simple-parser sparse-vector-vector))

(defun mapper-length (mapper)
  (let ((n 0))
    (funcall mapper (lambda (&rest args)
                      (declare (ignore args))
                      (incf n)))
    n))

(defgeneric convert-vector (object ctype)
  (:method ((vector vector) ctype)
    (let* ((n (length vector))
           (v (foreign-alloc ctype :count n)))
      (dotimes (i n)
        (setf (mem-aref v ctype i) (aref vector i)))
      (values v n)))
  (:method ((mapper function) ctype)
    (let* ((n (mapper-length mapper))
           (v (foreign-alloc ctype :count n))
           (i 0))
      (funcall mapper
               (lambda (value)
                 (setf (mem-aref v ctype i) value)
                 (incf i)))
      (values v n)))
  (:method ((symbol symbol) ctype)
    (convert-vector (symbol-function symbol) ctype)))

(defmethod translate-to-foreign ((v vector) (name double-vector))
  (convert-vector v 'auto-double))

(defmethod translate-to-foreign ((v function) (name double-vector))
  (convert-vector v 'auto-double))

(defmethod translate-to-foreign ((v symbol) (name double-vector))
  (convert-vector v 'auto-double))

(define-condition sparse-index-error (liblinear-error)
  ((index :initarg :index :reader index)
   (max-index :initarg :max-index :reader max-index))
  (:report (lambda (condition stream)
             (format stream "Invalid sparse index ~S is not greater than ~S. ~
                             Indices must be in ascending order ~
                             and greater than zero."
                     (index condition) (max-index condition)))))

(defmethod translate-to-foreign ((vector vector) (name sparse-vector))
  (let* ((n (length vector))
         (v (foreign-alloc 'node :count (1+ n)))
         (max-index 0))
    (dotimes (i n)
      (destructuring-bind (index . value) (aref vector i)
        (when (<= index max-index)
          (error 'sparse-index-error :index index :max-index max-index))
        (setf max-index index)
        (setf (foreign-slot-value (mem-aref v 'node i) 'node 'index) index)
        (setf (foreign-slot-value (mem-aref v 'node i) 'node 'value) value)))
    (setf (foreign-slot-value (mem-aref v 'node n) 'node 'index) -1)
    (setf (foreign-slot-value (mem-aref v 'node n) 'node 'value) 0.0d0)
    v))

(defmethod translate-to-foreign ((mapper function) (name sparse-vector))
  (let* ((n (mapper-length mapper))
         (v (foreign-alloc 'node :count (1+ n)))
         (i 0)
         (max-index 0))
    (funcall mapper
             (lambda (index value)
               (when (<= index max-index)
                 (error 'sparse-index-error :index index :max-index max-index))
               (setf max-index index)
               (setf (foreign-slot-value (mem-aref v 'node i) 'node 'index)
                     index)
               (setf (foreign-slot-value (mem-aref v 'node i) 'node 'value)
                     value)
               (incf i)))
    (setf (foreign-slot-value (mem-aref v 'node n) 'node 'index) -1)
    (setf (foreign-slot-value (mem-aref v 'node n) 'node 'value) 0.0d0)
    v))

(defmethod translate-to-foreign ((symbol symbol) (name sparse-vector))
  (translate-to-foreign symbol name))

(defmethod translate-to-foreign ((v vector) (name sparse-vector-vector))
  (convert-vector v 'sparse-vector))

(defmethod translate-to-foreign ((v function) (name sparse-vector-vector))
  (convert-vector v 'sparse-vector))

(defmethod translate-to-foreign ((v symbol) (name sparse-vector-vector))
  (convert-vector v 'sparse-vector))

(defmethod free-translated-object (value (name temporary-sparse-vector) param)
  (declare (ignore param))
  (foreign-free value))


;;;; Problem

(defcstruct problem-struct
  (l :int)
  (n :int)
  (y double-vector)
  (x sparse-vector-vector)
  (bias auto-double))

#|

(cffi:foreign-type-size 'problem-struct)
(cffi:foreign-slot-offset 'problem-struct 'bias)

(defun foo (p)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (foreign-slot-value p 'problem-struct 'bias) 1d0))

(defun foo (p)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (foreign-slot-value p 'problem-struct 'l) 1))
(disassemble 'foo)

|#

(define-foreign-type problem-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser problem)
  (:documentation "A problem consists of a number of sparse input
vectors and their respective targets. The target is the label of the
class for classification or value for regression."))

(defclass problem (wrapper) ())

(define-wrapped-pointer problem-type problem)

(defmethod print-object ((problem problem) stream)
  (print-unreadable-object (problem stream :type t :identity t)
    (format stream ":SIZE ~A" (problem-size problem)))
  problem)

(defmethod destroy-wrapped-pointer (problem (ctype problem-type))
  (foreign-free (foreign-slot-value problem 'problem-struct 'y))
  (let ((x (foreign-slot-value problem 'problem-struct 'x)))
    (dotimes (i (foreign-slot-value problem 'problem-struct 'l))
      (foreign-free (mem-aref x :pointer i))))
  (foreign-free (foreign-slot-value problem 'problem-struct 'x)))

(defun make-problem (targets inputs)
  "Create a problem from TARGET that is a vector of real numbers and
INPUTS that is a vector of sparse vectors. A sparse vector has
index/value conses as elements, alternatively it may be given as a
mapper function that maps to index and value."
  (multiple-value-bind (targets n-targets)
      (convert-vector targets 'auto-double)
    (multiple-value-bind (inputs n-inputs)
        (convert-vector inputs 'sparse-vector)
      (assert (= n-targets n-inputs))
      (let ((p (foreign-alloc 'problem-struct)))
        (setf (foreign-slot-value p 'problem-struct 'l) n-targets
              (foreign-slot-value p 'problem-struct 'y) targets
              (foreign-slot-value p 'problem-struct 'x) inputs
              (foreign-slot-value p 'problem-struct 'bias) 1d0)
        (let ((problem (wrap p (make-instance 'problem-type))))
          (setf (foreign-slot-value p 'problem-struct 'n)
                (1+ (max-problem-feature problem)))
          problem)))))

(defun max-problem-feature (problem)
  (let ((max -1))
    (dotimes (i (problem-size problem))
      (map-problem-input (lambda (index value)
                           (declare (ignore value))
                           (setf max (max max index)))
                         problem i))
    max))

(defun problem-size (problem)
  "Return the number of targets in PROBLEM."
  (foreign-slot-value* problem 'problem 'problem-struct 'l))

(defun problem-target (problem i)
  "Return the Ith target."
  (assert (and (<= 0 i) (< i (problem-size problem))))
  (mem-aref (foreign-slot-value* problem 'problem 'problem-struct 'y)
            'auto-double i))

(defun map-sparse-vector (function vector)
  (loop for i upfrom 0
        for element = (mem-aref vector 'node i)
        for index = (foreign-slot-value element 'node 'index)
        for value = (foreign-slot-value element 'node 'value)
        while (<= 0 index) do
        (funcall function index value)))

;;; Note that this is the only way to get back the input vectors from
;;; PROBLEM because pointers to the sparse vectors that make up the
;;; problem are not wrapped (see wrapper mechanism) and consequently
;;; they cannot be handed out to client code.
(defun map-problem-input (function problem i)
  "Map FUNCTION over the indices and values of the Ith input vector of
PROBLEM."
  (assert (and (<= 0 i) (< i (problem-size problem))))
  (map-sparse-vector function
                     (mem-aref
                      (foreign-slot-value* problem 'problem 'problem-struct 'x)
                      'sparse-vector i)))

(defun save-problem (problem filename)
  "Save PROBLEM to FILENAME in the LIBSVM/SVMLight format."
  (with-open-file (s filename :direction :output :if-does-not-exist :create
                   :if-exists :supersede :element-type 'base-char
                   :external-format :ascii)
    (let ((*print-pretty* nil))
      (dotimes (i (problem-size problem))
        (let ((target (problem-target problem i)))
          (prin1 (if (= target (round target))
                     (round target)
                     (float target 0.0))
                 s)
          (map-problem-input (lambda (index value)
                               (princ #\Space s)
                               (prin1 index s)
                               (princ #\: s)
                               (prin1 (float value 0.0) s))
                             problem i)
          (terpri s))))))

(defun load-problem (filename)
  "Read a problem from FILENAME in the LIBSVM/SVMLight format."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (labels
          ((map-file (fn)
             (with-open-file (s filename :element-type 'base-char
                              :external-format :ascii)
               (loop for line = (read-line s nil nil)
                     while line
                     do (funcall fn line))))
           (map-targets (fn)
             (map-file (lambda (line)
                         (funcall fn (read-from-string line)))))
           (map-input (fn line)
             (loop with p = (nth-value 1 (read-from-string line))
                   do
                   (multiple-value-bind (index position)
                       (parse-integer line :start p :junk-allowed t)
                     (setf p position)
                     (unless index
                       (return))
                     (multiple-value-bind (value position)
                         (read-from-string line t nil :start (1+ position))
                       (funcall fn index value)
                       (setf p position)))))
           (map-inputs (fn)
             (map-file (lambda (line)
                         (funcall fn (lambda (fn)
                                       (map-input fn line)))))))
        (make-problem #'map-targets #'map-inputs)))))

;;;; Parameter

(defcenum solver-type :l2r-lr :l2r-l2loss-svc-dual :l2r-l2loss-svc
  :l2r-l1loss-svc-dual :mcsvm-cs :l1r-l2loss-svc :l1r-lr :l2r-lr-dual)

;; (foreign-type-alignment 'auto-double)
(defcstruct parameter-struct
  (solver-type solver-type)
  ;; stopping criteria
  (eps auto-double)
  (c auto-double)
  (nr-weight :int)
  (weight-label :pointer)
  (weight :pointer)
  (p auto-double))

(define-foreign-type parameter-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser parameter)
  (:documentation "A parameter object encapsulates the different kinds
of parameters of SVM. Some of the parameters are specific to a
particular kernel."))

(defmacro define-slot-reader (name (&key pointer-ctype
                                         (class-name pointer-ctype)
                                         ctype
                                         (slot-name name))
                              &optional documentation)
  `(defun ,name (,class-name)
     ,@(when documentation (list documentation))
     (foreign-slot-value* ,class-name ',pointer-ctype ',ctype ',slot-name)))

(define-slot-reader solver-type
    (:pointer-ctype parameter :ctype parameter-struct)
  "Return the value of the SOLVER-TYPE slot of PARAMETER.")

;;; FIXME: missing readers for parameters

(defclass parameter (wrapper) ())

(define-wrapped-pointer parameter-type parameter)

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t :identity t)
    (format stream "~A" (solver-type parameter)))
  parameter)

(defun make-parameter (&key (solver-type :l2r-lr) (c 1) (eps 0.001) (p 0.1))
  "Make an object that describes how to TRAIN. See the LIBLINEAR
documentation for the meaning of the arguments."
  (let ((parameter (foreign-alloc 'parameter-struct)))
    (macrolet ((set-slots (&rest names)
                 (list* 'progn
                        (loop for name in names collect
                              `(setf (foreign-slot-value parameter
                                      'parameter-struct
                                      ',name)
                                ,name)))))
      ;; These three parameters are unsupported by this wrapper for
      ;; the time being.
      (let ((nr-weight 0)
            (weight-label (null-pointer))
            (weight (null-pointer)))
        (set-slots solver-type c eps nr-weight weight-label weight p))
      (wrap parameter (make-instance 'parameter-type)))))

(defcfun ("check_parameter" %check-parameter) :string
  (problem problem)
  (parameter parameter))

(define-condition parameter-error (liblinear-error)
  ((parameter :initarg :parameter :reader parameter)
   (problem :initarg :problem :reader problem)
   (explanation :initarg :explanation :reader explanation))
  (:report (lambda (condition stream)
             (format stream "Bad parameter ~S for ~S: ~S"
                     (parameter condition) (problem condition)
                     (explanation condition)))))

(defun parameter-error (problem parameter explanation)
  (error 'parameter-error :problem problem :parameter parameter
         :explanation explanation))

(defun check-parameter (problem parameter &key errorp)
  "See if PARAMETER is suitable for PROBLEM. Return T if it is, and
NIL and a string explaining why if it is not. If ERRORP and the check
fails signal BAD-PARAMETER condition."
  (let ((r (%check-parameter problem parameter)))
    (if r
        (if errorp
            (parameter-error problem parameter r)
            (values nil r))
        t)))

;;;; Model

(define-foreign-type model-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser model)
  (:documentation "A model is what falls out of training and can be
used later to make predictions."))

(defclass model (wrapper) ())

(define-wrapped-pointer model-type model)

(defcfun ("free_and_destroy_model" %free-and-destroy-model) :void
  (model model))

(defmethod destroy-wrapped-pointer (model (ctype model))
  (%free-and-destroy-model model))

(define-foreign-type error-code-type ()
  ()
  (:actual-type :int)
  (:simple-parser error-code))

(defmethod translate-from-foreign (error-code (name error-code-type))
  (unless (zerop error-code)
    (error "Error code: ~S" error-code))
  (values))

(defcfun ("save_model" %save-model) error-code
  (filename :string)
  (model model))

(defun save-model (model filename)
  "Save MODEL to FILENAME."
  (%save-model (namestring
                (merge-pathnames filename *default-pathname-defaults*))
               model))

(defcfun ("load_model" %load-model) model
  (filename :string))

(defun load-model (filename)
  "Load a model from a file."
  (let* ((filename (merge-pathnames filename *default-pathname-defaults*))
         (model (%load-model (namestring filename))))
    (when (null-pointer-p (pointer model))
      (error "Cannot load ~S" filename))
    model))

(defcfun ("train" %train) model
  (problem problem)
  (parameter parameter))

(defun model-parameter (model)
  (assert (typep model 'model))
  ;; The model struct starts with the parameter struct
  ;; FIXME: wrapping doesn't work right here
  (convert-from-foreign (pointer model) 'parameter))

(defun train (problem parameter)
  "Train and return a model object on PROBLEM according PARAMETER.
Signal a PARAMETER-ERROR if PARAMETER is incorrect."
  (check-parameter problem parameter :errorp t)
  (let ((model (%train problem parameter)))
    ;; The models created by train keep references into the
    ;; problem so it must be kept around.
    (push problem (references model))
    model))

(defcfun ("get_nr_class" n-classes) :int
  (model model))

(setf (documentation #'n-classes 'function)
      "For a classification model, this function gives the number of
classes. For a regression or an one-class model, 2 is returned.")

(defcfun ("get_labels" %get-labels) :void
  (model model)
  (labels :pointer))

(defun get-labels (model)
  "Wrapper around get_labels."
  (let* ((n-classes (n-classes model))
         (v (foreign-alloc :int :count n-classes)))
    (%get-labels model v)
    (let ((labels (make-array n-classes :element-type 'integer)))
      (dotimes (i n-classes)
        (setf (aref labels i) (mem-aref v :int i)))
      labels)))

(defcfun ("predict" predict) :double
  (model model)
  (input temporary-sparse-vector))

(setf (documentation #'predict 'function)
      "Return the prediction (a double float) for the sparse vector
INPUT according to MODEL.")

(defcfun ("predict_values" %predict-values) :void
  (model model)
  (input temporary-sparse-vector)
  (decision-values double-vector))

(defun predict-values (model input)
  "Wrapper around predict_values. For a classification model with
nr_class classes, this function gives nr_class*(nr_class-1)/2 decision
values in the array dec_values, where nr_class can be obtained from
the function get_nr_class. The order is label[0] vs. label[1],
..., label[0] vs. label[nr_class-1], label[1] vs. label[2], ...,
label[nr_class-2] vs. label[nr_class-1], where label can be obtained
from the function get_labels.

For a regression model, label[0] is the function value of x calculated
using the model. For one-class model, label[0] is +1 or -1."
  (let* ((n-classes (n-classes model))
         (n (/ (* n-classes (1- n-classes)) 2)))
    (with-foreign-pointer (v (* (foreign-type-size :double) n))
      (%predict-values model input v)
      (let ((decision-values (make-array n :element-type 'double-float)))
        (dotimes (i n)
          (setf (aref decision-values i) (mem-aref v :double i)))
        decision-values))))

(defcfun ("predict_probability" %predict-probability) :double
  (model model)
  (input temporary-sparse-vector)
  (probabilities double-vector))

(defun predict-probabilities (model input)
  "Return the prediction (a double float) for the sparse vector INPUT
according to MODEL. As the second value return a double float vector
of probabilities for the labels in the order they appear in
GET-LABELS."
  (let* ((n-classes (n-classes model)))
    (with-foreign-pointer (v (* (foreign-type-size :double) n-classes))
      (let ((prediction (%predict-probability model input v))
            (probabilities (make-array n-classes :element-type 'double-float)))
        (dotimes (i n-classes)
          (setf (aref probabilities i) (mem-aref v :double i)))
        (values prediction probabilities)))))

(defun upper-half-index (row col n)
  "If the upper half of a square matrix of size N is stored in a
vector in a quasi row major manner, then return the index into this
vector corresponding to the element at ROW and COL. (< ROW COL N) must
hold."
  ;; The upper half of a square matrix is stored as:
  ;;
  ;; 0: . U U R R
  ;; 1: . . U R R
  ;; 2: . . . X V
  ;; 3: . . . . V
  ;; 4: . . . . .
  (assert (< row col n))
  (let* ((s (expt row 2)))
    (1- (+
         ;; marked as U:
         (- s (/ (- s row) 2))
         ;; marked as R
         (* row (- n row 1))
         (- col row)))))

(defun symmetric-upper-half-index (row col n)
  (when (< col row)
    (rotatef row col))
  (upper-half-index row col n))

(let ((indices #2a((nil 0 1 2 3)
                   (nil nil 4 5 6)
                   (nil nil nil 7 8)
                   (nil nil nil nil 9)
                   (nil nil nil nil nil))))
  (dotimes (row 5)
    (dotimes (col 5)
      (assert (eql (aref indices row col)
                   (ignore-errors
                     (upper-half-index row col 5)))))))

(let ((indices #2a((nil 0 1 2 3)
                   (0 nil 4 5 6)
                   (1 4 nil 7 8)
                   (2 5 7 nil 9)
                   (3 6 8 9 nil))))
  (dotimes (row 5)
    (dotimes (col 5)
      (assert (eql (aref indices row col)
                   (ignore-errors
                     (symmetric-upper-half-index row col 5)))))))

(defun value-for-subsvm (seq label1 label2 &key model
                         (model-labels (get-labels model)))
  "In classification tasks, there is one subsvm for each unordered
pair of different labels. Return the value in SEQ pertaining to the
subsvm that dicedes between LABEL1 and LABEL2. This is to look up
values in the result of PREDICT-VALUES, MODEL-W2S or
DISTANCES-FROM-HYPERPLANE."
  (let ((l1 (position label1 model-labels :test #'=))
        (l2 (position label2 model-labels :test #'=))
        (n (length model-labels)))
    (assert l1)
    (assert l2)
    (elt seq (symmetric-upper-half-index l1 l2 n))))

;;; FIXME: cross validation, probability stuff is missing


;;;; Normalizer

(defun map-it (function sequence-or-mapper)
  (if (typep sequence-or-mapper 'sequence)
      (map nil function sequence-or-mapper)
      (funcall sequence-or-mapper function)))

(defun map-input (function sequence-or-mapper)
  (if (typep sequence-or-mapper 'sequence)
      (map nil (lambda (feature)
                 (funcall function (car feature) (cdr feature)))
           sequence-or-mapper)
      (funcall sequence-or-mapper function)))

(defclass normalizer ()
  ((lower :initarg :lower :reader lower)
   (upper :initarg :upper :reader upper)
   (min-maxes :initarg :min-maxes :reader min-maxes))
  (:documentation "Normalizers offer basically the same functionality
as svm-scale."))

(defun make-normalizer (inputs &key (lower -1) (upper 1))
  "Create a normalizer that will translate inputs to the [LOWER,UPPER]
range."
  (let ((min-maxes (make-array 0 :adjustable t)))
    (labels ((one-feature (index value)
               (unless (< index (length min-maxes))
                 (adjust-array min-maxes (1+ index) :initial-element nil))
               (if (null (aref min-maxes index))
                   (setf (aref min-maxes index) (cons value value))
                   (destructuring-bind (min . max) (aref min-maxes index)
                     (when (or (null min) (< value min))
                       (setf (car (aref min-maxes index)) value))
                     (when (or (null max) (< max value))
                       (setf (cdr (aref min-maxes index)) value)))))
             (one-input (input)
               (map-input #'one-feature input)))
      (map-it #'one-input inputs))
    (make-instance 'normalizer :lower lower :upper upper
                   :min-maxes min-maxes)))

(defun map-normalized-input (normalizer input function)
  "Map function over the features in INPUT normalized by NORMALIZER."
  (let ((lower (lower normalizer))
        (upper (upper normalizer))
        (min-maxes (min-maxes normalizer)))
    (flet ((norm (value min max)
             (if (eql min max)
                 ;; either not encountered or singular
                 value
                 (+ lower
                    (* (/ (- upper lower)
                          (- max min))
                       (- value min))))))
      (map-input (lambda (index value)
                   (destructuring-bind (min . max)
                       (if (< 0 index (length min-maxes))
                           (aref min-maxes index)
                           (cons nil nil))
                     (funcall function index (norm value min max))))
                 input))))

(defun write-normalizer (normalizer stream)
  "Save NORMALIZER to STREAM in the format used by svm-scale."
  (let ((*print-pretty* nil)
        (min-maxes (min-maxes normalizer)))
    (format stream "x~%~S ~S~%" (lower normalizer) (upper normalizer))
    (loop for i below (length min-maxes) do
          (when (aref min-maxes i)
            (destructuring-bind (min . max) (aref min-maxes i)
              (format stream "~D ~S ~S~%"
                      i (float min 0.0) (float max 0.0)))))))

(defun save-normalizer (normalizer filename)
  "Save NORMALIZER to FILENAME in the format used by svm-scale."
  (with-open-file (stream filename
                   :direction :output :if-does-not-exist :create
                   :if-exists :supersede :element-type 'base-char
                   :external-format :ascii)
    (write-normalizer normalizer stream)))

(defun read-normalizer (stream)
  "Load normalizer from STREAM that is in the format used by svm-scale."
  (unless (string= "x" (read-line stream))
    (error "File format not supported."))
  (let ((lower (read stream))
        (upper (read stream))
        (min-maxes (make-array 0 :adjustable t)))
    (loop for line = (read-line stream nil nil)
          while line
          do
          (with-input-from-string (stream line)
            (let ((index (read stream))
                  (min (read stream))
                  (max (read stream)))
              (unless (< index (length min-maxes))
                (adjust-array min-maxes (1+ index) :initial-element nil))
              (setf (aref min-maxes index) (cons min max)))))
    (make-instance 'normalizer :lower lower :upper upper
                   :min-maxes min-maxes)))

(defun load-normalizer (filename)
  "Load normalizer from FILENAME that is in the format used by svm-scale."
  (with-open-file (stream filename
                   :element-type 'base-char :external-format :ascii)
    (read-normalizer stream)))


;;;; Test

(defun test-problem ()
  (let* ((targets (vector 0 1 1 0))
         ;; inputs: ((0 0) (0 1) (1 0) (1 1)) (plus sparse indices)
         (inputs (vector (vector (cons 1 0) (cons 2 0))
                         ;; Pass a mapper function
                         (lambda (fn)
                           (map nil (lambda (c)
                                      (funcall fn (car c) (cdr c)))
                                (vector (cons 1 0) (cons 2 1))))
                         (vector (cons 1 1) (cons 2 0))
                         (vector (cons 1 1) (cons 2 1))))
         (problem (make-problem targets inputs)))
    (assert (= (length targets) (problem-size problem)))
    (loop for i below (length targets) do
      (assert (= (aref targets i) (problem-target problem i))))
    (flet ((input->vector (problem i)
             (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
               (map-problem-input (lambda (index value)
                                    (vector-push-extend (cons index value) v))
                                  problem i)
               v)))
      (loop for i below (length inputs) do
        (unless (= i 1)
          (assert (every (lambda (x y)
                           (and (= (car x) (car y))
                                (= (cdr x) (cdr y))))
                         (coerce (aref inputs i) 'list)
                         (coerce (input->vector problem i) 'list))))))
    (let ((parameter (make-parameter)))
      (assert (check-parameter problem parameter))
      (flet ((test-model (model)
               (assert (= 2 (or (n-classes model))))))
        (let ((model (train problem parameter))
              (filename (merge-pathnames (make-pathname :name "test-model")
                                         *liblinear-dir*)))
          (test-model model)
          (save-model model filename)
          (test-model (load-model filename)))))))

(defun ~= (x y &optional (tolerance 0.01))
  (< (abs (- x y)) tolerance))

(defun test-predict-values ()
  (let* ((inputs (coerce (loop repeat 100
                               collect (let ((x (random 1d0))
                                             (y (random 1d0)))
                                         (vector (cons 1 x) (cons 2 y))))
                         'vector))
         (targets (map 'vector
                       (lambda (input)
                         (if (< 0.5 (cdr (aref input 0)))
                             1
                             -1))
                       inputs))
         (problem (make-problem targets inputs)))
    (assert (= (length targets) (problem-size problem)))
    (let ((parameter (make-parameter)))
      (assert (check-parameter problem parameter))
      (flet ((test-model (model)
               (assert (= 2 (or (n-classes model))))
               (loop for i below (length inputs) do
                 (format t "~S: ~S~%" i (aref inputs i))
                 (predict-values model (aref inputs i)))))
        (let ((model (train problem parameter))
              (filename (merge-pathnames (make-pathname :name "test-model")
                                         *liblinear-dir*)))
          (test-model model)
          (save-model model filename)
          (test-model (load-model filename)))))))

(defun test-predict-probabilities ()
  (let* ((inputs (coerce (loop repeat 1000
                               collect (let ((x (random 1d0))
                                             (y (random 1d0)))
                                         (vector (cons 1 x) (cons 2 y))))
                         'vector))
         (targets (map 'vector
                       (lambda (input)
                         (cond ((and (> 0.4 (cdr (aref input 0)))
                                     (> 0.4 (cdr (aref input 1))))
                                0)
                               ((and (> 0.4 (cdr (aref input 0)))
                                     (< 0.6 (cdr (aref input 1))))
                                1)
                               ((and (< 0.6 (cdr (aref input 0)))
                                     (< 0.6 (cdr (aref input 1))))
                                2)
                               ((and (< 0.6 (cdr (aref input 0)))
                                     (> 0.4 (cdr (aref input 1))))
                                3)
                               (t
                                4)))
                       inputs))
         (problem (make-problem targets inputs)))
    (assert (= (length targets) (problem-size problem)))
    (let ((parameter (make-parameter)))
      (assert (check-parameter problem parameter))
      (flet ((test-model (model)
               (assert (= 5 (n-classes model)))
               (let ((n-misses 0)
                     (labels (get-labels model)))
                 (format *trace-output* "labels: ~S~%" (get-labels model))
                 (loop for i below (length inputs)
                       do (multiple-value-bind (label probabilities)
                              (predict-probabilities model (aref inputs i))
                            (when (/= label (aref targets i))
                              (incf n-misses))
                            (let ((label-probability
                                    (aref probabilities
                                          (position label labels :test #'=))))
                              (assert
                               (every (lambda (probability)
                                        (<= probability label-probability))
                                      probabilities)))))
                 (format *trace-output* "~S misses out of ~S~%"
                         n-misses (length inputs)))))
        (let ((model (train problem parameter))
              (filename (merge-pathnames (make-pathname :name "test-model")
                                         *liblinear-dir*)))
          (test-model model)
          (save-model model filename)
          (test-model (load-model filename)))))))

(defun test-normalizer ()
  (let* ((data (vector (vector (cons 1 4.0) (cons 3 -5.0))
                       (vector (cons 1 -2.0) (cons 3 3.0))
                       (vector (cons 1 1.0) (cons 4 6.0))))
         (normalizer (make-normalizer data)))
    (map-normalized-input normalizer (aref data 0)
                          (lambda (index value)
                            (assert (= value (ecase index
                                               ((1) 1)
                                               ((3) -1))))))
    (let ((string (with-output-to-string (s)
                    (write-normalizer normalizer s))))
      (with-input-from-string (s string)
        (let ((normalizer2 (read-normalizer s)))
          (assert (= (lower normalizer) (lower normalizer2)))
          (assert (= (upper normalizer) (upper normalizer2)))
          (assert (string= (prin1-to-string (min-maxes normalizer))
                           (prin1-to-string (min-maxes normalizer2)))))))))


(defun test ()
  (test-problem)
  (test-predict-values)
  (test-predict-probabilities)
  (test-normalizer))

#|

(test)

|#
