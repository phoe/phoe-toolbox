;;;; bag.lisp

(defpackage #:phoe-toolbox/bag
  (:use #:cl
        #:alexandria)
  (:export
   #:bag #:make-bag #:bag-contents
   #:bag-insert #:bag-count #:bag-remove #:bag-compress))

(in-package #:phoe-toolbox/bag)

(defstruct (bag (:constructor make-bag ())
                (:print-function print-bag))
  (contents (make-array 0 :fill-pointer t :adjustable t) :type (array t (*))))

(defun print-bag (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t)
    (format stream "(~A)" (fill-pointer (bag-contents object)))))

(defun bag-insert (bag element)
  (check-type bag bag)
  (vector-push-extend element (bag-contents bag))
  (values))

(defun bag-count (bag)
  (check-type bag bag)
  (fill-pointer (bag-contents bag)))

(defun bag-remove (bag)
  (check-type bag bag)
  (if (non-positive-integer-p (bag-count bag))
      (values nil nil)
      (let* ((count (bag-count bag))
             (n (random count))
             (result (aref (bag-contents bag) n)))
        (setf (aref (bag-contents bag) n)
              (aref (bag-contents bag) (1- count)))
        (decf (fill-pointer (bag-contents bag)))
        (values result t))))

(defun bag-compress (bag)
  (adjust-array (bag-contents bag) (bag-count bag))
  (values))
