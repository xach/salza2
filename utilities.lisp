;;;; $Id: utilities.lisp,v 1.4 2007/12/17 12:52:21 xach Exp $

(in-package #:salza2)

(defvar *dtrace-enabled* nil)

(defmacro dtrace (&rest vars)
  `(when *dtrace-enabled*
     (format *trace-output* "~{~&; ~S => ~S~%~}~%"
             (list ,@(loop for sym in vars
                           collect `(quote ,sym)
                           collect sym)))))

(defun toggle-dtrace ()
  (setf *dtrace-enabled* (not *dtrace-enabled*)))


(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun octet-vector (&rest elements)
  (make-array (length elements)
              :element-type 'octet
              :initial-contents elements))
