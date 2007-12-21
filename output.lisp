;;;; $Id: output.lisp,v 1.1.1.1 2007/12/03 19:43:34 xach Exp $

(in-package #:salza2)

(defun output-error (data start end)
  (declare (ignore data start end))
  (error "No output callback defined!"))

(defclass output ()
  ((data
    :initarg :data
    :accessor data
    :documentation "An octet vector that holds compressed data to
    be processed by the applicaton callback.")
   (start
    :initarg :start
    :accessor start
    :documentation "The index of the first octet of compressed data.")
   (end
    :initarg :end
    :accessor end
    :documentation "The index after last octet of compressed data.")
   (callback
    :initarg :callback
    :accessor callback
    :documentation "A designator for a function of three
    arguments: an octet vector, start, and end. The application
    can treat octets START through END-1 as compressed data to
    process as needed."))
  (:default-initargs
   :data (make-array 4096 :element-type 'octet)
   :start 0
   :end 0
   :callback 'output-error))

(defgeneric call-callback (object)
  (:method (output)
    (funcall (callback output) (data output) (start output) (end output))
    (setf (start output) 0
          (end output) 0)))

(defgeneric fullp (object)
  (:method ((output output))
    (= (end output) (length (data output)))))

(defgeneric output-octet (octet output)
  (:method (octet output)
    (setf (aref (data output) (end output)) octet)
    (when (fullp output)
      (call-callback output))))

(defgeneric output-literal (code output)
  (:method (code output)
    (print (list 'output-literal code))))

(defgeneric output-distance (distance output)
  (:method (distance output)
    (print (list 'output-distance distance))))

(defgeneric output-length (length output)
  (:method (length output)
    (print (list 'output-length length))))
