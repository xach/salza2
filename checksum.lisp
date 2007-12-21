;;;; $Id: checksum.lisp,v 1.2 2007/12/20 16:32:00 xach Exp $

(in-package #:salza2)

(defclass checksum () ())

(defgeneric update (checksum buffer start count)
  (:documentation "Update the CHECKSUM object with COUNT octets
  from BUFFER, starting from START."))

(defgeneric result (checksum)
  (:documentation "Return the result of CHECKSUM as an integer."))

(defgeneric result-octets (checksum)
  (:documentation "Return the result of CHECKSUM as a list of
  octets, in MSB order."))

(defun ub32-octets (result)
  (list (ldb (byte 8 24) result)
        (ldb (byte 8 16) result)
        (ldb (byte 8  8) result)
        (ldb (byte 8  0) result)))
