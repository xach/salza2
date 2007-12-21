;;;; $Id: adler32.lisp,v 1.5 2007/12/19 20:57:08 xach Exp $

(in-package #:salza2)

(defconstant +adler32-base+ 65521)

(defun adler32-update (adler-high adler-low buf start count)
  (declare (type array-index start)
           (type (integer 0 65536) count)
           (type (unsigned-byte 16) adler-high adler-low)
           (type octet-vector buf)
           (optimize speed))
  (cond ((zerop count)
         (values adler-high adler-low))
        (t
         (let ((length count)
               (i 0)
               (k 0)
               (s1 adler-low)
               (s2 adler-high))
           (declare (type (integer 0 32658) length)
                    (type (integer 0 16) i k)
                    (type (integer 0 65536) s1 s2))
           (tagbody
            loop
              (setf k (min length 16))
              (decf length k)
            sum
              (setf s1 (+ (aref buf (logand #xFFFF (+ start i))) s1))
              (setf s2 (+ s1 s2))
              (decf k)
              (incf i)
              (unless (zerop k)
                (go sum))
              (setf s1 (mod s1 +adler32-base+))
              (setf s2 (mod s2 +adler32-base+))
              (unless (zerop length)
                (go loop)))
           (values s2 s1)))))

;;; Class interface

(defclass adler32-checksum (checksum)
  ((high
    :initarg :high
    :accessor high)
   (low
    :initarg :low
    :accessor low))
  (:default-initargs
   :high 0
   :low 1))

(defmethod result ((checksum adler32-checksum))
  (+ (ash (high checksum) 16)
     (low checksum)))

(defmethod result-octets ((checksum adler32-checksum))
  (ub32-octets (result checksum)))

(defmethod update ((checksum adler32-checksum) buffer start count)
  (setf (values (high checksum)
                (low checksum))
        (adler32-update (high checksum)
                        (low checksum)
                        buffer
                        start
                        count)))
