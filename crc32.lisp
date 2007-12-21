;;;; $Id: crc32.lisp,v 1.2 2007/12/19 20:53:26 xach Exp $

(in-package #:salza2)

(defun crc32-table ()
  (let ((table (make-array 512 :element-type '(unsigned-byte 16))))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1)))
          (setf (aref table (ash n 1)) (ldb (byte 16 16) c)
                (aref table (1+ (ash n 1))) (ldb (byte 16 0) c)))))))

(defun crc32 (high low buf start count)
  (declare (type (unsigned-byte 16) high low)
           (type (integer 0 32768) count)
           (type octet-vector buf)
           (optimize speed))
  (let ((i start)
        (table *crc32-table*))
    (declare (type (integer 0 65536) i)
             (type (simple-array (unsigned-byte 16) (*)) table))
    (dotimes (j count (values high low))
      (let ((index (logxor (logand low #xFF) (aref buf i))))
        (declare (type (integer 0 255) index))
        (let ((high-index (ash index 1))
              (low-index (1+ (ash index 1))))
          (declare (type (integer 0 511) high-index low-index))
          (let ((t-high (aref table high-index))
                (t-low (aref table low-index)))
            (declare (type (unsigned-byte 16) t-high t-low))
            (incf i)
            (setf low (logxor (ash (logand high #xFF) 8)
                              (ash low -8)
                              t-low))
            (setf high (logxor (ash high -8) t-high))))))))

;;; Class interface

(defclass crc32-checksum (checksum)
  ((low
    :initarg :low
    :accessor low)
   (high
    :initarg :high
    :accessor high))
  (:default-initargs
   :low #xFFFF
   :high #xFFFF))

(defmethod update ((checksum crc32-checksum) input start count)
  (setf (values (high checksum)
                (low checksum))
        (salza2::crc32 (high checksum) (low checksum)
                       input start count)))

(defmethod result ((checksum crc32-checksum))
  (+ (ash (logxor (high checksum) #xFFFF) 16)
     (logxor (low checksum) #xFFFF)))

(defmethod result-octets ((checksum crc32-checksum))
  (ub32-octets (result checksum)))
