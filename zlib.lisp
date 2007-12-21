;;;; $Id: zlib.lisp,v 1.10 2007/12/20 16:30:09 xach Exp $

(in-package #:salza2)

(defclass zlib-compressor (deflate-compressor)
  ((adler32
    :initarg :adler32
    :accessor adler32))
  (:default-initargs
   :adler32 (make-instance 'adler32-checksum)))

(defmethod start-data-format :before ((compressor zlib-compressor))
  (let ((bitstream (bitstream compressor)))
    ;; FIXME: Replace these naked constants with symbolic constants.
    (write-octet 8 bitstream)
    (write-octet 153 bitstream)))

(defmethod process-input :after ((compressor zlib-compressor) input start count)
  (let ((checksum (adler32 compressor)))
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor zlib-compressor))
  (let ((bitstream (bitstream compressor)))
    (dolist (octet (result-octets (adler32 compressor)))
      (write-octet octet bitstream))))

