;;;; $Id: gzip.lisp,v 1.2 2007/12/20 16:30:03 xach Exp $

(in-package #:salza2)

(defvar *gzip-signature*
  (make-array 2
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x1F #x8B)))

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-fast-compression+ 4)
(defconstant +gzip-flags+ 0)
(defconstant +gzip-unix-os+ 3)
(defconstant +gzip-mtime+ 0)

(defun gzip-write-u32 (value bitstream)
  ;; LSB
  (write-octet (ldb (byte 8 0) value) bitstream)
  (write-octet (ldb (byte 8 8) value) bitstream)
  (write-octet (ldb (byte 8 16) value) bitstream)
  (write-octet (ldb (byte 8 24) value) bitstream))

(defclass gzip-compressor (deflate-compressor)
  ((checksum
    :initarg :checksum
    :accessor checksum)
   (data-length
    :initarg :data-length
    :accessor data-length))
  (:default-initargs
   :checksum (make-instance 'crc32-checksum)
   :data-length 0))

(defmethod start-data-format :before ((compressor gzip-compressor))
  (let ((bitstream (bitstream compressor)))
    (write-octet-vector *gzip-signature* bitstream)
    (write-octet +gzip-deflate-compression+ bitstream)
    (write-octet +gzip-flags+ bitstream)
    (gzip-write-u32 +gzip-mtime+ bitstream)
    (write-octet +gzip-fast-compression+ bitstream)
    (write-octet +gzip-unix-os+ bitstream)))

(defmethod process-input :after ((compressor gzip-compressor)
                                 input start count)
  (incf (data-length compressor) count)
  (update (checksum compressor) input start count))

(defmethod finish-data-format :after ((compressor gzip-compressor))
  (let ((bitstream (bitstream compressor)))
    (gzip-write-u32 (result (checksum compressor)) bitstream)
    (gzip-write-u32 (data-length compressor) bitstream)))
