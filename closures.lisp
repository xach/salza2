;;;; $Id: closures.lisp,v 1.1 2007/12/07 17:14:28 xach Exp $

(in-package #:salza2)

(defun make-huffman-writer (huffman-codes bitstream)
  (let ((codes (codes huffman-codes))
        (sizes (sizes huffman-codes))
        (buffer (buffer bitstream))
        (callback (callback bitstream)))
    (lambda (value)
      (setf (bits bitstream)
            (merge-bits (aref codes value)
                        (aref sizes value)
                        buffer
                        (bits bitstream)
                        callback)))))

(defun make-byte-writer (bitstream)
  (let ((buffer (buffer bitstream))
        (callback (callback bitstream)))
    (lambda (byte)
      (setf (bits bitstream)
            (merge-byte byte buffer (bits bitstream) callback)))))
