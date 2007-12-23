;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:salza2)

(defun gzip-stream (input output)
  (let ((callback (lambda (data end)
                    (write-sequence data
                                    output
                                    :end end)))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (with-compressor (compressor :class 'gzip-compressor
                                 :callback callback)
      (loop
       (let ((end (read-sequence buffer input)))
         (when (zerop end)
           (return))
         (compress-octet-vector buffer compressor :end end))))))

(defun gzip-file (input output &key (if-exists :supersede))
  (with-open-file (istream input :element-type '(unsigned-byte 8))
    (with-open-file (ostream output
                             :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists if-exists)
      (gzip-stream istream ostream)))
  (probe-file output))

(defun compress-data (data compressor-class)
  (let ((chunks '())
        (size 0))
    (with-compressor (compressor :class compressor-class
                                 :callback (lambda (buffer end)
                                             (incf size end)
                                             (push (subseq buffer 0 end)
                                                   chunks)))
      (salza2:compress-octet-vector data compressor))
    (let ((compressed (make-array size :element-type '(unsigned-byte 8)))
          (start 0))
      (dolist (chunk (nreverse chunks))
        (replace compressed chunk :start1 start)
        (incf start (length chunk)))
      compressed)))

(defun deflate-compress (data)
  (compress-data data 'deflate-compressor))

(defun zlib-compress (data)
  (compress-data data 'zlib-compressor))

(defun gzip-compress (data)
  (compress-data data 'gzip-compressor))



