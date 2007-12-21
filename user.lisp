;;;; $Id: user.lisp,v 1.1 2007/12/20 21:04:19 xach Exp $

(in-package #:salza2)

(defmacro with-compressor ((var &key (class 'zlib-compressor) callback)
                           &body body)
  `(let ((,var (make-instance ,class
                              ,@(when callback (list :callback callback)))))
     (multiple-value-prog1 
         (progn ,@body)
       (finish-compression ,var))))

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



