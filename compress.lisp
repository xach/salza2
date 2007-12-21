;;;; $Id: compress.lisp,v 1.6 2007/12/19 20:55:16 xach Exp $

(in-package #:salza2)

(defun compress (input chains start end
                 literal-fun length-fun distance-fun)
  (declare (type input-buffer input)
           (type chains-buffer chains)
           (type input-index start end)
           (type function literal-fun length-fun distance-fun)
           (optimize speed))
  (let ((p start))
    (loop
     (when (= p end)
       (return))
     (multiple-value-bind (length distance)
         (longest-match p input chains end 4)
       (declare (type (integer 0 258) length)
                (type (integer 0 32768) distance))
       (cond ((zerop length)
              (funcall literal-fun (aref input p))
              (setf p (logand (+ p 1) #xFFFF)))
             (t
              (funcall length-fun length)
              (funcall distance-fun distance)
              (setf p (logand (+ p length) #xFFFF))))))))
