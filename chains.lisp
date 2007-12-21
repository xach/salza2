;;;; $Id: chains.lisp,v 1.9 2007/12/19 20:57:27 xach Exp $

(in-package #:salza2)

(defun hash-value (input position)
  (+ (* #.+rmax+ (aref input position))
     (* #.+radix+ (aref input (logand #.+input-mask+ (+ position 1))))
     (aref input (logand #.+input-mask+ (+ position 2)))))

(declaim (inline mod8191))
(defun mod8191 (z)
  (declare (type (integer 0 3057705) z))
  (let ((zz (+ (ash z -13) (logand #x1FFF z))))
    (if (< zz #x1FFF)
        zz
        (- zz #x1FFF))))

(defun update-chains (input hashes chains start count)
  (declare (type input-buffer input)
           (type hashes-buffer hashes)
           (type chains-buffer chains)
           (type input-index start)
           (type (integer 0 32768) count)
           (optimize speed))
  (when (< count 3)
    (return-from update-chains))
  (let* ((hash (hash-value input start))
         (p0 start)
         (p1 (logand (+ start 2) #xFFFF)))
    (declare (type (integer 0 3057705) hash))
    (loop
     (let ((hash-index (mod8191 hash)))
       ;; Stuff the old hash index into chains at p0
       (setf (aref chains p0) (aref hashes hash-index))
       ;; Stuff p0 into the hashes
       (setf (aref hashes hash-index) p0)
       ;; Tentatively advance; if we hit the end, don't do the rest of
       ;; the hash update
       (setf p1 (logand (1+ p1) #xFFFF))
       (decf count)
       (when (= count 2)
         (return))
       ;; We're not at the end, so lop off the high, shift left, and
       ;; add the low to form a new hash value
       (setf hash (- hash (* (aref input p0) 11881)))
       (setf hash (* hash 109))
       (setf p0 (logand (1+ p0) #xFFFF))
       (setf hash (+ hash (aref input p1)))))))
