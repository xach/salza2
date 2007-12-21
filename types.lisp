;;;; $Id: types.lisp,v 1.3 2007/12/09 00:54:32 xach Exp $

(in-package #:salza2)

(deftype array-index ()
  `(mod ,array-dimension-limit))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype input-index ()
  '(unsigned-byte 16))

(deftype input-buffer ()
  `(simple-array (unsigned-byte 8) (,+input-size+)))

(deftype chains-buffer ()
  `(simple-array (unsigned-byte 16) (,+input-size+)))

(deftype hashes-buffer ()
  `(simple-array (unsigned-byte 16) (,+hashes-size+)))

(deftype hash ()
  `(integer 0 ,+hashes-size+))

(deftype bitstream-buffer ()
  `(simple-array (unsigned-byte 8) (,+bitstream-buffer-size+)))

(deftype bitstream-buffer-bit-count ()
  `(integer 0 ,+bitstream-buffer-bits+))
