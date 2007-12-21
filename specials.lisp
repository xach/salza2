;;;; $Id: specials.lisp,v 1.4 2007/12/19 20:52:49 xach Exp $

(in-package #:salza2)

(defparameter +input-limit+ 32768)
(defparameter +input-limit-mask+ (1- +input-limit+))
(defparameter +buffer-size+ (* +input-limit+ 2))
(defparameter +buffer-size-mask+ (1- +buffer-size+))

(defparameter +input-size+ #x10000)
(defparameter +input-mask+ #x0FFFF)
(defparameter +hashes-size+ 8209)
(defparameter +radix+ 109)
(defparameter +rmax+ (* +radix+ +radix+))

(defparameter +bitstream-buffer-size+ 4096)
(defparameter +bitstream-buffer-mask+ (1- +bitstream-buffer-size+))
(defparameter +bitstream-buffer-bits+ (* +bitstream-buffer-size+ 8))
(defparameter +bitstream-buffer-bitmask+ (1- +bitstream-buffer-bits+))

(defconstant +final-block+ #b1)
(defconstant +fixed-tables+ #b01)

(defparameter *crc32-table* (crc32-table))
