;;;; $Id: package.lisp,v 1.4 2007/12/20 21:05:39 xach Exp $

(defpackage #:salza2
  (:use #:cl)
  (:export
   ;; bitstream
   #:bitstream
   #:write-bits
   #:write-octet
   #:write-octet-vector
   #:flush
   #:callback
   ;; compressor
   #:deflate-compressor
   #:start-data-format
   #:compress-octet
   #:compress-octet-vector
   #:process-input
   #:finish-data-format
   #:finish-compression
   ;; zlib
   #:zlib-compressor
   ;; gzip
   #:gzip-compressor
   ;; checksum
   #:update
   #:result
   #:result-octets
   #:adler32-checksum
   #:crc32-checksum
   ;; user
   #:with-compressor
   #:gzip-stream
   #:gzip-file
   #:compress-data
   #:deflate-compress
   #:zlib-compress
   #:gzip-compress))
