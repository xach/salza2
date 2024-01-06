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

(asdf:defsystem #:salza2
  :author "Zachary Beane <xach@xach.com>"
  :license "BSD"
  :version "2.1"
  :description "Create compressed data in the ZLIB, DEFLATE, or GZIP
  data formats"
  :depends-on ("trivial-gray-streams")
  :in-order-to ((asdf:test-op (asdf:test-op "salza2/test")))
  :components ((:file "package")
               (:file "reset"
                      :depends-on ("package"))
               (:file "specials"
                      :depends-on ("package"))
               (:file "types"
                      :depends-on ("package"
                                   "specials"
                                   "types"))
               (:file "checksum"
                      :depends-on ("package"
                                   "reset"))
               (:file "adler32"
                      :depends-on ("checksum"
                                   "types"))
               (:file "crc32"
                      :depends-on ("checksum"
                                   "types"))
               (:file "chains"
                      :depends-on ("package"
                                   "types"))
               (:file "bitstream"
                      :depends-on ("package"
                                   "specials"
                                   "types"
                                   "reset"))
               (:file "matches"
                      :depends-on ("package"
                                   "types"))
               (:file "compress"
                      :depends-on ("types"
                                   "matches"))
               (:file "huffman"
                      :depends-on ("package"))
               (:file "closures"
                      :depends-on ("huffman"
                                   "bitstream"))
               (:file "compressor"
                      :depends-on ("package"
                                   "closures"
                                   "utilities"
                                   "specials"
                                   "bitstream"
                                   "reset"))
               (:file "utilities"
                      :depends-on ("package"))
               (:file "zlib"
                      :depends-on ("package"
                                   "adler32"
                                   "reset"
                                   "compressor"))
               (:file "gzip"
                      :depends-on ("package"
                                   "crc32"
                                   "reset"
                                   "compressor"))
               (:file "user"
                      :depends-on ("package"
                                   "compressor"
                                   "zlib"
                                   "gzip"))
               (:file "stream"
                      :depends-on ("package"))))

(asdf:defsystem #:salza2/test
  :author "Zachary Beane <xach@xach.com>"
  :license "BSD"
  :version "2.1"
  :description "Tests for Salza2 system"
  :depends-on ("salza2" "parachute" "flexi-streams" "chipz")
  :perform (asdf:test-op (op c)
                         (unless (eql :passed
                                      (uiop:symbol-call
                                       :parachute :status
                                       (uiop:symbol-call :parachute :test :salza2-test)))
                           (error "Tests failed")))
  :pathname "test/"
  :components ((:file "package")
               (:file "stream" :depends-on ("package"))))
