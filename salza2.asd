;;;; $Id: salza2.asd,v 1.8 2007/12/20 16:35:00 xach Exp $

(asdf:defsystem #:salza2
  :components ((:file "package")
               (:file "specials"
                      :depends-on ("package"
                                   "crc32"))
               (:file "types"
                      :depends-on ("package"
                                   "specials"))
               (:file "checksum"
                      :depends-on ("package"))
               (:file "adler32"
                      :depends-on ("checksum"))
               (:file "crc32"
                      :depends-on ("checksum"))
               (:file "chains"
                      :depends-on ("package"
                                   "specials"))
               (:file "bitstream"
                      :depends-on ("package"))
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
                                   "bitstream"))
               (:file "utilities"
                      :depends-on ("package"))
               (:file "zlib"
                      :depends-on ("package"
                                   "adler32"
                                   "compressor"))
               (:file "gzip"
                      :depends-on ("package"
                                   "crc32"
                                   "compressor"))
               (:file "user"
                      :depends-on ("package"
                                   "compressor"
                                   "zlib"
                                   "gzip"))))
