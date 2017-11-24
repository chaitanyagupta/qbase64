;;;; package.lisp

(defpackage #:qbase64
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:+max-bytes-length+
   #:+max-string-length+
   ;; encode
   #:encoder
   #:make-encoder
   #:encode
   #:encode-stream
   #:encode-bytes
   ;; decode
   #:decoder
   #:make-decoder
   #:decode
   #:decode-stream
   #:decode-string))
