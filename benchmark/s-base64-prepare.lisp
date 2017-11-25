(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :s-base64))

(defun encode (in out benchmark-type)
  (s-base64:encode-base64 in out (string= benchmark-type "encode"))
  (write-char #\Newline out))

(defun decode (in out benchmark-type)
  (declare (ignore benchmark-type))
  (s-base64:decode-base64 in out))

(load "cl-prepare")

(save-image "s-base64")
