(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-base64))

(defun encode (in out benchmark-type)
  (declare (type string benchmark-type))
  (declare (optimize speed))
  (let* ((file-length (file-length in))
         (bytes (make-array file-length :element-type '(unsigned-byte 8))))
    (read-sequence bytes in)
    (base64:usb8-array-to-base64-stream bytes out
                                        :columns (if (string= benchmark-type "encode") 76 0))
    (write-char #\Newline out)))

(defun decode (in out benchmark-type)
  (declare (ignore benchmark-type))
  (declare (optimize speed))
  (let* ((file-length (file-length in))
         (string (make-string file-length :element-type 'base-char)))
    (read-sequence string in)
    (let ((bytes (base64:base64-string-to-usb8-array string)))
      (write-sequence bytes out))))

(load "cl-prepare")

(save-image "cl-base64")
