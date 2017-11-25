(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :qbase64))

(defun encode (in out benchmark-type &key (size 4098))
  (declare (type string benchmark-type))
  (declare (optimize speed))
  (with-open-stream (base64-out (make-instance 'qbase64:encode-stream
                                               :underlying-stream out
                                               :linebreak (if (string= benchmark-type "encode")
                                                              76
                                                              0)))
    (let ((seq (make-array size :element-type '(unsigned-byte 8))))
      (loop
         for pos = (read-sequence seq in)
         do (write-sequence seq base64-out :end pos)
         until (< pos (length seq)))
      (when (string= benchmark-type "encode-no-linebreak")
        (write-char #\Newline out)))))

(defun decode (in out benchmark-type &key (size 4098))
  (declare (ignore benchmark-type))
  (declare (optimize speed))
  (with-open-stream (base64-in (make-instance 'qbase64:decode-stream
                                              :underlying-stream in))
    (let ((seq (make-array size :element-type '(unsigned-byte 8))))
      (loop
         for pos = (read-sequence seq base64-in)
         do (write-sequence seq out :end pos)
         until (< pos (length seq))))))

(load "cl-prepare")

(save-image "qbase64")
