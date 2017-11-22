(in-package #:cl-user)

(defpackage #:qbase64-test
  (:use #:cl #:fiveam)
  (:import-from #:qbase64 #:bytes #:make-byte-vector))

(in-package #:qbase64-test)

;;; utils

(defun external-encode (bytes)
  (temporary-file:with-open-temporary-file (tmp :direction :output :element-type '(unsigned-byte 8))
    (write-sequence bytes tmp)
    (force-output tmp)
    (uiop:run-program `("base64" "-i" ,(namestring tmp)) :output '(:string :stripped t))))

(let ((gen (gen-integer :min 0 :max 255)))
  (defun random-byte ()
    (funcall gen)))

(defun random-bytes (size)
  (let ((bytes (make-byte-vector size)))
    (dotimes (i size bytes)
      (setf (aref bytes i) (random-byte)))))

;;; encoder tests

(def-suite encoder)

(test (encode-bytes :suite encoder)
  (dolist (size (list 0 1 2 3 4 5 6 7 8 9 10 100))
    (let* ((bytes (random-bytes size))
           (encoded (qbase64:encode-bytes bytes))
           (external-encoded (external-encode bytes)))
      (is (string= external-encoded encoded)
          "Failed for size ~A: Expected ~S for ~S, but got ~S"
          size external-encoded bytes encoded))))

(test (encode-stream-states :suite encoder)
  (with-output-to-string (s)
    (let ((out (make-instance 'qbase64:encode-stream
                              :underlying-stream s)))
      (is (open-stream-p out))
      (is (equalp (stream-element-type out) '(unsigned-byte 8)))
      (is (output-stream-p out))
      (is (not (input-stream-p out)))
      (close out)
      (is (not (open-stream-p out)))
      ;; underlying stream is not closed automatically
      (is (open-stream-p s)))))

;;; decoder tests

(def-suite decoder)

(test (decode-string :suite decoder)
  (dolist (size (list 0 1 2 3 4 5 6 7 8 9 10 100))
    (let* ((bytes (random-bytes size))
           (string (external-encode bytes))
           (decoded (qbase64:decode-string string)))
      (is (equalp bytes decoded)
          "Failed for size ~A: Expected ~S for ~S, but got ~S"
          size bytes string decoded))))

(test (decode-stream-states :suite decoder)
  (with-input-from-string (s "AQID")
    (let ((in (make-instance 'qbase64:decode-stream
                             :underlying-stream s)))
      (is (open-stream-p in))
      (is (equalp (stream-element-type in) '(unsigned-byte 8)))
      (is (input-stream-p in))
      (is (not (output-stream-p in)))
      (close in)
      (is (not (open-stream-p in)))
      ;; underlying stream is not closed automatically
      (is (open-stream-p s)))))
