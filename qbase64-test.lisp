(in-package #:cl-user)

(defpackage #:qbase64-test
  (:use #:cl #:qbase64 #:fiveam)
  (:import-from #:qbase64 #:octets #:make-octet-vector))

(in-package #:qbase64-test)

;;; utils

(defun external-encode (octets)
  (temporary-file:with-open-temporary-file (tmp :direction :output :element-type '(unsigned-byte 8))
    (write-sequence octets tmp)
    (force-output tmp)
    (uiop:run-program `("base64" "-i" ,(namestring tmp)) :output '(:string :stripped t))))

(let ((gen (gen-integer :min 0 :max 255)))
  (defun random-octet ()
    (funcall gen)))

(defun random-octets (size)
  (let ((octets (make-octet-vector size)))
    (dotimes (i size octets)
      (setf (aref octets i) (random-octet)))))

;;; encoder tests

(def-suite encoder)

(in-suite encoder)

(test octets-to-base64
  (dolist (size (list 0 1 2 3 4 5 6 7 8 9 10 100))
    (let* ((octets (random-octets size))
           (encoded (qbase64::octets-to-base64 octets))
           (external-encoded (external-encode octets)))
      (is (string= external-encoded encoded)
          "Failed for size ~A: Expected ~S for ~S, but got ~S"
          size external-encoded octets encoded))))

;;; decoder tests

(def-suite decoder)

(in-suite decoder)

(test base64-to-octets
  (dolist (size (list 0 1 2 3 4 5 6 7 8 9 10 100))
    (let* ((octets (random-octets size))
           (string (external-encode octets))
           (decoded (qbase64::base64-to-octets string)))
      (is (equalp octets decoded)
          "Failed for size ~A: Expected ~S for ~S, but got ~S"
          size octets string decoded))))
