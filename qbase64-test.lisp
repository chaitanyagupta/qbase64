(in-package #:cl-user)

(defpackage #:qbase64-test
  (:use #:cl #:qbase64 #:fiveam)
  (:import-from #:qbase64 #:octets #:make-octet-vector))

(in-package #:qbase64-test)

(def-suite encoder)

(in-suite encoder)

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

(test empty
  (let ((octets (octets)))
    (is (string= "" (qbase64::octets-to-base64 octets)))))

(test one
  (let ((octets (random-octets 1)))
    (is (string= (external-encode octets) (qbase64::octets-to-base64 octets)))))

(test two
  (let ((octets (random-octets 2)))
    (is (string= (external-encode octets) (qbase64::octets-to-base64 octets)))))

(test three
  (let ((octets (random-octets 3)))
    (is (string= (external-encode octets) (qbase64::octets-to-base64 octets)))))



