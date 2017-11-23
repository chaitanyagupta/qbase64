(in-package #:cl-user)

(defpackage #:qbase64-test
  (:use #:cl #:fiveam #:temporary-file)
  (:import-from #:qbase64 #:bytes #:make-byte-vector))

(in-package #:qbase64-test)

;;; utils

(defun external-encode (bytes &key (linebreak 0))
  (if (zerop (length bytes))
      ""
      (with-open-temporary-file (tmp :direction :output :element-type '(unsigned-byte 8))
        (write-sequence bytes tmp)
        (force-output tmp)
        (let* ((encoded (uiop:run-program `("base64" "-b" ,(format nil "~A" linebreak) "-i" ,(namestring tmp)) :output (if (zerop linebreak) '(:string :stripped t) :string)))
               (length (length encoded)))
          (cond ((and (> length 1)
                      (string= (subseq encoded (- length 2))
                               (format nil "~A~A" #\Newline #\Newline)))
                 (subseq encoded 0 (1- length)))
                (t encoded))))))

(let ((gen (gen-integer :min 0 :max 255)))
  (defun random-byte ()
    (funcall gen)))

(defun random-bytes (size)
  (let ((bytes (make-byte-vector size)))
    (dotimes (i size bytes)
      (setf (aref bytes i) (random-byte)))))

(defun gen-random-encoding-set ()
  (let ((cache-table (make-hash-table :test #'equal)))
    (lambda (size linebreak)
      (let* ((key (cons size linebreak))
             (cached (gethash key cache-table)))
        (when (null cached)
          (let* ((bytes (random-bytes size))
                 (encoded (external-encode bytes :linebreak linebreak)))
            (setf cached (list bytes encoded)
                  (gethash key cache-table) cached)))
        cached))))

(let ((gen (gen-random-encoding-set)))
  (defun random-encoding-set (size linebreak)
    (funcall gen size linebreak)))

(defmacro with-encoding-set ((bytes-var encoded-var) size linebreak &body body)
  `(destructuring-bind (,bytes-var ,encoded-var)
       (random-encoding-set ,size ,linebreak)
     ,@body))

;;; encoder tests

(def-suite encoder)

(test (encode-bytes :suite encoder)
  (dolist (size (list 0 1 2 3 6 10 30 100 1024))
    (dolist (linebreak (list 0 1 2 3 5 10 30 100))
      (with-encoding-set (bytes encoded)
          size linebreak
        (let ((generated (qbase64:encode-bytes bytes :linebreak linebreak)))
          (is (string= generated encoded)
              "Failed for size ~A, linebreak ~A: Expected ~S for ~S, but got ~S"
              size linebreak encoded bytes generated))))))

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

(test (encode-stream-single-write :suite encoder)
  (dolist (size (list 0 1 2 3 6 10 30 100 1024))
    (dolist (linebreak (remove-if (lambda (l) (> l size)) (list 0 1 2 3 5 10 30 100)))
      (with-encoding-set (bytes encoded)
          size linebreak
        (let ((generated
               (with-output-to-string (str-out)
                 (with-open-stream (out (make-instance 'qbase64:encode-stream
                                                       :underlying-stream str-out
                                                       :linebreak linebreak))
                   (write-sequence bytes out)))))
          (is (string= generated encoded)
              "Failed for size ~A, linebreak ~A: Expected ~S for ~S, but got ~S"
              size linebreak encoded bytes generated))))))

(test (encode-stream-multi-write :suite encoder)
  (dolist (size (list 0 1 2 3 6 10 30 100 1024))
    (dolist (linebreak (remove-if (lambda (l) (> l size))
                                  (list 0 1 2 3 5 10 30 100)))
      (with-encoding-set (bytes encoded)
          size linebreak
        (dolist (seq-size (remove-if (lambda (s) (> s size))
                                     (list 1 2 3 5 10 30 100)))
          (let ((generated
                 (with-output-to-string (str-out)
                   (with-open-stream (out (make-instance 'qbase64:encode-stream
                                                         :underlying-stream str-out
                                                         :linebreak linebreak))
                     (loop
                        for start = 0 then end
                        for end = (min seq-size (length bytes)) then (min (+ start seq-size) (length bytes))
                        do (write-sequence bytes out :start start :end end)
                        until (= end (length bytes)))))))
            (is (string= generated encoded)
                "Failed for size ~A, linebreak ~A, seq-size ~A: Expected ~S for ~S, but got ~S"
                size linebreak seq-size encoded bytes generated)))))))

;;; decoder tests

(def-suite decoder)

(test (decode-string :suite decoder)
  (dolist (size (list 0 1 2 3 6 10 30 100 1024))
    (dolist (linebreak (list 0 1 2 3 5 10 30 100))
      (with-encoding-set (bytes encoded)
          size linebreak
        (let ((generated (qbase64:decode-string encoded)))
          (is (equalp generated bytes)
              "Failed for size ~A, linebreak ~A: Expected ~S for ~S, but got ~S"
              size linebreak bytes encoded generated))))))

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
