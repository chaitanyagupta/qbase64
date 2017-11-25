(defpackage #:base64-benchmark
  (:use #:cl))

(in-package #:base64-benchmark)

(defun benchmark-type (name)
  (if (member name '("encode" "encode-no-linebreak" "decode" "decode-no-linebreak")
              :test #'string=)
      (intern (string-upcase name) "KEYWORD")
      (error "Unknown name for benchmark type: ~A" name)))

(defun split-sequence (separator sequence &key (test #'eql))
  (let ((result nil))
    (loop
       for start = 0 then (1+ pos)
       for pos = (position separator sequence :test test :start start)
       do (push (subseq sequence start pos) result)
       while pos
       finally (return (nreverse result)))))

(defun find-executable (name &optional (path (sb-posix:getenv "PATH")))
  (or (probe-file name)
      (some (lambda (directory)
              (probe-file
               (merge-pathnames name
                                (make-pathname :directory `(:absolute ,directory)))))
            (split-sequence #\: path))))

(defclass benchmark-module ()
  ((preparedp :initform nil :accessor preparedp)))

(defgeneric prepare (module))

(defmethod prepare :before ((module benchmark-module))
  (format t "~&Preparing module ~A...~%" (class-name (class-of module))))

(defgeneric run (module benchmark-type input-file output-file error-file))

(defmethod run :before ((module benchmark-module) benchmark-type input-file output-file error-file)
  (format t "~&Running module ~A...~%" (class-name (class-of module))))

(defvar *benchmark-modules* nil)

(defun load-benchmark-module (name)
  (let ((class-name (find-class (intern (string-upcase name)) nil)))
    (when (not class-name)
      (load (compile-file name)))
    (pushnew name *benchmark-modules* :test #'string=)))

(defun module-name (module)
  (string-downcase (class-name (class-of module))))

(defun run-and-collect-result (module benchmark-type)
  (flet ((benchmark-type-is (name)
           (string= benchmark-type name)))
    (let* ((input-file (cond ((benchmark-type-is "encode") "test.bin")
                             ((benchmark-type-is "encode-no-linebreak") "test.bin")
                             ((benchmark-type-is "decode") "test-base64.txt")
                             ((benchmark-type-is "decode-no-linebreak") "test-base64-no-linebreak.txt")
                             (t (error "Unknown benchmark type ~A" benchmark-type))))
           (output-file "/dev/null")
           (error-file (make-string-output-stream))
           (start-time (get-internal-real-time))
           (process (run module benchmark-type input-file output-file error-file))
           (end-time (get-internal-real-time))
           (error-string (get-output-stream-string error-file))
           (exit-code (sb-ext:process-exit-code process)))
      (if (zerop exit-code)
          (list :success (- end-time start-time)
                :stderr error-string)
          (list :process-error exit-code
                :stderr error-string)))))

(defun run-benchmarks (benchmark-types module-names)
  (mapc #'load-benchmark-module module-names)
  (let ((results nil))
    (dolist (name (intersection module-names *benchmark-modules* :test #'string=))
      (let* ((class-name (find-class (intern (string-upcase name) #.*package*)))
             (module (make-instance class-name)))
        (prepare module)
        (dolist (benchmark-type benchmark-types)
          (push (append (list :module name :benchmark-type benchmark-type)
                        (run-and-collect-result module benchmark-type))
                results))))
    results))

(defun present-results (results)
  (dolist (benchmark-type '("encode" "encode-no-linebreak" "decode" "decode-no-linebreak"))
    (let ((benchmark-results (remove benchmark-type results
                                     :key (lambda (result) (getf result :benchmark-type))
                                     :test #'string/=)))
      (when benchmark-results
        (format t "~&~A~%" #1=benchmark-type)
        (loop repeat (length #1#)
           do (princ "=")
           finally (terpri))
        (dolist (result benchmark-results)
          (format t "~A~%" #2=(getf result :module))
          (loop repeat (length #2#)
             do (princ "-")
             finally (terpri))
          (format t "Result: ~A~%" (if (getf result :success) "Success" "Failed"))
          (princ (getf result :stderr))
          (terpri))))))

(defun run! (benchmark-types module-names)
  (present-results (run-benchmarks benchmark-types module-names)))

(defparameter *run-time-utility* t)

(defun run-program (program args &rest remaining-args)
  (if *run-time-utility*
      (apply #'sb-ext:run-program
             "/usr/bin/time"
             (append (list "-lp" (namestring program)) args)
             remaining-args)
      (apply #'sb-ext:run-program program args remaining-args)))

;;; lisp module

(defparameter *sbcl-bin* (or (sb-posix:getenv "SBCL_BIN")
                             (find-executable "sbcl")))

(defclass lisp-module (benchmark-module)
  ())

(defmethod prepare ((module lisp-module))
  (unless *sbcl-bin*
    (error "Couldn't find sbcl binary"))
  (let ((process (sb-ext:run-program *sbcl-bin*
                                     (list "--load"
                                           (format nil "~A-prepare.lisp" (module-name module))
                                           "--quit")
                                     :output *standard-output*
                                     :error :output)))
    (unless (zerop (sb-ext:process-exit-code process))
      (error "Couldn't save core file"))))

(defmethod run ((module lisp-module) benchmark-type input-file output-file error-file)
  (run-program *sbcl-bin*
               (list "--core"
                     (format nil "~A.core" (module-name module))
                     benchmark-type input-file output-file)
               :error error-file
               :if-error-exists :supersede))
