(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :parse-number))

(defpackage #:base64-benchmark
  (:use #:cl))

(in-package #:base64-benchmark)

(defparameter *benchmark-types* '("encode" "encode-no-linebreak" "decode" "decode-no-linebreak"))

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

(defvar *write-output-files* nil)

(defun run-and-collect-result (module benchmark-type)
  (flet ((benchmark-type-is (name)
           (string= benchmark-type name)))
    (let* ((input-file (cond ((benchmark-type-is "encode") "test.bin")
                             ((benchmark-type-is "encode-no-linebreak") "test.bin")
                             ((benchmark-type-is "decode") "test-base64.txt")
                             ((benchmark-type-is "decode-no-linebreak") "test-base64-no-linebreak.txt")
                             (t (error "Unknown benchmark type ~A" benchmark-type))))
           (output-file (if *write-output-files*
                            (format nil "/tmp/test-~A-~A"
                                    (module-name module)
                                    (cond ((benchmark-type-is "encode") "encoded.txt")
                                          ((benchmark-type-is "encode-no-linebreak") "encoded-no-linebreak.txt")
                                          ((benchmark-type-is "decode") "decoded.bin")
                                          ((benchmark-type-is "decode-no-linebreak") "decoded-no-linebreak.bin")
                                          (t (error "Unknown benchmark type ~A" benchmark-type))))
                            "/dev/null"))
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
  (dolist (benchmark-type *benchmark-types*)
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

(defun print-boundary (lengths &optional (stream *standard-output*))
  (dolist (column lengths)
    (write-string "+-" stream)
    (loop repeat column do (write-string "-" stream))
    (write-string "-" stream))
  (write-string "+" stream)
  (terpri))

(defun print-row (row lengths &optional (stream *standard-output*))
  (mapc (lambda (column length)
          (write-string "| " stream)
          (write-string column stream)
          (loop repeat (1+ (- length (length column))) do (write-string " " stream)))
        row
        lengths)
  (write-string "|" stream)
  (terpri))

(defun print-table (header &rest row-groups)
  (let ((lengths (apply #'mapcar
                        (lambda (&rest strings)
                          (apply #'max (mapcar #'length strings)))
                        header
                        (apply #'append row-groups))))
    (print-boundary lengths)
    (print-row header lengths)
    (print-boundary lengths)
    (dolist (rows row-groups)
      (mapc (lambda (row) (print-row row lengths)) rows)
      (print-boundary lengths))))

(defun parse-time-output (string)
  (mapcar (lambda (line)
            (string-trim " " line))
          (split-sequence #\Newline string)))

(defun real-time (time-output)
  (format nil "~,2f" (parse-number:parse-number (first (last (split-sequence #\Space (first time-output)))))))

(defun memory-usage (time-output)
  (format nil "~,2f" (/ (parse-integer (fourth time-output) :junk-allowed t) 1024 1024)))

(defun summarize (results)
  (let ((header '("benchmark" "library" "run time (s)" "max rss (MB)"))
        (row-groups nil))
    (dolist (benchmark-type *benchmark-types*)
      (push (mapcar (lambda (result)
                      (let ((time-output (parse-time-output (getf result :stderr))))
                        (list benchmark-type
                              (getf result :module)
                              (real-time time-output)
                              (memory-usage time-output))))
                    (remove benchmark-type results
                            :key (lambda (result) (getf result :benchmark-type))
                            :test #'string/=))
            row-groups))
    (cons header (remove nil row-groups))))

(defun run! (benchmark-types module-names)
  (apply #'print-table (summarize (run-benchmarks benchmark-types module-names))))

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
