(in-package #:cl-user)

(defvar *decode-output-element-type* '(unsigned-byte 8))

(defun resume ()
  (with-simple-restart (abort "Exit benchmark")
    (destructuring-bind (benchmark-type input-file output-file)
        (rest sb-ext:*posix-argv*)
      (cond
        ((member benchmark-type '("encode" "encode-no-linebreak") :test #'string=)
         (with-open-file (in input-file :element-type '(unsigned-byte 8))
           (with-open-file (out output-file
                                :direction :output
                                :element-type 'character
                                :if-exists :supersede)
             (encode in out benchmark-type))))
        ((member benchmark-type '("decode" "decode-no-linebreak") :test #'string=)
         (with-open-file (in input-file :element-type 'character)
           (with-open-file (out output-file
                                :direction :output
                                :element-type *decode-output-element-type*
                                :if-exists :supersede)
             (decode in out benchmark-type))))))))

(defun save-image (name)
  (sb-ext:save-lisp-and-die (format nil "~A.core" name)
                            :toplevel 'resume
                            :purify t))
