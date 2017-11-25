(in-package #:base64-benchmark)

(defparameter *base64-bin* (or (sb-posix:getenv "BASE64_BIN")
                               (find-executable "base64")))

(defclass base64-cli (benchmark-module)
  ())

(defmethod prepare ((module base64-cli))
  (unless *base64-bin*
    (error "Couldn't find base64 binary")))

(defmethod run ((module base64-cli) benchmark-type input-file output-file error-file)
  (run-program *base64-bin*
               (append (when (string= benchmark-type "encode")
                         (list "-b" "72"))
                       (when (member benchmark-type '("decode" "decode-no-linebreak") :test #'string=)
                         (list "-D"))
                       `("-i" ,input-file "-o" ,output-file))
               :error error-file
               :if-error-exists :supersede))

