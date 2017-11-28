(in-package #:qbase64)

;;; stream mixin

(defclass stream-mixin ()
  ((openp :accessor stream-open-p :initform t)))

(defmethod open-stream-p ((stream stream-mixin))
  (stream-open-p stream))

(defmethod close ((stream stream-mixin) &key abort)
  (declare (ignore abort))
  (setf (stream-open-p stream) nil))

#-clisp
(defmethod input-stream-p ((stream stream-mixin))
  nil)

#-clisp
(defmethod output-stream-p ((stream stream-mixin))
  nil)
