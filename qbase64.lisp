;;;; qbase64.lisp

(in-package #:qbase64)

;;; utils

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro nest (&rest r)
  (reduce (lambda (o i) `(,@o ,i)) r :from-end t))


;;; encode

(define-constant +original-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define-constant +uri-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(declaim (simple-string +original-set+ +uri-set+))

(define-constant +base64-pad+ #\=)
(declaim (base-char +base64-pad+))

(declaim (ftype (function (positive-fixnum t) positive-fixnum) base64-length))
(defun base64-length (length encode-trailing-bytes)
  (declare (positive-fixnum length))
  (declare (optimize speed))
  (* 4 (if encode-trailing-bytes
           (ceiling length 3)
           (floor length 3))))

(declaim (ftype (function (positive-fixnum simple-string) base-char) octet-to-base64))
(defun octet-to-base64 (octet set)
  (declare (optimize speed))
  (char set (logand #o77 octet)))

(deftype base64-scheme ()
  '(member :original :uri))

(declaim (ftype (function (positive-fixnum) base-char) octet-to-base64-original))
(defun octet-to-base64-original (octet)
  (declare (optimize speed))
  (char +original-set+ (logand #o77 octet)))

(declaim (ftype (function (positive-fixnum) base-char) octet-to-base64-uri))
(defun octet-to-base64-uri (octet)
  (declare (optimize speed))
  (char +uri-set+ (logand #o77 octet)))

(declaim (ftype (function ((simple-array (unsigned-byte 8))
                           simple-string
                           &key
                           (:scheme base64-scheme)
                           (:encode-trailing-bytes t)
                           (:start1 positive-fixnum)
                           (:end1 positive-fixnum)
                           (:start2 positive-fixnum)
                           (:end2 positive-fixnum))
                          (values positive-fixnum positive-fixnum))
                %octets-to-base64))
(defun %octets-to-base64 (octets string &key
                                          (scheme :original)
                                          (encode-trailing-bytes t)
                                          (start1 0)
                                          (end1 (length octets))
                                          (start2 0)
                                          (end2 (length string)))
  (declare (optimize speed))
  (loop
     with conv = (ecase scheme
                   (:original #'octet-to-base64-original)
                   (:uri #'octet-to-base64-uri))
     with length1 of-type positive-fixnum = (- end1 start1)
     with length2 of-type positive-fixnum = (- end2 start2)
     with count1 = (multiple-value-bind (count rem)
                       (floor length1 3)
                     (if encode-trailing-bytes
                         (if (plusp rem) (1+ count) count)
                         count))
     with count2 = (floor length2 4)
     for n of-type positive-fixnum below (min count1 count2)
     for i1 of-type positive-fixnum from start1 by 3
     for i2 of-type positive-fixnum from start2 by 4
     for last-two-missing = (= (- end1 i1) 1)
     for last-one-missing = (or last-two-missing (= (- end1 i1) 2))
     for first of-type (unsigned-byte 8) = (aref octets i1)
     for second of-type (unsigned-byte 8) = (if last-two-missing 0 (aref octets (+ i1 1)))
     for third of-type (unsigned-byte 8) =  (if last-one-missing 0 (aref octets (+ i1 2)))
     do (setf (char string i2)        (funcall conv (ash first -2))
              (char string (+ i2 1))  (funcall conv
                                               (logior (ash first 4) (ash second -4)))
              (char string (+ i2 2))  (if last-two-missing
                                          +base64-pad+
                                          (funcall conv
                                                   (logior (ash second 2) (ash third -6))))
              (char string (+ i2 3)) (if last-one-missing
                                         +base64-pad+
                                         (funcall conv third)))
     finally (return (values (min (+ start1 (* n 3)) end1)
                             (+ start2 (* n 4))))))

(defstruct (encoder
             (:constructor %make-encoder))
  scheme
  pad
  pbytes
  (pbytes-end 0)
  finish-p)

(defun make-encoder (&key (scheme :original) (pad t))
  (%make-encoder :scheme scheme
                 :pad pad))

(bind::defbinding-form (:symbol-macrolet :use-values-p nil)
  `(symbol-macrolet ((,(first bind::variables) ,bind::values))))

(defun encode (encoder octets string &key
                                       (start1 0)
                                       (end1 (length octets))
                                       (start2 0)
                                       (end2 (length string))
                                       finish)
  "Returns POSITION, PENDING-P.

POSITION: First index of STRING that wasn't updated
PENDING-P: True if not all OCTETS were encoded"
  (bind:bind (((:slots scheme pbytes pbytes-end finish-p) encoder)
              ((:symbol-macrolet len1) (- end1 start1)))
    (when (and (plusp len1) finish-p)
      (error "New OCTETS can't be passed when :FINISH was previously true"))

    ;; Check and encode any leftover previous bytes (PBYTES)
    (when (plusp (length pbytes))
      ;; Ensure that PBYTES length is a multiple of 3 by copying from OCTETS
      (let* ((last-group-fill-length (rem (- 3 (rem pbytes-end 3)) 3))
             (bytes-to-copy (min (- end1 start1) last-group-fill-length)))
        (replace pbytes octets
                 :start1 pbytes-end
                 :end1 (incf pbytes-end bytes-to-copy)
                 :start2 0
                 :end2 bytes-to-copy)
        (incf start1 bytes-to-copy))
      ;; Then encode PBYTES
      (multiple-value-bind (pos1 pos2)
          (%octets-to-base64 pbytes string
                             :scheme scheme
                             :start1 0
                             :end1 pbytes-end
                             :start2 start2
                             :end2 end2
                             :encode-trailing-bytes (and (zerop len1) finish))
        (setf start2 pos2)
        ;; If we can't encode all PBYTES, copy everything from OCTETS
        ;; and finish now
        (when (< pos1 pbytes-end)
          (let* ((new-pbytes-length (+ (- pbytes-end pos1) len1))
                 (new-pbytes (make-array (* 3 (ceiling new-pbytes-length 3))
                                         :element-type '(unsigned-byte 8))))
            (replace new-pbytes pbytes
                     :start2 pos1
                     :end2 pbytes-end)
            (replace new-pbytes octets
                     :start1 (- pbytes-end pos1)
                     :start2 start1
                     :end2 end1)
            (setf pbytes new-pbytes
                  pbytes-end new-pbytes-length
                  finish-p finish)
            (return-from encode (values pos2 t))))))

    ;; If OCTETS are not given
    (when (null octets)
      (setf pbytes nil
            pbytes-end 0
            finish-p nil)
      (return-from encode (values start2 nil)))

    ;; Encode OCTETS now
    (multiple-value-bind (pos1 pos2)
        (%octets-to-base64 octets string
                           :scheme scheme
                           :start1 start1
                           :end1 end1
                           :start2 start2
                           :end2 end2
                           :encode-trailing-bytes finish)
      ;; If we can't encode all OCTETS, copy the remaining to PBYTES
      (when (< pos1 end1)
        (let* ((new-pbytes-length (- end1 pos1))
               (new-pbytes (make-array (* 3 (ceiling new-pbytes-length 3))
                                      :element-type '(unsigned-byte 8))))
          (replace new-pbytes octets
                   :start2 pos1
                   :end2 end1)
          (setf pbytes new-pbytes
                pbytes-end new-pbytes-length
                finish-p finish)
          (return-from encode (values pos2 t))))

      ;; All octets encoded
      (setf pbytes nil
            pbytes-end 0
            finish-p nil)
      (return-from encode (values pos2 nil)))))

;;; stream

(defclass base64-stream (fundamental-binary-stream)
  ((underlying-input-stream :initform nil :initarg :underlying-input-stream)
   (underlying-output-stream :initform nil :initarg :underlying-output-stream)
   output-encoder
   (output-string :initform nil)))

(defmethod initialize-instance :after ((stream base64-stream) &key (scheme :original))
  (with-slots (output-encoder)
      stream
    (setf output-encoder (make-encoder :scheme scheme))))

(defmethod stream-element-type ((stream base64-stream))
  '(unsigned-byte 8))

(defmethod input-stream-p ((stream base64-stream))
  (not (null (slot-value stream 'underlying-input-stream))))

(defmethod output-stream-p ((stream base64-stream))
  (not (null (slot-value stream 'underlying-output-stream))))

(defmethod stream-read-sequence ((stream base64-stream) sequence start end &key)
  
  )

(defmacro expand-stream-write-sequence ()
  `(progn
     (when (null end)
       (setf end (length sequence)))
     (bind:bind (((:slots (encoder output-encoder)
                          (string output-string)
                          (underlying-stream underlying-output-stream))
                  stream)
                 ((:slots pbytes-end) encoder)
                 (length (base64-length (+ pbytes-end (- end start)) nil)))
       (when (or (null string)
                 (< (length string) length))
         (setf string (make-string length :element-type 'base-char)))
       ;; TODO: what happens when STRING size is fixed
       (multiple-value-bind (pos2 pendingp)
           (encode encoder sequence string :start1 start :end1 end)
         (declare (ignore pendingp))
         (write-string string underlying-stream :end pos2))
       sequence)))

(defmethod stream-write-sequence ((stream base64-stream) sequence start end &key)
  (expand-stream-write-sequence))

;; SBCL expects SB-GRAY:STREAM-WRITE-SEQUENCE to be overridden
#+sbcl
(defmethod sb-gray:stream-write-sequence ((stream base64-stream) sequence &optional start end)
  (expand-stream-write-sequence))

(defun flush-pending-bytes (stream)
  (bind:bind (((:slots (encoder output-encoder)
                       (string output-string)
                       (underlying-stream underlying-output-stream))
               stream)
              ((:slots pbytes-end) encoder)
              (length (base64-length pbytes-end t)))
    (when (or (null string)
              (< (length string) length))
      (setf string (make-string length :element-type 'base-char)))
    ;; TODO: what happens when STRING size is fixed
    (multiple-value-bind (pos2 pending)
        (encode encoder nil string :finish t)
      (declare (ignore pending))
      (when (plusp pos2)
        (write-sequence string underlying-stream :end pos2)))))

(defmethod stream-force-output ((stream base64-stream))
  (flush-pending-bytes stream)
  (force-output (slot-value stream 'underlying-output-stream)))

(defmethod stream-finish-output ((stream base64-stream))
  (flush-pending-bytes stream)
  (finish-output (slot-value stream 'underlying-output-stream)))

(defmethod close ((stream base64-stream) &key abort)
  (declare (ignore abort))
  (flush-pending-bytes stream)
  (call-next-method))

;;; archived


