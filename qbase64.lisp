;;;; qbase64.lisp

(in-package #:qbase64)

;;; constants

(declaim ((array (unsigned-byte 8)) +empty-bytes+))
(define-constant +empty-bytes+ (make-byte-vector 0))

(declaim (simple-base-string +empty-string+))
(define-constant +empty-string+ (make-string 0 :element-type 'base-char))

;;; alphabet

(declaim (simple-string +original-set+ +uri-set+))
(define-constant +original-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define-constant +uri-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(define-constant +pad-char+ #\=)
(declaim (base-char +pad-char+))

(deftype scheme ()
  '(member :original :uri))

;;; encode

(declaim (ftype (function (positive-fixnum t) positive-fixnum) encode-length))
(defun encode-length (length encode-trailing-bytes)
  (declare (type positive-fixnum length))
  (declare (optimize speed))
  (* 4 (if encode-trailing-bytes
           (ceiling length 3)
           (floor length 3))))

(declaim (ftype (function (positive-fixnum) base-char) encode-byte/original))
(defun encode-byte/original (byte)
  (declare (optimize speed))
  (char +original-set+ (logand #o77 byte)))

(declaim (ftype (function (positive-fixnum) base-char) encode-byte/uri))
(defun encode-byte/uri (byte)
  (declare (optimize speed))
  (char +uri-set+ (logand #o77 byte)))

(defun/td %encode-bytes (bytes string &key
                               (scheme :original)
                               (encode-trailing-bytes t)
                               (start1 0)
                               (end1 (length bytes))
                               (start2 0)
                               (end2 (length string)))
    (((bytes (simple-array (unsigned-byte 8))) (string simple-string))
     ((bytes (simple-array (unsigned-byte 8))) (string string))
     ((bytes (array (unsigned-byte 8)))        (string string))
     ((bytes (array (unsigned-byte 8)))        (string simple-string))
     ((bytes array)                            (string string)))
  (declare (type scheme scheme))
  (declare (type positive-fixnum start1 end1 start2 end2))
  (declare (optimize speed))
  (loop
     with conv = (ecase scheme
                   (:original #'encode-byte/original)
                   (:uri #'encode-byte/uri))
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
     for first of-type (unsigned-byte 8) = (aref bytes i1)
     for second of-type (unsigned-byte 8) = (if last-two-missing 0 (aref bytes (+ i1 1)))
     for third of-type (unsigned-byte 8) =  (if last-one-missing 0 (aref bytes (+ i1 2)))
     do (setf (char string i2)        (funcall conv (ash first -2))
              (char string (+ i2 1))  (funcall conv
                                               (logior (ash first 4) (ash second -4)))
              (char string (+ i2 2))  (if last-two-missing
                                          +pad-char+
                                          (funcall conv
                                                   (logior (ash second 2) (ash third -6))))
              (char string (+ i2 3)) (if last-one-missing
                                         +pad-char+
                                         (funcall conv third)))
     finally (return (values (the positive-fixnum (min (+ start1 (* n 3)) end1))
                             (the positive-fixnum (+ start2 (* n 4)))))))
(defstruct (encoder
             (:constructor %make-encoder))
  (scheme :original :type scheme)
  (pbytes +empty-bytes+ :type (simple-array (unsigned-byte 8)))
  (pbytes-end 0 :type positive-fixnum)
  finish-p)

(defun make-encoder (&key (scheme :original))
  (%make-encoder :scheme scheme))

(defun encode (encoder bytes string &key
                                      (start1 0)
                                      (end1 (length bytes))
                                      (start2 0)
                                      (end2 (length string))
                                      finish)
  "Returns POSITION, PENDING-P.

POSITION: First index of STRING that wasn't updated
PENDING-P: True if not all BYTES were encoded"
  (declare (type encoder encoder)
           (type array bytes)
           (type string string)
           (type positive-fixnum start1 end1 start2 end2))
  (bind:bind (((:slots scheme pbytes pbytes-end finish-p) encoder)
              ((:symbol-macrolet len1) (- end1 start1)))
    (when (and (plusp len1) finish-p)
      (error "New BYTES can't be passed when :FINISH was previously true"))

    ;; Check and encode any leftover previous bytes (PBYTES)
    (when (plusp (length pbytes))
      ;; Ensure that PBYTES length is a multiple of 3 by copying from BYTES
      (let* ((last-group-fill-length (rem (- 3 (rem pbytes-end 3)) 3))
             (bytes-to-copy (min len1 last-group-fill-length)))
        (replace pbytes bytes
                 :start1 pbytes-end
                 :end1 (incf pbytes-end bytes-to-copy)
                 :start2 0
                 :end2 bytes-to-copy)
        (incf start1 bytes-to-copy))
      ;; Then encode PBYTES
      (multiple-value-bind (pos1 pos2)
          (%encode-bytes pbytes string
                         :scheme scheme
                         :start1 0
                         :end1 pbytes-end
                         :start2 start2
                         :end2 end2
                         :encode-trailing-bytes (and (zerop len1) finish))
        (setf start2 pos2)
        ;; If we can't encode all PBYTES, copy everything from BYTES
        ;; and finish now
        (when (< pos1 pbytes-end)
          (let* ((new-pbytes-length (+ (- pbytes-end pos1) len1))
                 (new-pbytes (make-array (* 3 (ceiling new-pbytes-length 3))
                                         :element-type '(unsigned-byte 8))))
            (replace new-pbytes pbytes
                     :start2 pos1
                     :end2 pbytes-end)
            (replace new-pbytes bytes
                     :start1 (- pbytes-end pos1)
                     :start2 start1
                     :end2 end1)
            (setf pbytes new-pbytes
                  pbytes-end new-pbytes-length
                  finish-p finish)
            (return-from encode (values pos2 t))))))

    ;; Encode BYTES now
    (multiple-value-bind (pos1 pos2)
        (%encode-bytes bytes string
                       :scheme scheme
                       :start1 start1
                       :end1 end1
                       :start2 start2
                       :end2 end2
                       :encode-trailing-bytes finish)
      ;; If we can't encode all BYTES, copy the remaining to PBYTES
      (when (< pos1 end1)
        (let* ((new-pbytes-length (- end1 pos1))
               (new-pbytes (make-array (* 3 (ceiling new-pbytes-length 3))
                                       :element-type '(unsigned-byte 8))))
          (replace new-pbytes bytes
                   :start2 pos1
                   :end2 end1)
          (setf pbytes new-pbytes
                pbytes-end new-pbytes-length
                finish-p finish)
          (return-from encode (values pos2 t))))

      ;; All bytes encoded
      (setf pbytes +empty-bytes+
            pbytes-end 0
            finish-p nil)
      (return-from encode (values pos2 nil)))))

;;; stream mixin

(defclass stream-mixin ()
  ((openp :accessor stream-open-p :initform t)))

(defmethod open-stream-p ((stream stream-mixin))
  (stream-open-p stream))

(defmethod close ((stream stream-mixin) &key abort)
  (declare (ignore abort))
  (setf (stream-open-p stream) nil))

(defmethod input-stream-p ((stream stream-mixin))
  nil)

(defmethod output-stream-p ((stream stream-mixin))
  nil)

;;; output stream

(defclass encode-stream (stream-mixin fundamental-binary-output-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   encoder
   (string :initform nil)
   (single-byte-vector :initform (make-byte-vector 1))))

(defmethod initialize-instance :after ((stream encode-stream) &key (scheme :original))
  (with-slots (encoder)
      stream
    (setf encoder (make-encoder :scheme scheme))))

(defmethod output-stream-p ((stream encode-stream))
  t)

(defmethod stream-element-type ((stream encode-stream))
  '(unsigned-byte 8))

(defun %stream-write-sequence (stream sequence start end finish)
  (when (null end)
    (setf end (length sequence)))
  (bind:bind (((:slots encoder string underlying-stream)
               stream)
              ((:slots pbytes-end) encoder)
              (length (encode-length (+ pbytes-end (- end start)) finish)))
    (when (or (null string)
              (< (length string) length))
      (setf string (make-string length :element-type 'base-char)))
    ;; TODO: what happens when STRING size is fixed
    (multiple-value-bind (pos2 pendingp)
        (encode encoder sequence string :start1 start :end1 end :finish finish)
      (declare (ignore pendingp))
      (write-string string underlying-stream :end pos2))
    sequence))

(defmethod stream-write-sequence ((stream encode-stream) sequence start end &key)
  (%stream-write-sequence stream sequence start end nil))

(defmethod stream-write-byte ((stream encode-stream) integer)
  (with-slots (single-byte-vector)
      stream
    (setf (aref single-byte-vector 0) integer)
    (%stream-write-sequence stream single-byte-vector 0 1 nil)
    integer))

(defun flush-pending-bytes (stream)
  (%stream-write-sequence stream +empty-bytes+ 0 0 t))

(defmethod stream-force-output ((stream encode-stream))
  (flush-pending-bytes stream)
  (force-output (slot-value stream 'underlying-stream)))

(defmethod stream-finish-output ((stream encode-stream))
  (flush-pending-bytes stream)
  (finish-output (slot-value stream 'underlying-stream)))

(defmethod close :before ((stream encode-stream) &key abort)
  (declare (ignore abort))
  (flush-pending-bytes stream))

(defun encode-bytes (bytes &key (scheme :original))
  (with-output-to-string (str)
    (with-open-stream (out (make-instance 'encode-stream
                                          :scheme scheme
                                          :underlying-stream str))
      (write-sequence bytes out))))

;;; decode

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reverse-set (set)
    (let ((array (make-array 128
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (loop
         for i upfrom 0
         for char across set
         do (setf (aref array (char-code char)) i))
      array)))

(define-constant +original-reverse-set+
    (reverse-set +original-set+))

(define-constant +uri-reverse-set+
    (reverse-set +uri-set+))

(defun decode-char/original (char)
  (aref +original-reverse-set+ (char-code char)))

(defun decode-char/uri (char)
  (aref +uri-reverse-set+ (char-code char)))

(defun decode-length (length)
  (* 3 (ceiling length 4)))

(defun/td %decode-string (string bytes &key
                                 (scheme :original)
                                 (start1 0)
                                 (end1 (length string))
                                 (start2 0)
                                 (end2 (length bytes)))
    (((string simple-string) (bytes (simple-array (unsigned-byte 8))))
     ((string string)        (bytes (simple-array (unsigned-byte 8))))
     ((string simple-string) (bytes (array (unsigned-byte 8))))
     ((string string)        (bytes (array (unsigned-byte 8))))
     ((string string)        (bytes array)))
  (declare (type scheme scheme)
           (type positive-fixnum start1 end1 start2 end2))
  (declare (optimize speed))
  (let* ((conv (ecase scheme
                 (:original #'decode-char/original)
                 (:uri #'decode-char/uri)))
         (length1 (- end1 start1))
         (length2 (- end2 start2))
         (count (min (floor length1 4) (floor length2 3))))
    (declare (type positive-fixnum length1 length2 count))
    (when (zerop count)
      (return-from %decode-string (values start1 start2)))
    (loop
       for n of-type positive-fixnum below count
       for i1 of-type positive-fixnum from start1 by 4
       for i2 of-type positive-fixnum from start2 by 3
       for first-byte of-type (unsigned-byte 8) = (funcall conv (char string i1))
       for second-byte of-type (unsigned-byte 8) = (funcall conv (char string (+ i1 1)))
       for third-byte of-type (unsigned-byte 8) = (funcall conv (char string (+ i1 2)))
       for fourth-byte of-type (unsigned-byte 8) = (funcall conv (char string (+ i1 3)))
       do (setf (aref bytes i2)       (logand #xff (logior (ash first-byte 2) (ash second-byte -4)))
                (aref bytes (+ i2 1)) (logand #xff (logior (ash second-byte 4) (ash third-byte -2)))
                (aref bytes (+ i2 2)) (logand #xff (logior (ash third-byte 6) fourth-byte)))
       finally (return (values (the positive-fixnum (+ start1 (* n 4)))
                               (the positive-fixnum
                                    (+ start2 (- (* n 3)
                                                 (if (eql +pad-char+ (char string (+ i1 2))) 1 0)
                                                 (if (eql +pad-char+ (char string (+ i1 3))) 1 0)))))))))

(defstruct (decoder
             (:constructor %make-decoder))
  scheme
  (pchars (make-string 0 :element-type 'base-char) :type (simple-array base-char))
  (pchars-end 0))

(defun make-decoder (&key (scheme :original))
  (%make-decoder :scheme scheme))

(defun decode (decoder string bytes &key
                                      (start1 0)
                                      (end1 (length string))
                                      (start2 0)
                                      (end2 (length bytes)))
  (declare (type decoder decoder)
           (type string string)
           (type array bytes)
           (type positive-fixnum start1 end1 start2 end2))
  (bind:bind (((:slots scheme pchars pchars-end) decoder)
              ((:symbol-macrolet len1) (- end1 start1)))
    (declare (type simple-string pchars))
    (declare (type positive-fixnum pchars-end))
    ;; decode PCHARS first
    (when (plusp pchars-end)
      (let ((bytes-to-copy (min (rem (- 4 (rem pchars-end 4)) 4)
                                len1)))
        (replace pchars string
                 :start1 pchars-end
                 :end1 (incf pchars-end bytes-to-copy)
                 :start2 0
                 :end2 bytes-to-copy)
        (incf start1 bytes-to-copy)
        (multiple-value-bind (pos1 pos2)
            (%decode-string pchars bytes
                            :scheme scheme
                            :start1 0
                            :end1 pchars-end
                            :start2 start2
                            :end2 end2)
          (setf start2 pos2)
          (when (< pos1 pchars-end)
            (let* ((new-pchars-length (+ (- pchars-end pos1) len1))
                   (new-pchars (if (<= new-pchars-length (length pchars))
                                   pchars
                                   (make-string (least-multiple-upfrom 4 new-pchars-length)
                                                :element-type 'base-char))))
              (declare (type positive-fixnum new-pchars-length))
              (replace new-pchars pchars
                       :start2 pos1
                       :end2 pchars-end)
              (replace new-pchars string
                       :start1 (- pchars-end pos1)
                       :start2 start1
                       :end2 end1)
              (setf pchars new-pchars
                    pchars-end new-pchars-length)
              (return-from decode (values pos2 t)))))))

    ;; If STRING is not given
    (when (zerop len1)
      (setf pchars +empty-string+
            pchars-end 0)
      (return-from decode (values start2 nil)))

    ;; Decode STRING now
    (multiple-value-bind (pos1 pos2)
        (%decode-string string bytes
                        :scheme scheme
                        :start1 start1
                        :end1 end1
                        :start2 start2
                        :end2 end2)
      (when (< pos1 end1)
        (let* ((new-pchars-length (- end1 pos1))
               (new-pchars (if (<= new-pchars-length (length pchars))
                               pchars
                               (make-string (least-multiple-upfrom 4 new-pchars-length)
                                            :element-type 'base-char))))
          (declare (type positive-fixnum new-pchars-length))
          (replace new-pchars string
                   :start2 pos1
                   :end2 end1)
          (setf pchars new-pchars
                pchars-end new-pchars-length)
          (return-from decode (values pos2 t))))

      ;; All chars encoded
      (setf pchars +empty-string+
            pchars-end 0)
      (return-from decode (values pos2 nil)))))

;;; input stream

(defclass decode-stream (stream-mixin fundamental-binary-input-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   decoder
   (string :initform +empty-string+)
   (buffer :initform +empty-bytes+)
   (buffer-end :initform 0)
   (single-byte-vector :initform (make-byte-vector 1))))

(defmethod initialize-instance :after ((stream decode-stream) &key (scheme :original))
  (with-slots (decoder)
      stream
    (setf decoder (make-decoder :scheme scheme))))

(defmethod input-stream-p ((stream decode-stream))
  t)

(defmethod stream-element-type ((stream decode-stream))
  '(unsigned-byte 8))

(defmethod stream-read-sequence ((stream decode-stream) sequence start end &key)
  (when (null end)
    (setf end (length sequence)))
  (bind:bind (((:slots decoder string underlying-stream buffer buffer-end) stream)
              ((:slots pchars-end) decoder)
              ((:symbol-macrolet length) (- end start))
              (string-end (- (encode-length (max 0 (- length buffer-end)) t) pchars-end)))
    (when (plusp buffer-end)
      (let ((bytes-copied (min length buffer-end)))
        (replace sequence buffer
                 :start1 start :end1 end
                 :start2 0 :end2 buffer-end)
        (replace buffer buffer
                 :start2 bytes-copied
                 :end2 buffer-end)
        (decf buffer-end bytes-copied)
        (incf start bytes-copied)))
    (when (< (length string) string-end)
      (setf string (make-string string-end :element-type 'base-char)))
    (bind:bind ((end1 (read-sequence string underlying-stream :end string-end))
                ((:values pos2 pendingp)
                 (decode decoder string sequence
                         :end1 end1
                         :start2 start
                         :end2 end)))
      (when (and (< pos2 end) pendingp)
        (bind:bind ((remaining-length (- end pos2))
                    (buffer-length (least-multiple-upfrom 3 remaining-length))
                    (new-buffer (if (> buffer-length (length buffer))
                                    (make-byte-vector buffer-length)
                                    buffer))
                    (buffer-pos (decode decoder +empty-string+ new-buffer :end2 buffer-length))
                    (bytes-copied (min remaining-length buffer-pos)))
          (replace sequence new-buffer
                   :start1 pos2 :end1 end
                   :start2 0 :end2 buffer-pos)
          (replace new-buffer new-buffer
                   :start2 bytes-copied
                   :end2 buffer-pos)
          (setf buffer new-buffer
                buffer-end (- buffer-pos bytes-copied))
          (incf pos2 bytes-copied)))
      pos2)))

(defmethod stream-read-byte ((stream decode-stream))
  (with-slots (single-byte-vector)
      stream
    (let ((pos (stream-read-sequence stream single-byte-vector 0 1)))
      (if (zerop pos)
          :eof
          (aref single-byte-vector 0)))))

(defun decode-string (string &key (scheme :original))
  (with-input-from-string (str-in string)
    (with-open-stream (in (make-instance 'decode-stream
                                         :underlying-stream str-in
                                         :scheme scheme))
      (let ((bytes (make-byte-vector (decode-length (length string)))))
        (make-array (read-sequence bytes in)
                    :element-type '(unsigned-byte 8)
                    :displaced-to bytes)))))
