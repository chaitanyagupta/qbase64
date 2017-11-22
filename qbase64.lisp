;;;; qbase64.lisp

(in-package #:qbase64)

;;; constants

(declaim ((array (unsigned-byte 8)) +empty-bytes+))
(define-constant +empty-bytes+ (make-byte-vector 0))

(declaim (simple-base-string +empty-string+))
(define-constant +empty-string+ (make-string 0 :element-type 'base-char))

;;; alphabet

(declaim (simple-base-string +original-set+ +uri-set+))

(define-constant +original-set+
    (make-array (length #1="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
                :element-type 'base-char
                :initial-contents #1#))

(define-constant +uri-set+
    (make-array (length #1="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
                :element-type 'base-char
                :initial-contents #1#))

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
  (let ((set (ecase scheme
               (:original +original-set+)
               (:uri +uri-set+))))
    (declare (type simple-base-string set))
    (flet ((encode-byte (byte)
             (char set (logand #o77 byte))))
      (declare (inline encode-byte))
      (loop
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
         do (setf (char string i2)        (encode-byte (ash first -2))
                  (char string (+ i2 1))  (encode-byte
                                           (logior (ash first 4) (ash second -4)))
                  (char string (+ i2 2))  (if last-two-missing
                                              +pad-char+
                                              (encode-byte
                                               (logior (ash second 2) (ash third -6))))
                  (char string (+ i2 3)) (if last-one-missing
                                             +pad-char+
                                             (encode-byte third)))
         finally (return (values (the positive-fixnum (min (+ start1 (* n 3)) end1))
                                 (the positive-fixnum (+ start2 (* n 4)))))))))
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

;;; output stream

(defclass encode-stream (stream-mixin fundamental-binary-output-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   encoder
   (string :initform nil)
   (single-byte-vector :initform (make-byte-vector 1))
   (linebreak :initform 0 :initarg :linebreak)
   (column :initform 0)))

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
  (bind:bind (((:slots encoder string underlying-stream linebreak column)
               stream)
              ((:slots pbytes-end) encoder)
              (length (encode-length (+ pbytes-end (- end start)) finish)))
    (declare (type encoder encoder))
    (when (or (null string)
              (< (length string) length))
      (setf string (make-string length :element-type 'base-char)))
    ;; TODO: what happens when STRING size is fixed
    (multiple-value-bind (pos2 pendingp)
        (encode encoder sequence string :start1 start :end1 end :finish finish)
      (declare (ignore pendingp))
      (if (plusp linebreak)
          (loop
             for line-start = start then line-end
             for line-end = (min pos2 (+ line-start (- linebreak column)))
             do
               (write-string string underlying-stream
                             :start line-start
                             :end line-end)
               (setf column (rem (+ column (- line-end line-start)) linebreak))
               (when (and (zerop column) (> line-end line-start))
                 (write-char #\Newline underlying-stream))
             while (< line-end pos2))
          (write-string string underlying-stream :end pos2)))
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
  (with-slots (linebreak column underlying-stream)
      stream
    (%stream-write-sequence stream +empty-bytes+ 0 0 t)
    (when (and (plusp linebreak) (plusp column))
      (write-char #\Newline underlying-stream))))

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

(defun decode-length (length)
  (* 3 (ceiling length 4)))

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  "Returns T for a whitespace character."
  (declare (type character c))
  (declare (optimize speed))
  (or (char= c #\Newline) (char= c #\Space)
      (char= c #\Linefeed) (char= c #\Return)
      (char= c #\Tab)))

(defun/td %decode-string (string bytes &key
                                 (scheme :original)
                                 (start1 0)
                                 (end1 (length string))
                                 (start2 0)
                                 (end2 (length bytes)))
    (((string simple-base-string) (bytes (simple-array (unsigned-byte 8))))
     ((string simple-string)      (bytes (simple-array (unsigned-byte 8))))
     ((string string)             (bytes (simple-array (unsigned-byte 8))))
     ((string string)             (bytes (array (unsigned-byte 8))))
     ((string string)             (bytes array)))
  (declare (type scheme scheme)
           (type positive-fixnum start1 end1 start2 end2))
  (declare (optimize speed))
  (let* ((reverse-set (ecase scheme
                        (:original +original-reverse-set+)
                        (:uri +uri-reverse-set+)))
         (i1 start1)
         (i2 start2))
    (declare (type (simple-array (unsigned-byte 8)) reverse-set))
    (declare (type positive-fixnum i1 i2))
    (flet ((next-char ()
             (loop
                for char = (when (< i1 end1) (char string i1))
                do (incf i1)
                while (and char (whitespace-p char))
                finally (return char)))
           (char-to-digit (char)
             (declare (type (or null character) char))
             (if char (aref reverse-set (char-code char)) 0)))
      (declare (inline next-char char-to-digit))
      (the (values positive-fixnum positive-fixnum)
           (loop
              with padded = nil
              for i1-begin of-type positive-fixnum = i1
              for i2-begin of-type positive-fixnum = i2
              for c1 of-type (or null character) = (next-char)
              for c2 of-type (or null character) = (next-char)
              for c3 of-type (or null character) = (next-char)
              for c4 of-type (or null character) = (next-char)
              for d1 of-type (unsigned-byte 8) = (char-to-digit c1)
              for d2 of-type (unsigned-byte 8) = (char-to-digit c2)
              for d3 of-type (unsigned-byte 8) = (char-to-digit c3)
              for d4 of-type (unsigned-byte 8) = (char-to-digit c4)
              for encode-group = (and c4 (<= (+ i2 3) end2))
              if encode-group
              do (setf (aref bytes i2)       (logand #xff (logior (ash d1 2) (ash d2 -4)))
                       (aref bytes (+ i2 1)) (logand #xff (logior (ash d2 4) (ash d3 -2)))
                       (aref bytes (+ i2 2)) (logand #xff (logior (ash d3 6) d4))
                       i2 (+ i2 3)
                       padded (char= +pad-char+ c4))
              while (and encode-group (< i1 end1) (not padded))
              finally
                (return (values (if encode-group i1 i1-begin)
                                (cond ((not encode-group) i2-begin)
                                      ((char= +pad-char+ c3) (+ i2-begin 1))
                                      ((char= +pad-char+ c4) (+ i2-begin 2))
                                      (t i2)))))))))

(defstruct (decoder
             (:constructor %make-decoder))
  scheme
  (pchars (make-string 0 :element-type 'base-char) :type simple-base-string)
  (pchars-end 0))

(defun make-decoder (&key (scheme :original))
  (%make-decoder :scheme scheme))

(defun resize-pchars (pchars pchars-end new-length)
  (if (< (length pchars) new-length)
      (let ((new-pchars (make-string (least-multiple-upfrom 4 new-length)
                                     :element-type 'base-char)))
        (replace new-pchars pchars :end2 pchars-end))
      pchars))

(defun fill-pchars (decoder string &key (start 0) (end (length string)))
  (let ((pchars (decoder-pchars decoder))
        (pchars-end (decoder-pchars-end decoder)))
    (setf pchars (resize-pchars pchars pchars-end (+ pchars-end (- end start))))
    (loop
       with i = pchars-end
       with j = start
       while (and (< i (length pchars)) (< j end))
       for char = (char string j)
       if (not (whitespace-p char))
       do (setf (char pchars i) char) (incf i)
       do (incf j)
       finally
         (setf (decoder-pchars decoder) pchars
               (decoder-pchars-end decoder) i)
         (return j))))

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
      (setf start1 (fill-pchars decoder string
                                :start start1 :end end1))
      (multiple-value-bind (pos1 pos2)
          (%decode-string pchars bytes
                          :scheme scheme
                          :start1 0 :end1 pchars-end
                          :start2 start2 :end2 end2)
        (when (< pos1 pchars-end)
          ;; no more decoding can be done at this point, shift
          ;; remaining PCHARS left, slurp STRING and return
          (replace pchars pchars :start2 pos1 :end2 pchars-end)
          (decf pchars-end pos1)
          (fill-pchars decoder string :start start1 :end end1)
          (return-from decode (values pos2 t)))
        (setf pchars-end 0 start2 pos2)))

    ;; Decode STRING now
    (multiple-value-bind (pos1 pos2)
        (%decode-string string bytes
                        :scheme scheme
                        :start1 start1
                        :end1 end1
                        :start2 start2
                        :end2 end2)
      (when (< pos1 end1)
        (fill-pchars decoder string :start pos1 :end end1))
      (values pos2 (plusp pchars-end)))))

;;; input stream

(defclass decode-stream (stream-mixin fundamental-binary-input-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   decoder
   (string :initform +empty-string+)
   (buffer :initform (make-byte-vector 3))
   (buffer-end :initform 0)
   (single-byte-vector :initform (make-byte-vector 1))))

(defmethod initialize-instance :after ((stream decode-stream) &key (scheme :original))
  (with-slots (underlying-stream decoder)
      stream
    (setf decoder (make-decoder :scheme scheme))))

(defmethod input-stream-p ((stream decode-stream))
  t)

(defmethod stream-element-type ((stream decode-stream))
  '(unsigned-byte 8))

(defun write-buffer-to-sequence (stream sequence start end)
  (let ((buffer (slot-value stream 'buffer))
        (buffer-end (slot-value stream 'buffer-end)))
    (if (plusp buffer-end)
        (let ((bytes-copied (min (- end start) buffer-end)))
          (replace sequence buffer
                   :start1 start :end1 end
                   :start2 0 :end2 buffer-end)
          (replace buffer buffer
                   :start2 bytes-copied :end2 buffer-end)
          (decf (slot-value stream 'buffer-end) bytes-copied)
          (+ start bytes-copied))
        start)))

(defmethod stream-read-sequence ((stream decode-stream) sequence start end &key)
  (when (null end)
    (setf end (length sequence)))
  (bind:bind (((:slots decoder string underlying-stream buffer buffer-end) stream)
              ((:slots pchars-end) decoder)
              ((:symbol-macrolet length) (- end start)))
    (loop
       with eof = nil
       while (and (< start end) (not eof))
       do
         (setf start (write-buffer-to-sequence stream sequence start end))
       when (< start end)
       do
         (let ((string-end (encode-length length t)))
           (when (< (length string) string-end)
             (setf string (make-string string-end)))
           (bind:bind ((end1 (read-sequence string underlying-stream :end string-end))
                       ((:values pos2 pendingp)
                        (decode decoder string sequence
                                :end1 end1
                                :start2 start
                                :end2 end)))
             (setf eof (and (zerop end1) (< end1 string-end)))
             (when (and (< pos2 end) pendingp)
               (setf buffer-end (decode decoder +empty-string+ buffer
                                        :start2 buffer-end))
               (setf pos2 (write-buffer-to-sequence stream sequence pos2 end)))
             (setf start pos2)))
       finally (return start))))

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
