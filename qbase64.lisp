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

(defun octets (&rest contents)
  (make-array (length contents)
              :element-type '(unsigned-byte 8)
              :initial-contents contents))

(defun make-octet-vector (size)
  (make-array size :element-type '(unsigned-byte 8)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (bind::defbinding-form (:symbol-macrolet :use-values-p nil)
    `(symbol-macrolet ((,(first bind::variables) ,bind::values)))))

;;; alphabet

(define-constant +original-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define-constant +uri-set+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(declaim (simple-string +original-set+ +uri-set+))

(define-constant +pad-char+ #\=)
(declaim (base-char +pad-char+))

;;; encode

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

(defgeneric %octets-to-base64 (octets string &key
                                               scheme
                                               encode-trailing-bytes
                                               start1
                                               end1
                                               start2
                                               end2))

(defmacro def-%octets-to-base64 (array-type)
  ` (defmethod %octets-to-base64 ((octets ,array-type) string &key
                                                                (scheme :original)
                                                                (encode-trailing-bytes t)
                                                                (start1 0)
                                                                (end1 (length octets))
                                                                (start2 0)
                                                                (end2 (length string)))
      (declare ((,array-type (unsigned-byte 8)) octets))
      (declare (simple-string string))
      (declare (base64-scheme scheme))
      (declare (positive-fixnum start1 end1 start2 end2))
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
                                              +pad-char+
                                              (funcall conv
                                                       (logior (ash second 2) (ash third -6))))
                  (char string (+ i2 3)) (if last-one-missing
                                             +pad-char+
                                             (funcall conv third)))
         finally (return (values (the positive-fixnum (min (+ start1 (* n 3)) end1))
                                 (the positive-fixnum (+ start2 (* n 4))))))))

(def-%octets-to-base64 simple-array)
(def-%octets-to-base64 array)

(defstruct (encoder
             (:constructor %make-encoder))
  scheme
  pbytes
  (pbytes-end 0)
  finish-p)

(defun make-encoder (&key (scheme :original))
  (%make-encoder :scheme scheme))

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
             (bytes-to-copy (min len1 last-group-fill-length)))
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

;;; output stream

(defclass base64-output-stream (fundamental-binary-output-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   encoder
   (string :initform nil)))

(defmethod initialize-instance :after ((stream base64-output-stream) &key (scheme :original))
  (with-slots (encoder)
      stream
    (setf encoder (make-encoder :scheme scheme))))

(defmethod stream-element-type ((stream base64-output-stream))
  '(unsigned-byte 8))

(defun %stream-write-sequence (stream sequence start end finish)
  (when (null end)
    (setf end (length sequence)))
  (bind:bind (((:slots encoder string underlying-stream)
               stream)
              ((:slots pbytes-end) encoder)
              (length (base64-length (+ pbytes-end (- end start)) finish)))
    (when (or (null string)
              (< (length string) length))
      (setf string (make-string length :element-type 'base-char)))
    ;; TODO: what happens when STRING size is fixed
    (multiple-value-bind (pos2 pendingp)
        (encode encoder sequence string :start1 start :end1 end :finish finish)
      (declare (ignore pendingp))
      (write-string string underlying-stream :end pos2))
    sequence))

(defmethod stream-write-sequence ((stream base64-output-stream) sequence start end &key)
  (%stream-write-sequence stream sequence start end nil))

(define-constant +empty-octets+ (make-array 0 :element-type '(unsigned-byte 8)))

(defun flush-pending-bytes (stream)
  (%stream-write-sequence stream +empty-octets+ 0 0 t))

(defmethod stream-force-output ((stream base64-output-stream))
  (flush-pending-bytes stream)
  (force-output (slot-value stream 'underlying-stream)))

(defmethod stream-finish-output ((stream base64-output-stream))
  (flush-pending-bytes stream)
  (finish-output (slot-value stream 'underlying-stream)))

(defmethod close ((stream base64-output-stream) &key abort)
  (declare (ignore abort))
  (flush-pending-bytes stream)
  #-cmucl (call-next-method))

;;; octets-to-base64

(defun octets-to-base64 (octets &key (scheme :original))
  (with-output-to-string (str)
    (with-open-stream (out (make-instance 'base64-output-stream
                                          :scheme scheme
                                          :underlying-stream str))
      (write-sequence octets out))))

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

(defun base64-to-byte-original (char)
  (aref +original-reverse-set+ (char-code char)))

(defun base64-to-byte-uri (char)
  (aref +uri-reverse-set+ (char-code char)))

(defun octets-length (length)
  (* 3 (ceiling length 4)))

(defun %base64-to-octets (string octets &key
                                          (scheme :original)
                                          (start1 0)
                                          (end1 (length string))
                                          (start2 0)
                                          (end2 (length octets)))
  (let* ((conv (ecase scheme
                 (:original #'base64-to-byte-original)
                 (:uri #'base64-to-byte-uri)))
         (length1 (- end1 start1))
         (length2 (- end2 start2))
         (count (min (floor length1 4) (floor length2 3))))
    (declare (positive-fixnum length1 length2 count))
    (when (zerop count)
      (return-from %base64-to-octets (values start1 start2)))
    (loop
       for n of-type positive-fixnum below count
       for i1 of-type positive-fixnum from start1 by 4
       for i2 of-type positive-fixnum from start2 by 3
       for first-byte = (funcall conv (char string i1))
       for second-byte = (funcall conv (char string (+ i1 1)))
       for third-byte = (funcall conv (char string (+ i1 2)))
       for fourth-byte = (funcall conv (char string (+ i1 3)))
       do (setf (aref octets i2)       (logand #xff (logior (ash first-byte 2) (ash second-byte -4)))
                (aref octets (+ i2 1)) (logand #xff (logior (ash second-byte 4) (ash third-byte -2)))
                (aref octets (+ i2 2)) (logand #xff (logior (ash third-byte 6) fourth-byte)))
       finally (return (values (+ start1 (* n 4))
                               (+ start2 (- (* n 3)
                                            (if (eql +pad-char+ (char string (+ i1 2))) 1 0)
                                            (if (eql +pad-char+ (char string (+ i1 3))) 1 0))))))))

(defstruct (decoder
             (:constructor %make-decoder))
  scheme
  pchars
  (pchars-end 0))

(defun make-decoder (&key (scheme :original))
  (%make-decoder :scheme scheme))

(defun decode (decoder string octets &key
                                       (start1 0)
                                       (end1 (length string))
                                       (start2 0)
                                       (end2 (length octets)))
  (bind:bind (((:slots scheme pchars pchars-end) decoder)
              ((:symbol-macrolet len1) (- end1 start1)))
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
            (%base64-to-octets pchars octets
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
                                   (make-string (* 4 (ceiling new-pchars-length 4))
                                                :element-type 'base-char))))
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
      (setf pchars nil
            pchars-end 0)
      (return-from decode (values start2 nil)))

    ;; Encode STRING now
    (multiple-value-bind (pos1 pos2)
        (%base64-to-octets string octets
                           :scheme scheme
                           :start1 start1
                           :end1 end1
                           :start2 start2
                           :end2 end2)
      (when (< pos1 end1)
        (let* ((new-pchars-length (- end1 pos1))
               (new-pchars (if (<= new-pchars-length (length pchars))
                               pchars
                               (make-string (* 4 (ceiling new-pchars-length 4))
                                            :element-type 'base-char))))
          (replace new-pchars string
                   :start2 pos1
                   :end2 end1)
          (setf pchars new-pchars
                pchars-end new-pchars-length)
          (return-from decode (values pos2 t))))

      ;; All chars encoded
      (setf pchars nil
            pchars-end 0)
      (return-from decode (values pos2 nil)))))

;;; input stream

(defclass base64-input-stream (fundamental-binary-input-stream trivial-gray-stream-mixin)
  ((underlying-stream :initarg :underlying-stream)
   decoder
   (string :initform nil)
   (buffer :initform nil)
   (buffer-end :initform 0)))

(defmethod initialize-instance :after ((stream base64-input-stream) &key (scheme :original))
  (with-slots (decoder)
      stream
    (setf decoder (make-decoder :scheme scheme))))

(defmethod stream-element-type ((stream base64-output-stream))
  '(unsigned-byte 8))

(defun least-multiple-upfrom (n m)
  (* n (ceiling m n)))

(defmethod stream-read-sequence ((stream base64-input-stream) sequence start end &key)
  (when (null end)
    (setf end (length sequence)))
  (bind:bind (((:slots decoder string underlying-stream buffer buffer-end) stream)
              ((:slots pchars-end) decoder)
              ((:symbol-macrolet length) (- end start))
              ((:symbol-macrolet string-length) (base64-length length t)))
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
    (when (< (length string) string-length)
      (setf string (make-string string-length :element-type 'base-char)))
    (bind:bind ((end1 (read-sequence string underlying-stream))
                ((:values pos2 pendingp)
                 (decode decoder string sequence
                         :end1 end1
                         :start2 start
                         :end2 end)))
      (when (and (< pos2 end) pendingp)
        (bind:bind ((remaining-length (- end pos2))
                    (buffer-length (least-multiple-upfrom 3 remaining-length))
                    (new-buffer (if (> buffer-length (length buffer))
                                    (make-octet-vector buffer-length)
                                    buffer))
                    (buffer-pos (decode decoder nil new-buffer :end2 buffer-length))
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
