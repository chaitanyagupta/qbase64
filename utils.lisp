;;;; qbase64.lisp

(in-package #:qbase64)

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

(bind::defbinding-form (:symbol-macrolet :use-values-p nil)
  `(symbol-macrolet ((,(first bind::variables) ,bind::values))))

(declaim (ftype (function (positive-fixnum positive-fixnum) positive-fixnum) least-multiple-upfrom))
(defun least-multiple-upfrom (multiple-of upfrom)
  (* multiple-of (ceiling upfrom multiple-of)))

;; Copied from ALEXANDRIA
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defmacro defun/td (&whole whole name lambda-list type-decls-list &body body)
  (multiple-value-bind (body decls doc-string)
      (parse-body body :documentation t :whole whole)
    `(defun ,name ,lambda-list
       ,@(when doc-string (list doc-string))
       (cond
         ,@(mapcar (lambda (type-decls)
                     `((and ,@(loop for (name type) in type-decls
                                 collect `(typep ,name ',type)))
                       (locally
                           (declare ,@(loop for (name type) in type-decls
                                         collect `(type ,type ,name)))
                         ,@decls
                         ,@body)))
                   type-decls-list)
         (t (error "Arguments don't satisfy any of these type declarations: ~A" ',type-decls-list))))))
