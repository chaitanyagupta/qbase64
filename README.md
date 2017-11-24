# qbase64

qbase64 provides a fast and flexible base64 encoder and decoder for
Common Lisp. It provides three interfaces for both encoding and
decoding:

* `ENCODE-BYTES` and `DECODE-STRING` are the easiest to use. They
  allow one to encode a byte array and decode a base64 string in one
  go.

* `ENCODE-STREAM` and `DECODE-STREAM` are gray stream classes that
  allow one to use CL's binary stream functions to write and read
  bytes to/from underlying character streams.

* `ENCODER` and `DECODER` provide the low level interface. The other
  interfaces are built on top of these.

## Encoding (simple)

The encoding examples below use `ENCODE-BYTES` and `ENCODE-STREAM`.

```lisp
(asdf:load-system :qbase64)
; => T

;;; ENCODE-BYTES
(qbase64:encode-bytes #(1 2 3 4 5 6 7 8))
; => "AQIDBAUGBwg="

;;; ENCODE-STREAM
(with-output-to-string (s)
  (with-open-stream (out (make-instance 'qbase64:encode-stream
                                        :underlying-stream s))
    (write-sequence #(1 2 3 4) out)
    (write-sequence #(5 6 7 8) out)))
; => "AQIDBAUGBwg="
```

## Decoding (simple)

The decoding examples below use `DECODE-STRING` and `DECODE-STREAM`.

```lisp
(asdf:load-system :qbase64)
; => T

;;; DECODE-STRING
(qbase64:decode-string "AQIDBAUGBwg=")
; => #(1 2 3 4 5 6 7 8)

;;; DECODE-STREAM
(with-input-from-string (s "AQIDBAUGBwg=")
  (with-open-stream (in (make-instance 'qbase64:decode-stream
                                       :underlying-stream s))
    (let ((bytes (make-array 4)))
      (loop
         for position = (read-sequence bytes in)
         do (print (subseq bytes 0 position))
         while (= position (length bytes))))))
; prints =>
; #(1 2 3 4) 
; #(5 6 7 8) 
; #() 
```

## Advanced encoding and decoding

Normally you wouldn't need to use `ENCODER` and `DECODER` directly,
but if you do (perhaps you want more control over memory), you can
refer to the examples below.

```lisp
(asdf:load-system :qbase64)
; => T

;;; ENCODER
(loop
   with encoder = (qbase64:make-encoder)
   with bytes = #(1 2 3 4 5 6 7 8)
   for start = 0 then end
   for end = (min (+ start 4) (length bytes)) 
   with string = (make-string 5)
   with pending = nil
   while (or pending (< start end))
   do
     (multiple-value-bind (string-end pendingp)
         (qbase64:encode encoder bytes string
                         :start1 start :end1 end
                         :finish (= start end))
       (write-string string *standard-output* :end string-end)
       (setf pending pendingp)))
; prints =>
; AQIDBAUGBwg=

;;; DECODER
(loop
   with decoder = (qbase64:make-decoder)
   with string = "AQIDBAUGBwg="
   for start = 0 then end
   for end = (min (+ start 4) (length string))
   with bytes = (make-array 5)
   with pending = nil
   while (or pending (< start end))
   do
     (multiple-value-bind (bytes-end pendingp)
         (qbase64:decode decoder string bytes
                         :start1 start :end1 end)
       (print (subseq bytes 0 bytes-end))
       (setf pending pendingp)))
; prints =>
; #(1 2 3)
; #(4 5 6)
; #(7 8)
```

## More Features

### Encoding Schemes

Two base64 encoding schemes are supported: original (used by default)
and URI.

URI encoding scheme is useful when base64 strings are used as GET or
POST values in an HTTP request.

The scheme can be provided by the `:SCHEME` keyword.

```lisp
(qbase64:encode-bytes #(251 252 253 254 255) :scheme :original)
; => "+/z9/v8="

(qbase64:encode-bytes #(251 252 253 254 255) :scheme :uri)
; => "-_z9_v8="
```

### Linebreaks

The encoded base64 stream can broken into multiple lines using the
`:LINEBREAK` keyword. By default it is 0, which means that no
newlines are output. Setting it to a positive integer indicates the
column number at which lines should be broken.

```lisp
(princ (qbase64:encode-bytes #(1 2 3 4 5 6 7 8) :linebreak 4))
; prints =>
; AQID
; BAUG
; Bwg=
```

During decoding, all whitespace (including newlines) is ignored.

## API Reference

At the moment, API reference is available in the form of doc
strings for all the exported symbols.
