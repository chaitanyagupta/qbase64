# qbase64

qbase64 provides a fast and flexible base64 encoder and decoder for
Common Lisp. It provides three interfaces for both encoding and
decoding:

* `ENCODE-BYTES` and `DECODE-STRING` are the easiest to use. They
  allow one to encode a byte array and decode a base64 string in one
  go.

* `ENCODE-STREAM` and `DECODE-STREAM` are gray stream classes that
  allow one to write and read bytes from underlying character streams.

* `ENCODER` and `DECODER` provide the low level interface. The other
  interfaces are built on top of these.

## Table of Contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Usage](#usage)
  - [Encoding](#encoding)
  - [Decoding](#decoding)
  - [Advanced](#advanced)
- [Limits](#limits)
- [Performance](#performance)
- [Additional Features](#additional-features)
  - [Encoding Schemes](#encoding-schemes)
  - [Linebreaks](#linebreaks)
- [API Reference](#api-reference)
- [Reporting Bugs](#reporting-bugs)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Usage

### Encoding

The examples below use `ENCODE-BYTES` and `ENCODE-STREAM`.

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :qbase64))

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

### Decoding

The examples below use `DECODE-STRING` and `DECODE-STREAM`.

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :qbase64))

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

### Advanced

Normally you wouldn't need to use `ENCODER` and `DECODER` directly,
but if you do (say you want more control over memory management), you
can refer to the examples below.

In these examples, fixed length sequences are used for both input and
output, and any input buffered by the encoder/decoder is first cleared
before further input is fed to it. This allows very tight control over
how much memory gets used.

Refer to the doc strings for `ENCODER`, `ENCODE`, `DECODER` and
`DECODE` for more details.

Note that running the following examples requires
[FLEXI-STREAMS](http://weitz.de/flexi-streams/).

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :qbase64)
  (asdf:load-system :flexi-streams))

;;; ENCODER
(flexi-streams:with-input-from-sequence (in #(1 2 3 4 5 6 7 8))
  (let* ((encoder (qbase64:make-encoder))
         (bytes (make-array 4))
         (string (make-string 5))
         (read-bytes t)
         (buffered nil)
         (eof nil))
    (loop
       while (or buffered (not eof))
       for end1 = (when read-bytes (read-sequence bytes in))
       if (and read-bytes (< end1 (length bytes))) do (setf eof t)
       do
         (multiple-value-bind (end2 pending)
             (if read-bytes
                 (qbase64:encode encoder bytes string :end1 end1 :finish eof)
                 (qbase64:encode encoder #() string :finish eof))
           (write-string string nil :end end2)
           (setf buffered pending
                 read-bytes (or (not pending) (zerop end2)))))))
; prints =>
; AQIDBAUGBwg=

;;; DECODER
(with-input-from-string (in "AQIDBAUGBwg=")
  (let* ((decoder (qbase64:make-decoder))
         (string (make-string 4))
         (bytes (make-array 5))
         (read-string t)
         (buffered nil)
         (eof nil))
    (loop
       while (or buffered (not eof))
       for end1 = (when read-string (read-sequence string in))
       if (and read-string (< end1 (length string))) do (setf eof t)
       do
         (multiple-value-bind (end2 pending)
             (if read-string
                 (qbase64:decode decoder string bytes :end1 end1)
                 (qbase64:decode decoder "" bytes))
           (print (subseq bytes 0 end2))
           (setf buffered pending
                 read-string (or (not pending) (zerop end2)))))))
; prints =>
; #(1 2 3)
; #(4 5 6)
; #(7 8)
; #()
```

## Limits

The library relies on fixnum arithmetic to achieve good
performance. Consequently, it asserts the following:

* Max length of the byte array that is used as encoding input or
  decoding output should never exceed `+MAX-BYTES-LENGTH+`.

* Max length of the string that is used as encoding output or decoding
  input should never exceed `+MAX-STRING-LENGTH+`.

## Performance

Encoding and decoding should be very fast under these conditions:

* The byte array is a `SIMPLE-ARRAY` of element type `(UNSIGNED-BYTE 8)`.

* The string is a `SIMPLE-STRING`. Theoretically `SIMPLE-BASE-STRING`
  should be even faster.

## Additional Features

### Encoding Schemes

Two base64 encoding schemes are supported: original (the default) and
URI.

URI encoding scheme is useful when base64 strings are used as GET or
POST values in an HTTP request.

The scheme can be set by using the `:SCHEME` keyword.

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

## Reporting Bugs

To report a bug in the library, create a [Github
issue](https://github.com/chaitanyagupta/qbase64/issues).
