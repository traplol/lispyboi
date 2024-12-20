(in-package :lispyboi)
(provide "stream")

(defgeneric output-stream-write-byte (stream byte)
  "Write a BYTE to STREAM.")

(defgeneric output-stream-write-bytes (stream bytes)
  "Write a BYTES to STREAM.")

(defgeneric output-stream-write-char (stream character)
  "Write a CHARACTER to STREAM.")

(defgeneric output-stream-write-string (stream string)
  "Write a STRING to STREAM.")

(defgeneric input-stream-eof-p (stream)
  "Returns T when STREAM reaches the end, otherwise NIL.")

(defgeneric input-stream-peek-char (stream eof-error-p eof-value)
  "Returns the next character in STREAM without consuming it.")

(defgeneric input-stream-read-char (stream eof-error-p eof-value)
  "Consumes and returns the next character in STREAM.")

(export '(output-stream-write-byte
          output-stream-write-bytes
          output-stream-write-char
          output-stream-write-string

          input-stream-eof-p
          input-stream-peek-char
          input-stream-read-char))
