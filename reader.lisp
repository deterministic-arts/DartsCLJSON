#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- JSON parsing and rendering
  Copyright (c) 2014 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.JSON")


(defun read-json-token (stream)
  "Reads the next JSON token from character stream STREAM. 
Answers two values. The primary value is a keyword, which
describes the type of the token read, and the secondary value
is a Lisp object providing the actual token value. The 
following token types (and values) are reported:

  :EOF nil             the end of STREAM has been reached
  :NUMBER <rational>   a number literal has been read
  :STRING <string>     a string literal has been read
  :BEGIN-ARRAY nil     `[' has been read
  :END-ARRAY nil       `]' has been read
  :BEGIN-OBJECT nil    `{' has been read
  :END-OBJECT nil      `}' has been read
  :COMMA nil           `,' has been read
  :COLON nil           `:' has been read
  :TRUE nil            `true' has been read
  :FALSE nil           `false' has been read
  :NULL nil            `null' has been read

Numeric values are always reported as exact numbers, i.e.,
this function returns ratios and integers, never floats.

If an error is detected, this function signals a condition,
whose type is not yet fully specified..."
  (declare (optimize (debug 1) (speed 3) (safety 1)))
  (labels
      ((whitecharp (char)
         (find char #.(concatenate 'string '(#\space #\tab #\newline #\return))))
       (read-character () (read-char stream))
       (peek-character (&optional eof-ok)
         (if eof-ok
             (peek-char nil stream nil nil)
             (peek-char nil stream)))
       (eat-token (text)
         (loop
            :for expected :across text
            :for actual := (read-character)
            :unless (eql actual expected)
            :do (error "invalid character; expected ~S but got ~S" expected actual)))
       (number-char-class (char)
         (let ((value (digit-char-p char 10)))
           (cond
             (value (values :digit value))
             ((eql char #\e) (values :exponent 0))
             ((eql char #\E) (values :exponent 0))
             ((eql char #\.) (values :point 0))
             ((eql char #\-) (values :sign -1))
             ((eql char #\+) (values :sign 1))
             (t (values :invalid 0)))))
       (read-number ()
         (let ((numerator 0) (sign 1) (denominator 1) 
               (exponent 0) (esign 1) (state :before-sign))
           (declare (type (integer 0) numerator denominator exponent)
                    (type (member -1 1) sign esign))
           (loop
              :for char := (peek-character t)
              :while char
              :do (multiple-value-bind (class value) (number-char-class char)
                    (declare (type (integer -1 9) value))
                    (ecase class
                      ((:digit)
                       (case state
                         ((:before-sign :after-sign :integer)
                          (read-character)
                          (setf numerator (+ value (* 10 numerator))
                                state :integer))
                         ((:after-point :fraction)
                          (read-character)
                          (setf numerator (+ value (* 10 numerator))
                                denominator (* 10 denominator)
                                state :fraction))
                         ((:after-exp-marker :after-exp-sign :exponent)
                          (read-character)
                          (setf exponent (+ value (* 10 exponent))
                                state :exponent))
                         (otherwise (return))))
                      ((:sign)
                       (case state
                         ((:after-exp-marker) 
                          (read-character)
                          (setf esign value state :after-exp-sign))
                         ((:before-sign) 
                          (read-character)
                          (setf sign value state :after-sign))
                         (otherwise (return))))
                      ((:point)
                       (case state 
                         ((:integer) 
                          (read-character)
                          (setf state :after-point))
                         (otherwise (return))))
                      ((:exponent)
                       (case state 
                         ((:integer :fraction) 
                          (read-character)
                          (setf state :after-exp-marker))
                         (otherwise (return))))
                      ((:invalid) (return)))))
           (case state
             ((:integer :fraction :exponent) 
              (values :number
                      (* sign 
                         (* sign (/ numerator denominator)) 
                         (expt 10 (* esign exponent)))))
             (otherwise (error "unsupported number syntax (~D ~D ~D ~D ~D ~S)" numerator denominator sign esign exponent state)))))
       (read-hex-digit ()
         (let* ((char (read-character))
                (value (digit-char-p char 16)))
           (if (not value)
               (error "invalid character ~S: not a hexadecimal digit" char)
               value)))
       (read-escaped-char ()
         (let* ((d1 (read-hex-digit))
                (d2 (read-hex-digit))
                (d3 (read-hex-digit))
                (d4 (read-hex-digit)))
           (code-char (logior d4 (ash d3 4) (ash d2 8) (ash d1 12)))))
       (read-literal ()
         (let ((buffer (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)))
           (read-literal-1 buffer)))
       (read-literal-1 (buffer)
         (let ((char (read-character)))
           (cond
             ((char= char #\") (values :string (coerce buffer 'simple-string)))
             ((char= char #\\) (read-literal-2 buffer))
             (t (vector-push-extend char buffer)
                (read-literal-1 buffer)))))
       (read-literal-2 (buffer)
         (let ((char (read-char stream)))
           (cond
             ((eql char #\") (vector-push-extend #\" buffer) (read-literal-1 buffer))
             ((eql char #\n) (vector-push-extend #\newline buffer) (read-literal-1 buffer))
             ((eql char #\\) (vector-push-extend #\\ buffer) (read-literal-1 buffer))
             ((eql char #\t) (vector-push-extend #\tab buffer) (read-literal-1 buffer))
             ((eql char #\r) (vector-push-extend #\return buffer) (read-literal-1 buffer))
             ((eql char #\/) (vector-push-extend #\/ buffer) (read-literal-1 buffer))
             ((eql char #\b) (vector-push-extend #.(code-char 7) buffer) (read-literal-1 buffer))
             ((eql char #\f) (vector-push-extend #.(code-char 12) buffer) (read-literal-1 buffer))
             ((eql char #\u) (vector-push-extend (read-escaped-char) buffer) (read-literal-1 buffer))
             (t (error "invalid character ~S" char)))))
       (read-toplevel (char)
         (cond
           ((not char) (values :eof nil))
           ((whitecharp char) (read-character) (read-toplevel (peek-character t)))
           ((eql char #\{) (read-character) (values :begin-object nil))
           ((eql char #\}) (read-character) (values :end-object nil))
           ((eql char #\[) (read-character) (values :begin-array nil))
           ((eql char #\]) (read-character) (values :end-array nil))
           ((eql char #\,) (read-character) (values :comma nil))
           ((eql char #\:) (read-character) (values :colon nil))
           ((eql char #\t) (eat-token "true") (values :true nil))
           ((eql char #\f) (eat-token "false") (values :false nil))
           ((eql char #\n) (eat-token "null") (values :null nil))
           ((eql char #\") (read-character) (read-literal))
           ((eql char #\-) (read-number))
           ((find char "0123456789") (read-number))
           (t (error "invalid character ~S" char)))))
    (read-toplevel (peek-character t))))


(defun read-json-value (stream)
  "Reads the next available JSON value from character stream
STREAM and returns it. Returns nil, if the end of the stream
has been reached.

The JSON values are mapped as followes:

  true     => :TRUE
  false    => :FALSE
  null     => :NULL
  <string> => <string>
  <number> => <number>
  [<v1>, <v2>, ...]   => (:ARRAY <v1> <v2> ...)
  {<k1>: <v1>, ...}   => (:OBJECT <k1> <v1> ...)

Numbers are always exact, i.e., ratios instead of floats. The
key strings in objects are never implicitly converted to symbols,
but reported as strings instead."
  (declare (optimize (debug 1) (speed 3) (safety 1)))
  (labels
      ((next-token (&optional token value) 
         (if token 
             (values token value)
             (read-json-token stream)))
       (read-value (&key token value eof-ok)
         (multiple-value-bind (token value) (next-token token value)
           (cond
             ((eq token :eof) (if eof-ok nil (error "unexpected end of input")))
             ((eq token :true) :true)
             ((eq token :false) :false)
             ((eq token :null) :null)
             ((eq token :number) value)
             ((eq token :string) value)
             ((eq token :begin-array) (read-array))
             ((eq token :begin-object) (read-object))
             (t (error "unexpected token ~S (~S)" token value)))))
       (read-array ()
         (multiple-value-bind (token value) (next-token)
           (if (eq token :end-array)
               (list ':array)
               (let ((value (read-value :token token :value value)))
                 (read-array-1 (list value))))))
       (read-array-1 (list)
         (multiple-value-bind (token value) (next-token)
           (cond
             ((eq token :end-array) (cons ':array (nreverse list)))
             ((eq token :comma) (read-array-1 (cons (read-value) list)))
             (t (error "unexpected token ~S (~S)" token value)))))
       (read-key/value (&optional itoken ivalue)
         (multiple-value-bind (token key) (next-token itoken ivalue)
           (if (not (eq token :string))
               (error "unexpected token ~S (~S)" token key)
               (multiple-value-bind (token unused) (next-token)
                 (if (not (eq token :colon))
                     (error "unexpected token ~S (~S)" token unused)
                     (let ((value (read-value)))
                       (values key value)))))))
       (read-object ()
         (multiple-value-bind (token value) (next-token)
           (if (eq token :end-object)
               (list ':object)
               (multiple-value-bind (key value) (read-key/value token value)
                 (read-object-1 (list value key))))))
       (read-object-1 (list)
         (multiple-value-bind (token value) (next-token)
           (cond
             ((eq token :end-object) (cons ':object (nreverse list)))
             ((eq token :comma) 
              (multiple-value-bind (key value) (read-key/value)
                (read-object-1 (cons value (cons key list)))))
             (t (error "unexpected token ~S (~S)" token value))))))
    (read-value :eof-ok t)))


(defun read-json-value-from-string (string &optional (start 0) end)
  (with-input-from-string (stream string :start start :end end)
    (read-json-value stream)))

