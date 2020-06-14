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

(defun make-utf-8-decoder (callback)
  "Answers a function, which consumes bytes, decodes them as UTF-8 
   encoded characters, and passes the result on to function `callback'. 
   The function returned by `make-utf-8-decoder' is a function (here 
   referred to here as the `decoder')

     (lambda (buffer &optional (start 0) end) ...)

   which takes an array of octets as first argument, decodes those 
   under the assumption, that they represent UTF-8 encoded characters, 
   and calls the given `callback', which is expected to have the 
   signature

     (lambda (string start end) ...)

   whenever a non-empty sequence of characters becomes available. The 
   callback must not retain a reference to the `string' object supplied,
   since it may be re-used by the decoder and may become invalid right
   after the callback has been called.

   Upon EOF, the decoder function should be called with nil as first
   argument. The decoder checks, whether there are pending incomplete 
   characters and signals an error in this case. Also, the decoder will
   call the callback with nil as first argument, and 0 for the remaining 
   two arguments.

   When called, the decoder will return the number of bytes it needs to
   be fed into it, before it can decode another character and call the
   callback."
  (labels
      ((utf-8-group-size (byte)
         (cond 
           ((zerop (logand byte #b10000000)) 1)
           ((= (logand byte #b11100000) #b11000000) 2)
           ((= (logand byte #b11110000) #b11100000) 3)
           ((= (logand byte #b11111000) #b11110000) 4)
           (t (error "Invalid byte at start of character: 0x~X" byte))))
       (decode-utf-8-character (bytes group-size &optional (start 0))
         (macrolet ((next-byte ()
                      '(prog1 (elt bytes start)
                        (incf start)))
                    (six-bits (byte)
                      (let ((b (gensym)))
                        `(let ((,b ,byte))
                           (unless (= (logand ,b #b11000000) #b10000000)
                             (error "Invalid byte 0x~X inside a character." ,b))
                           (ldb (byte 6 0) ,b))))
                    (test-overlong (byte min-size)
                      (let ((b (gensym)))
                        `(let ((,b ,byte))
                           (unless (>= ,b ,min-size)
                             (error "Overlong byte sequence found."))
                           ,b))))
           (code-char 
            (ecase group-size
              (1 (next-byte))
              (2 (test-overlong (logior (ash (ldb (byte 5 0) (next-byte)) 6)
                                        (six-bits (next-byte))) 128))
              (3 (test-overlong (logior (ash (ldb (byte 4 0) (next-byte)) 12)
                                        (ash (six-bits (next-byte)) 6)
                                        (six-bits (next-byte))) 2048))
              (4 (test-overlong (logior (ash (ldb (byte 3 0) (next-byte)) 18)
                                        (ash (six-bits (next-byte)) 12)
                                        (ash (six-bits (next-byte)) 6)
                                        (six-bits (next-byte))) 65536)))))))
    (let ((buffer (make-array 128 :element-type 'character :fill-pointer 0 :adjustable t :initial-element #\space))
          (unfinished (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0 :initial-element 0))
          (missing 0))
      (lambda (sequence &optional (start 0) end)
        (if (null sequence)
            (progn
              (unless (zerop missing)
                (error "incomplete trailing characters: ~S (~D byte~:*~P missing)" unfinished missing))
              (funcall callback nil 0 0))
            (let ((end (or end (length sequence))))
              (loop
                :while (and (< start end) (plusp missing))
                :do (vector-push-extend (aref sequence start) unfinished)
                    (incf start)
                    (decf missing))
              (when (zerop missing)
                (when (plusp (length unfinished))
                  (vector-push-extend (decode-utf-8-character unfinished (length unfinished)) buffer)
                  (setf (fill-pointer unfinished) 0))
                (loop
                  :while (< start end)
                  :do (let* ((size (utf-8-group-size (aref sequence start)))
                             (new-start (+ start size)))
                        (if (<= new-start end)
                            (progn
                              (vector-push-extend (decode-utf-8-character sequence size start) buffer)
                              (setf start new-start))
                            (progn
                              (setf missing (- new-start end))
                              (loop 
                                :while (< start end)
                                :do (vector-push-extend (aref sequence start) unfinished)
                                    (incf start))))))
                (when (plusp (length buffer))
                  (funcall callback buffer 0 (length buffer))
                  (setf (fill-pointer buffer) 0)))))
        missing))))



(defun make-json-push-lexer-1 (callback)
  "Returns a function (here referred to as `lexer'), which has the
   signature

     (lambda (string &optional (start 0) end) ...)

   which groups the characters of `string' into tokens; whenever a 
   complete token has been detected, the given function `callback'
   is called. The callback must be a function

     (lambda (token value) ...)

   where `token' is a keyword specifying the type of token read,
   and `value' is the actual token value, which depends on the
   token type; for token types, which have no associated value,
   the `value' argument will be nil."
  (let (state
        (char-buffer (make-array 128 :element-type 'character :fill-pointer 0 :initial-element #\space :adjustable t))
        sign numerator denominator exponent exponent-sign num-digits)
    (labels
        ((report-token (token &optional value)
           (funcall callback token value)
           #'top-level-state)
         (report-error (control &rest arguments)
           (apply #'error control arguments))
         (top-level-state (char)
           (cond
             ((null char) (report-token :end-of-input))
             ((eql char #\space) #'top-level-state)
             ((eql char #\newline) #'top-level-state)
             ((eql char #\return) #'top-level-state)
             ((eql char #\tab) #'top-level-state)
             ((eql char #\/) #'after-slash-state)
             ((eql char #\:) (report-token :colon))
             ((eql char #\{) (report-token :open-brace))
             ((eql char #\}) (report-token :close-brace))
             ((eql char #\[) (report-token :open-bracket))
             ((eql char #\]) (report-token :close-bracket))
             ((eql char #\,) (report-token :comma))
             ((eql char #\t) (keyword-reader "true" :true))
             ((eql char #\f) (keyword-reader "false" :false))
             ((eql char #\n) (keyword-reader "null" :null))
             ((eql char #\") (setf (fill-pointer char-buffer) 0) #'read-string-top-level)
             ((eql char #\-) (initialize-number :sign -1) #'need-integral)
             (t (let ((digit (digit-char-p char 10)))
                  (if (not digit)
                      (report-error "unexpected character ~:C" char)
                      (progn
                        (initialize-number :initial-value digit)
                        #'read-integral))))))
         (initialize-number (&key ((:sign s) 1) ((:initial-value v) 0))
           (setf sign s)
           (setf numerator v)
           (setf denominator 1)
           (setf exponent 0)
           (setf exponent-sign 1)
           nil)
         (finish-number (state next-char)
           (ecase state
             ((:integer) (report-token :number (* sign numerator)))
             ((:fraction) (report-token :number (* sign (/ numerator denominator))))
             ((:exponent) 
              (if (zerop exponent) 
                  (report-token :number (* sign (/ numerator denominator)))
                  (let* ((value (coerce (* sign (/ numerator denominator)) 'double-float))
                         (number (* value (expt 10 (* exponent-sign exponent)))))
                    (report-token :number number)))))
           (top-level-state next-char))
         (need-integral (char)
           (let ((digit (and char (digit-char-p char 10))))
             (if (not digit)
                 (report-error "expected a digit after ~:C" #\-)
                 (progn
                   (setf numerator digit)
                   #'read-integral))))
         (read-integral (char)
           (cond
             ((null char) (finish-number :integer char))
             ((eql char #\.) #'need-fractional)
             ((eql char #\e) #'need-exponent)
             ((eql char #\E) #'need-exponent)
             (t (let ((digit (digit-char-p char 10)))
                  (if (not digit)
                      (finish-number :integer char)
                      (progn
                        (setf numerator (+ (* 10 numerator) digit))
                        #'read-integral))))))
         (need-fractional (char)
           (let ((digit (and char (digit-char-p char 10))))
             (if (not digit)
                 (report-error "missing digit after ~:C" #\.)
                 (progn
                   (setf numerator (+ (* 10 numerator) digit))
                   (setf denominator (* denominator 10))
                   #'read-fractional))))
         (read-fractional (char)
           (cond
             ((null char) (finish-number :fraction char))
             ((eql char #\e) #'need-exponent)
             ((eql char #\E) #'need-exponent)
             (t (let ((digit (digit-char-p char 10)))
                  (if (not digit)
                      (finish-number :fraction char)
                      (progn
                        (setf numerator (+ (* 10 numerator) digit))
                        (setf denominator (* denominator 10))
                        #'read-fractional))))))
         (need-exponent (char)
           (cond
             ((null char) (report-error "exponent expected after ~:C" #\e))
             ((eql char #\-) (setf exponent-sign -1) #'need-exponent-1)
             ((eql char #\+) (setf exponent-sign 1) #'need-exponent-1)
             (t (need-exponent-1 char))))
         (need-exponent-1 (char)
           (let ((digit (and char (digit-char-p char 10))))
             (if (not digit)
                 (report-error "exponent expected")
                 (progn 
                   (setf exponent digit)
                   #'read-exponent))))
         (read-exponent (char)
           (let ((digit (and char (digit-char-p char 10))))
             (if (not digit)
                 (finish-number :exponent char)
                 (progn
                   (setf exponent (+ (* 10 exponent) digit))
                   #'read-exponent))))
         (read-string-top-level (char)
           (cond
             ((null char) (report-error "unterminated string literal"))
             ((eql char #\") (report-token :string (copy-seq char-buffer)) #'top-level-state)
             ((eql char #\\) #'read-string-escape)
             (t (vector-push-extend char char-buffer)
                #'read-string-top-level)))
         (read-string-escape (char)
           (cond
             ((null char) (report-error "unterminated string literal"))
             ((eql char #\") (vector-push-extend #\" char-buffer) #'read-string-top-level)
             ((eql char #\\) (vector-push-extend #\\ char-buffer) #'read-string-top-level)
             ((eql char #\n) (vector-push-extend #\newline char-buffer) #'read-string-top-level)
             ((eql char #\r) (vector-push-extend #\return char-buffer) #'read-string-top-level)
             ((eql char #\t) (vector-push-extend #\tab char-buffer) #'read-string-top-level)
             ((eql char #\u) (setf num-digits 0) (setf numerator 0) #'read-string-u-escape)
             (t (report-error "unknown escape control character ~:C" char))))
         (word-char-p (char)
           (or (char<= #\A char #\Z) (char<= #\a char #\z) (char<= #\0 char #\9)
               (eql char #\_) (eql char #\$)))
         (read-string-u-escape (char)
           (if (null char) 
               (report-error "unterminated string literal")
               (let ((digit (digit-char-p char 16)))
                 (cond
                   ((not digit) (report-error "the ~:C escape must be followed by exactly 4 hex digits" #\u))
                   (t (setf numerator (+ digit (* 16 numerator)))
                      (incf num-digits)
                      (if (not (eql num-digits 4))
                          #'read-string-u-escape
                          (progn
                            (vector-push-extend (code-char numerator) char-buffer)
                            #'read-string-top-level)))))))
         (keyword-reporter (keyword token)
           (lambda (char)
             (if (or (null char) (not (word-char-p char)))
                 (progn
                   (report-token token)
                   (top-level-state char))
                 (report-error "parse error; expected end of keyword ~S, found ~:C" keyword char))))
         (keyword-reader (keyword token &optional (position 1))
           (let (self)
             (setf self
                   (lambda (char)
                     (if (not (eql char (char keyword position)))
                         (report-error "parse error; expected character ~:C in keyword ~S, found ~:C" 
                                       (aref keyword position) keyword char)
                         (progn 
                           (incf position)
                           (if (= position (length keyword))
                               (keyword-reporter keyword token)
                               self)))))
             self))
         (after-slash-state (char)
           (cond
             ((eql char #\/) #'skip-line-comment)
             ((eql char #\*) #'skip-block-comment)
             (t (report-error "unexpected character ~:C after ~:C" char #\/))))
         (skip-line-comment (char)
           (if (or (null char) (eql char #\newline))
               #'top-level-state
               #'skip-line-comment))
         (skip-block-comment (char)
           (cond
             ((null char) (report-error "unterminated block comment"))
             ((eql char #\*) #'maybe-after-block-comment)
             (t #'skip-block-comment)))
         (maybe-after-block-comment (char)
           (cond
             ((null char) (report-error "unterminated block comment"))
             ((eql char #\/) #'top-level-state)
             (t (skip-block-comment char)))))
      (setf state #'top-level-state)
      (lambda (buffer &optional (start 0) end)
        (if (null buffer)
            (setf state (funcall state nil))
            (let ((end (or end (length buffer))))
              (loop
                :for index :upfrom start :below end
                :do (setf state (funcall state (char buffer index))))))
        nil))))


(defun make-json-push-lexer (callback &key (byte-input nil))
  (if byte-input
      (make-utf-8-decoder (make-json-push-lexer-1 callback))
      (make-json-push-lexer-1 callback)))


(defun make-json-push-parser-1 (callback)
  (labels
      ((any-value (token value continuation)
         (ecase token
           ((:number :string) (funcall continuation value))
           ((:true) (funcall continuation :true))
           ((:false) (funcall continuation :false))
           ((:null) (funcall continuation :null))
           ((:open-brace) (object-reader continuation))
           ((:open-bracket) (array-reader continuation))))
       (string-value (token value continuation)
         (ecase token
           ((:string) (funcall continuation value))))
       (top-level (token value)
         (ecase token
           ((:end-of-input) (funcall callback :end-of-input) #'top-level)
           ((:number :string) (funcall callback value) #'top-level)
           ((:true) (funcall callback :true) #'top-level)
           ((:false) (funcall callback :false) #'top-level)
           ((:null) (funcall callback :null) #'top-level)
           ((:open-brace) (object-reader (lambda (value) (funcall callback value) #'top-level)))
           ((:open-bracket) (array-reader (lambda (value) (funcall callback value) #'top-level)))))
       (array-reader (continuation)
         (let ((data nil))
           (labels
               ((reader-1 (token value)
                  (if (eql token :close-bracket) 
                      (funcall continuation '(:array))
                      (any-value token value #'got-value)))
                (got-value (object)
                  (push object data)
                  #'close-or-next)
                (need-value (token value)
                  (any-value token value #'got-value))
                (close-or-next (token value)
                  (declare (ignore value))
                  (ecase token
                    ((:comma) #'need-value)
                    ((:close-bracket) (funcall continuation (cons :array (nreverse data)))))))
             #'reader-1)))
       (object-reader (continuation)
         (let ((data nil))
           (labels
               ((reader-1 (token value)
                  (if (eql token :close-brace)
                      (funcall continuation '(:object))
                      (string-value token value #'got-key)))
                (got-key (value)
                  (push value data)
                  #'need-colon)
                (need-colon (token value)
                  (declare (ignore value))
                  (ecase token
                    ((:colon) #'need-value)))
                (need-value (token value)
                  (any-value token value (lambda (object) (push object data) #'close-or-next)))
                (close-or-next (token value)
                  (declare (ignore value))
                  (ecase token
                    ((:comma) #'need-key)
                    ((:close-brace) (funcall continuation (cons :object (nreverse data))))))
                (need-key (token value)
                  (string-value token value #'got-key)))
             #'reader-1))))
    (let ((state #'top-level))
      (lambda (token value)
        (setf state (funcall state token value))
        nil))))
                  
             
(defun make-json-push-parser (callback &key (byte-input nil))
  (if byte-input
      (make-utf-8-decoder (make-json-push-lexer-1 (make-json-push-parser-1 callback)))
      (make-json-push-lexer-1 (make-json-push-parser-1 callback))))
           


