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

(in-package #:darts.lib.json)

;;; The "render-json-xxx" functions are only concerned with generating
;;; syntactically valid output of a certain kind. They do not participate
;;; in the output state machinery themselves. These are the basic building
;;; blocks, which are used to implement the machinery.
;;;
;;; The various "write-json-xxx" functions as well as the "writing-xxx"
;;; and "with-json-output-context" macros are part of the output state
;;; machinery. These routines are usually easier to use, since they keep
;;; track of what is currently being written and will inject the necessary
;;; separators and brackets as required.


(defun render-json-null (stream)
  (write-string "null" stream)
  nil)

(defun render-json-boolean (value stream)
  (write-string (if value "true" "false") stream)
  value)

(defun render-json-number (value stream)
  (etypecase value
    (integer (format stream "~D" value))
    (real (format stream "~F" value)))
  value)

(defun render-json-string (value* stream &key (start 0) end ascii-only)
  (let* ((value (etypecase value*
                  (string value*)
                  (symbol (symbol-name value*))
                  (character (string value*))))
         (end (or end (length value))))
    (write-char #\" stream)
    (loop
      :with last := start
      :for k :upfrom start :below end
      :for char := (char value k)
      :for code := (char-code char)
      :unless (or (<= 32 code 126) (and (not ascii-only) (> code 127)))
        :do (when (< last k) (write-string value stream :start last :end k))
            (cond
              ((eql code 9) (write-string "\\t" stream))
              ((eql code 10) (write-string "\\n" stream))
              ((eql code 13) (write-string "\\r" stream))
              ((eql code 34) (write-string "\\\"" stream))
              ((eql code 92) (write-string "\\\\" stream))
              ((> code #xffff) (error "not yet implemented; should do some surrogate block magic here"))
              (t (format stream "\\u~4,'0X" code)))
            (setf last (1+ k))
      :finally (when (< last end) (write-string value stream :start last :end end)))
    (write-char #\" stream)
    value*))

(defun render-json-array-start (stream &key (dense-output t))
  (if dense-output
      (write-char #\[ stream)
      (write-string "[ " stream))
  nil)

(defun render-json-array-end (stream &key (dense-output t))
  (if dense-output
      (write-char #\] stream)
      (write-string " ]" stream))
  nil)

(defun render-json-object-start (stream &key (dense-output t))
  (if dense-output
      (write-char #\{ stream)
      (write-string "{ " stream))
  nil)

(defun render-json-object-end (stream &key (dense-output t))
  (if dense-output
      (write-char #\} stream)
      (write-string " }" stream))
  nil)

(defun render-json-separator (stream &key (dense-output t))
  (if dense-output
      (write-char #\, stream)
      (write-string ", " stream))
  nil)

(defun render-json-key (value stream &key (start 0) end ascii-only (dense-output t))
  (render-json-string value stream :start start :end end :ascii-only ascii-only)
  (if dense-output
      (write-char #\: stream)
      (write-string ": " stream))
  value)



(defvar *json-output-stream* nil
  "The stream, into which the various `write-json-xxx' functions
   write their output. This variable is never assigned, only rebound
   by `invoke-with-json-output-context'.")

(defvar *json-output-ascii-only* nil
  "A boolean flag, which indicates, whether the output should only
   consist of ASCII characters. This affects the way, we generate
   output for string literals.")

(defvar *json-output-dense* t
  "Whether to prefer dense output; if this variable is true, then
   we omit unecessary space characters, otherwise, the output becomes
   more readable for human readers on the expanse of space required.")

(defvar *json-output-state* nil
  "The current state of the json output generator. This variable is
   used to keep track of the progress of the output, so that we can
   inject, for example, commas at their proper positions.")


(defun successor-state (event &optional (state *json-output-state*))
  "Given an event and a current output state (both keywords), this function
   answers two values:

   1. the new output state in effect, after the event's associated
      action has been taken
   2. a flag, which indicates, whether a comma is required before
      any output due to the actual event

   The event may be one of

   `:key'    the key part of a key/value pair within a JSON object
             is to be emitted

   `:value'  a general JSON value will be written. This may either
             be a toplevel value, or some value nested within an 
             array or object structure

   The state may be one of

   `:toplevel'         we are in a top-level output state

   `:first-array-elt'  we are right before the first element in an
                       array literal

   `:array-elt'        we are right after a previous element in an
                       array literal

   `:first-pair-key'   we are right before the first key in an object
                       literal

   `:pair-key'         we are right after a previous key/value pair
                       in an object literal

   `:pair-value'       we are right after the key part of a key/value
                       pair in an object literal

   If a transition is invalid, this function answers a new state of
   `:invalid'. This is the case, for example, on attempts to use the
   `write-json-key' function when we are not currently expecting the
   key of a key/value pair, or any of the functions, which emit general
   values, when we actually need the key of such a pair."
  (ecase event
    ((:key)
     (case state 
       ((:first-pair-key) (values :pair-value nil))
       ((:pair-key) (values :pair-value t))
       (otherwise (values :invalid nil))))
    ((:value)
     (case state
       ((:pair-value) (values :pair-key nil))
       ((:first-array-elt) (values :array-elt nil))
       ((:array-elt) (values :array-elt t))
       ((:toplevel) (values :toplevel nil))
       (otherwise (values :invalid nil))))))


(defun advance-output-state (&optional (event :value))
  (multiple-value-bind (new-state need-separator) (successor-state event)
    (if (eq new-state :invalid)
        (error "operation not permitted in current state")
        (progn
          (when need-separator (render-json-separator *json-output-stream* :dense-output *json-output-dense*))
          (setf *json-output-state* new-state)))))
    




(defun write-json-null ()
  (advance-output-state)
  (render-json-null *json-output-stream*))

(defun write-json-number (value)
  (advance-output-state)
  (render-json-number value *json-output-stream*))

(defun write-json-boolean (value)
  (advance-output-state)
  (render-json-boolean value *json-output-stream*))

(defun write-json-string (value &key (start 0) end)
  (advance-output-state)
  (render-json-string value *json-output-stream* :start start :end end :ascii-only *json-output-ascii-only*))

(defun write-json-key (value &key (start 0) end)
  (advance-output-state :key)
  (render-json-key value *json-output-stream* 
                   :start start :end end :dense-output *json-output-dense*
                   :ascii-only *json-output-ascii-only*))

(defun invoke-writing-json-array (function)
  (advance-output-state)
  (render-json-array-start *json-output-stream* :dense-output *json-output-dense*)
  (let ((*json-output-state* :first-array-elt))
    (funcall function)
    (render-json-array-end *json-output-stream* :dense-output *json-output-dense*))
  nil)

(defmacro writing-json-array (&body body)
  `(invoke-writing-json-array (lambda () ,@body)))

(defun invoke-writing-json-object (function)
  (advance-output-state)
  (render-json-object-start *json-output-stream* :dense-output *json-output-dense*)
  (let ((*json-output-state* :first-pair-key))
    (funcall function)
    (render-json-object-end *json-output-stream* :dense-output *json-output-dense*))
  nil)
    
(defmacro writing-json-object (&body body)
  `(invoke-writing-json-object (lambda () ,@body)))

(defun invoke-with-json-output-context (function stream &key (ascii-only nil) (dense-output t))
  (let ((*json-output-stream* stream)
        (*json-output-ascii-only* ascii-only)
        (*json-output-dense* dense-output)
        (*json-output-state* :toplevel))
    (funcall function)))

(defmacro with-json-output-context ((stream-form 
                                     &rest options
                                     &key ((:ascii-only ao) nil) 
                                          ((:dense-output do) t)) &body body)
  (declare (ignore ao do))
  `(invoke-with-json-output-context (lambda () ,@body) ,stream-form ,@options))




#-(and)
(with-json-output-context (*standard-output* :dense-output t)
  (writing-json-array 
    (write-json-boolean t)
    (write-json-boolean nil)
    (write-json-null)
    (write-json-string "foo")
    (writing-json-object
      (write-json-key "first")
      (write-json-number 1)
      (write-json-key "second")
      (writing-json-object 
        nil))))
