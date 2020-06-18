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

(defpackage #:darts.lib.json
  (:use #:common-lisp)
  (:export 
    #:read-json-token #:read-json-value #:read-json-value-from-string
    #:render-json-null #:render-json-number #:render-json-boolean #:render-json-string
    #:render-json-array-start #:render-json-array-end #:render-json-object-start
    #:render-json-object-end #:render-json-key #:render-json-separator
    #:write-json-null #:write-json-number #:write-json-boolean #:write-json-string
    #:write-json-key #:writing-json-array #:writing-json-object #:with-json-output-context
    #:invoke-writing-json-array #:invoke-writing-json-object #:invoke-with-json-output-context
    #:make-json-push-lexer #:make-json-push-parser #:json-match #:json-ematch
    #:if-json-bind #:when-json-bind #:json-parse-error #:simple-json-parse-error))

