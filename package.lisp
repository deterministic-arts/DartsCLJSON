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
  (:shadow #:parse-error #:write-string)
  (:use #:common-lisp)
  (:export 
    #:read-token #:read-value #:read-value-from-string
    #:render-null #:render-number #:render-boolean #:render-string-1 #:render-string
    #:render-array-start #:render-array-end #:render-object-start #:render-object-end
    #:render-key #:render-separator #:write-null #:write-number #:write-boolean
    #:write-string #:write-key #:writing-array #:writing-object #:with-output-context
    #:invoke-writing-array #:invoke-writing-object #:invoke-with-output-context
    #:make-push-lexer #:make-push-parser #:match #:ematch
    #:if-bind #:when-bind #:parse-error #:simple-parse-error))

