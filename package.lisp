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

(defpackage "DARTS.LIB.JSON"
  (:use "COMMON-LISP")
  (:export 
    "READ-JSON-TOKEN" "READ-JSON-VALUE" "READ-JSON-VALUE-FROM-STRING"
    "RENDER-JSON-NULL" "RENDER-JSON-NUMBER" "RENDER-JSON-BOOLEAN" "RENDER-JSON-STRING"
    "RENDER-JSON-ARRAY-START" "RENDER-JSON-ARRAY-END" "RENDER-JSON-OBJECT-START"
    "RENDER-JSON-OBJECT-END" "RENDER-JSON-KEY" "RENDER-JSON-SEPARATOR"
    "WRITE-JSON-NULL" "WRITE-JSON-NUMBER" "WRITE-JSON-BOOLEAN" "WRITE-JSON-STRING"
    "WRITE-JSON-KEY" "WRITING-JSON-ARRAY" "WRITING-JSON-OBJECT" "WITH-JSON-OUTPUT-CONTEXT"
    "INVOKE-WRITING-JSON-ARRAY" "INVOKE-WRITING-JSON-OBJECT" "INVOKE-WITH-JSON-OUTPUT-CONTEXT"
    "MAKE-JSON-PUSH-LEXER" "MAKE-JSON-PUSH-PARSER"))

