#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- JSON parsing and rendering
  Copyright (c) 2020 Dirk Esser

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

(defun variablep (object)
  (and (symbolp object) (not (keywordp object)) (not (eq object 't)) (not (eq object 'nil))))

(defun ignoredp (object)
  (or (not object) (string= object "_")))

(defun json-key-string (object)
  (if (stringp object) object (substitute #\_ #\- (string-downcase object))))


(defun parse-object-bindings (pattern)
  (labels
      ((misplaced-keyword (symbol)
         (error "misplaced pattern keyword ~S in ~S" symbol pattern)))
    (let (required optional rest-var whole-var)
      (loop
         with state = :required and previous-state = :invalid
         for element in (cdr pattern)
         do (cond
              ((eq element '&optional)
               (ecase state
                 ((:required) (setf state :optional))
                 ((:optional :rest :whole :end) (misplaced-keyword '&optional))))
              ((eq element '&whole)
               (ecase state
                 ((:required :optional :end) (setf previous-state state) (setf state :whole))
                 ((:whole :rest) (misplaced-keyword '&whole))))
              ((eq element '&rest)
               (ecase state
                 ((:required :optional) (setf state :rest))
                 ((:whole :rest :end) (misplaced-keyword '&rest))))
              ((variablep element)
               (ecase state
                 ((:whole) (setf whole-var element) (setf state previous-state))
                 ((:rest) (setf rest-var element) (setf state :end))
                 ((:end) (error "trailing pattern variables after ~S in ~S" '&rest pattern))
                 ((:required :optional)
                  (if (ignoredp element)
                      (error "naked ignored binding in ~S" pattern)
                      (let ((key (json-key-string element)))
                        (if (eq state :optional)
                            (push `(,key (:any ,element)) optional)
                            (push `(,key (:any ,element)) required)))))))
              ((typep element '(cons t (cons t null)))
               (ecase state
                 ((:whole :rest :end) (error "misplaced complex pattern binding in ~S" pattern))
                 ((:optional :required)
                  (let ((key (json-key-string (car element)))
                        (constraint (parse-pattern (cadr element))))
                    (if (eq state :optional)
                        (push `(,key ,constraint) optional)
                        (push `(,key ,constraint) required))))))
              (t (error "malformed object pattern binding in ~S" pattern))))
      (list ':object (reverse required) (reverse optional) rest-var whole-var))))
                    
(defun parse-array-bindings (pattern)
  (labels
      ((misplaced-keyword (symbol)
         (error "misplaced pattern keyword ~S in ~S" symbol pattern)))
    (let (required optional rest-var whole-var)
      (loop
         with state = :required and previous-state = :invalid
         for element in (cdr pattern)
         do (cond
              ((eq element '&optional)
               (ecase state
                 ((:required) (setf state :optional))
                 ((:optional :rest :whole :end) (misplaced-keyword '&optional))))
              ((eq element '&whole)
               (ecase state
                 ((:required :optional :end) (setf previous-state state) (setf state :whole))
                 ((:whole :rest) (misplaced-keyword '&whole))))
              ((eq element '&rest)
               (ecase state
                 ((:required :optional) (setf state :rest))
                 ((:whole :rest :end) (misplaced-keyword '&rest))))
              ((variablep element)
               (ecase state
                 ((:whole) (setf whole-var element) (setf state previous-state))
                 ((:rest) (setf rest-var element) (setf state :end))
                 ((:end) (error "trailing pattern variables after ~S in ~S" '&rest pattern))
                 ((:required) (push `(:any ,element) required))
                 ((:optional) (push `(:any ,element) optional))))
              (t
               (ecase state
                 ((:required) (push (parse-pattern element) required))
                 ((:optional) (push (parse-pattern element) optional))
                 ((:whole :rest :end) (error "misplaced complex binding in ~S" pattern))))))
      (list ':array (reverse required) (reverse optional) rest-var whole-var))))
                    
(defun parse-pattern (pattern)
  (cond
    ((variablep pattern) `(:any ,pattern))
    ((member pattern '(:any :string :boolean :number :null)) `(,pattern _))
    ((eq pattern :object) `(:object nil nil _ nil))
    ((eq pattern :array) `(:array nil nil _ nil))
    ((consp pattern)
     (cond
       ((member (car pattern) '(:any :string :boolean :number :null))
        (if (not (and (cdr pattern) (null (cddr pattern)) (variablep (cadr pattern))))
            (error "malformed pattern ~S" pattern)
            pattern))
       ((eq (car pattern) :object) (parse-object-bindings pattern))
       ((eq (car pattern) :array) (parse-array-bindings pattern))
       (t (error "malformed pattern ~S" pattern))))
    (t (error "malformed pattern ~S" pattern))))

(defun map-over-pattern-variables (function pattern)
  (labels
      ((invoke (name)
         (unless (ignoredp name)
           (funcall function name)))
       (walk (pattern)
         (ecase (car pattern)
           ((:any :string :boolean :null :number) (invoke (cadr pattern)))
           ((:array)
            (destructuring-bind (required optional rest-var whole-var) (cdr pattern)
              (mapc #'walk required)
              (mapc #'walk optional)
              (invoke rest-var)
              (invoke whole-var)))
           ((:object)
            (destructuring-bind (required optional rest-var whole-var) (cdr pattern)
              (mapc (lambda (info) (walk (second info))) required)
              (mapc (lambda (info) (walk (second info))) optional)
              (invoke rest-var)
              (invoke whole-var))))))
    (walk pattern)
    pattern))

(defun list-pattern-variables (pattern)
  (let (list)
    (map-over-pattern-variables (lambda (name) (pushnew name list)) pattern)
    (nreverse list)))

(defun json-boolean-p (value)
  (or (eq value :true) (eq value :false)))

(defun json-null-p (value)
  (eq value :null))

(defun generate-object-constraint-checks (input pattern then-body else-label)
  (destructuring-bind (required optional rest-var whole-var) (cdr pattern)
    (setf rest-var (if (ignoredp rest-var) nil rest-var))
    (setf whole-var (if (ignoredp whole-var) nil whole-var))
    (let ((work-var (gensym))
          (restart (gensym))
          (key-var (gensym))
          (value-var (gensym))
          (missing-init (and required (mapcar #'car required)))
          (missing-var (and required (gensym)))
          (rest-tail (and rest-var (gensym))))
      `(if (not (and (consp ,input) (eq (car ,input) :object)))
           (go ,else-label)
           (let ((,work-var (cdr ,input))
                 ,@(when missing-var `((,missing-var ',missing-init)))
                 ,key-var ,value-var
                 ,@(when rest-var (list rest-tail)))
             (tagbody
                ,restart
                (if (not ,work-var)
                    (progn
                      ,@(when missing-var `((unless (null ,missing-var) (go ,else-label))))
                      ,@(when whole-var `((setf ,whole-var ,input)))
                      ,then-body)
                    (if (or (null (cdr ,work-var)) (not (stringp (car ,work-var))))
                        (go ,else-label)
                        (progn
                          (setf ,key-var (pop ,work-var))
                          (setf ,value-var (pop ,work-var))
                          (cond
                            ,@(loop
                                 for (key pattern) in required
                                 collecting `((string= ,key-var ,key)
                                              ,(generate-pattern-constraint-checks value-var pattern `(progn
                                                                                                        (setf ,missing-var (remove ,key-var ,missing-var :test #'string=))
                                                                                                        (go ,restart))
                                                                                   else-label)))
                            ,@(loop
                                 for (key pattern) in optional
                                 collecting `((string= ,key-var ,key)
                                              ,(generate-pattern-constraint-checks value-var pattern `(go ,restart)
                                                                                   else-label)))
                            ,(if rest-var
                                 `(t (setf ,rest-tail
                                           (cdr (if ,rest-var
                                                    (setf (cdr ,rest-tail) (list ,key-var ,value-var))
                                                    (setf ,rest-var (list ,key-var ,value-var)))))
                                     (go ,restart))
                                 `(t (go ,else-label)))))))))))))

(defun generate-array-constraint-checks (input pattern then-body else-label)
  (destructuring-bind (required optional rest-var whole-var) (cdr pattern)
    (if (not (or required optional))
        `(if (and (consp ,input) (eq (car ,input) :array))
             (progn
               ,@(unless (ignoredp rest-var) `((setf ,rest-var (cdr ,input))))
               ,@(unless (ignoredp whole-var) `((setf ,whole-var ,input)))
               ,then-body)
             (go ,else-label))
        (progn
          (setf rest-var (if (ignoredp rest-var) nil rest-var))
          (setf whole-var (if (ignoredp whole-var) nil whole-var))
          (let ((work-var (or rest-var (gensym)))
                (head-var (gensym)))
            `(if (not (and (consp ,input) (eq (car ,input) :array)))
                 (go ,else-label)
                 (let (,@(unless (eq work-var rest-var) `((,work-var (cdr ,input))))
                       ,head-var)
                   ,@(when (eq work-var rest-var) `((setf ,work-var (cdr ,input))))
                   (tagbody
                      ,@(loop
                           for sub-pattern in required
                           as continue = (gensym)
                           collecting `(if (not ,work-var) (go ,else-label)
                                           (progn
                                             (setf ,head-var (pop ,work-var))
                                             ,(generate-pattern-constraint-checks head-var
                                                                                  sub-pattern
                                                                                  `(go ,continue)
                                                                                  else-label)))
                           collecting continue)
                      ,@(loop
                           for sub-pattern in optional
                           as continue = (gensym)
                           as sub-vars = (list-pattern-variables sub-pattern)
                           collecting `(if (not ,work-var)
                                           (progn
                                             ,@(mapcar (lambda (name) `(setf ,name nil)) sub-vars)
                                             (go ,continue))
                                           (progn
                                             (setf ,head-var (pop ,work-var))
                                             ,(generate-pattern-constraint-checks head-var
                                                                                  sub-pattern
                                                                                  `(go ,continue)
                                                                                  else-label)))
                           collecting continue)
                      ,@(when whole-var `((setf ,whole-var ,input)))
                      ,then-body))))))))

(defun generate-pattern-constraint-checks (input pattern then-body else-label)
  (labels
      ((generate-simple (predicate var)
         (if (ignoredp var)
             `(if (,predicate ,input) ,then-body (go ,else-label))
             `(if (,predicate ,input)
                  (progn (setf ,var ,input) ,then-body)
                  (go ,else-label)))))
    (ecase (car pattern)
      ((:any)
       (if (ignoredp (cadr pattern)) then-body
           `(progn (setf ,(cadr pattern) ,input)
              ,then-body)))
      ((:string) (generate-simple 'stringp (cadr pattern)))
      ((:number) (generate-simple 'numberp (cadr pattern)))
      ((:boolean) (generate-simple 'json-boolean-p (cadr pattern)))
      ((:null) (generate-simple 'json-null-p (cadr pattern)))
      ((:object) (generate-object-constraint-checks input pattern then-body else-label))
      ((:array) (generate-array-constraint-checks input pattern then-body else-label)))))

(defmacro if-json-bind (pattern form &body body-forms)
  (unless (eql 2 (length body-forms)) (error "malformed body"))
  (let* ((pattern (parse-pattern pattern))
         (variables (list-pattern-variables pattern))
         (else (gensym))
         (result (gensym))
         (input (gensym)))
    `(block ,result
       (let ((,input ,form))
         (tagbody
            (let (,@variables)
              ,(generate-pattern-constraint-checks input pattern
                                                   `(return-from ,result ,(car body-forms))
                                                   else))
            ,else
            (return-from ,result ,(cadr body-forms)))))))

(defmacro when-json-bind (pattern form &body body-forms)
  (let* ((pattern (parse-pattern pattern))
         (variables (list-pattern-variables pattern))
         (else (gensym))
         (result (gensym))
         (input (gensym)))
    `(block ,result
       (let ((,input ,form))
         (tagbody
            (let (,@variables)
              ,(generate-pattern-constraint-checks input pattern
                                                   `(return-from ,result (progn ,@body-forms))
                                                   else))
            ,else)))))

(defun expand-json-match (input form otherwise clauses)
  (let ((result-block (gensym)))
    `(block ,result-block
       (let ((,input ,form))
         (tagbody
            ,@(loop
                 for (pattern* . body) in clauses
                 as pattern = (parse-pattern pattern*)
                 as variables = (list-pattern-variables pattern)
                 as else-label = (gensym)
                 as then-body = `(return-from ,result-block (progn ,@body))
                 collecting `(let (,@variables)
                               ,(generate-pattern-constraint-checks input pattern then-body else-label))
                 collecting else-label)
            ,@otherwise)))))

(defmacro json-ematch (form &body clauses)
  (let ((input (gensym)))
    (expand-json-match input form `((error "value ~S fell through" ,input))
                       clauses)))

(defmacro json-match (form &body clauses)
  (expand-json-match (gensym) form nil clauses))


#-(and)
(json-ematch '(:array 1 2 3 4 5)
  ((:array fst snd) (list 'binary fst snd))
  ((:array &rest stuff) (cons 'otherwise stuff)))

#-(and)    
(when-json-bind (:object &optional first ("second" (:number second)) &rest more) '(:object "first" 1 "second" 2 "third" 3)
  (list* first second more))

#-(and)
(list-pattern-variables (parse-pattern '(:object &optional first ("second" (:number second)) &rest more)))

#-(and)
(json-match '(:array 1 2 3 4)
  ((:array &optional _) (print (list 'at-most-one)))
  ((:array _ _ x &rest _) (print (list 'at-least-three x))))
