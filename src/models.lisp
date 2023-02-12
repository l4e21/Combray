(defpackage combray/models
  (:use :cl :serapeum)
  (:export #:prepare-string-for-parsing
           #:t-state
           #:nil-state
           #:make-t-state
           #:make-nil-state
           #:with-state
           #:content))

(in-package :combray/models)

;; Char lists
(deftype char-list ()
  `(satisfies char-list-p))

(-> char-list-p (list) boolean)
(defun char-list-p (xs)
  (every (lambda (x) (typep x 'character)) xs))

;; Nodes
(defclass node ()
  ((tag
    :initarg :tag
    :accessor tag
    :type keyword)
   (content
    :initarg :content
    :accessor content
    :type list)))

(-> make-node (keyword list) 'node)
(defun make-node (tag content)
  (make-instance 'node :tag tag :content content))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((tag tag)
                     (content content))
        obj
      (format stream "~%TAG: ~a~%CONTENT: ~a" tag content))))

(deftype node-list ()
  `(satisfies node-list-p))

(-> node-list-p (list) boolean)
(defun node-list-p (xs)
  (every (lambda (x) (typep x 'node)) xs))

;; Parser state
;; Nil for error, t for success
(defclass p-state ()
  ((line
    :initarg :line
    :accessor line
    :type number)
   (remaining
    :initarg :remaining
    :accessor remaining
    :type char-list)))

(defclass nil-state (p-state)
  ((msg
    :initarg :msg
    :accessor msg
    :type string)))

(defclass t-state (p-state)
  ((result
    :initarg :result
    :accessor result
    :type node-list)))

(deftype parser-state ()
  `(or t-state nil-state))

(defmethod print-object ((obj nil-state) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((msg msg)
                         (line line))
            obj
          (format stream "~a" msg))))

(defmethod print-object ((obj t-state) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((remaining remaining)
                         (line line)
                         (result result))
            obj
          (format stream "~%Line: ~a~%Remaining: ~a~%Result: ~a" line remaining result))))

(-> make-t-state (number char-list list) t-state)
(defun make-t-state (line remaining result)
  (make-instance 't-state :line line :remaining remaining :result result))

(-> make-nil-state (number char-list string) nil-state)
(defun make-nil-state (line remaining msg)
  (make-instance 'nil-state :line line :remaining remaining :msg msg))

;; Parser functions
(deftype parser-fn ()
  `(-> (parser-state) parser-state))

(deftype parser-list ()
  `(satisfies parser-list-p))

(-> parser-list-p (list) boolean)
(defun parser-list-p (xs)
  (every (lambda (x) (typep x 'function)) xs))

(-> read-in (string) t-state)
(defun prepare-string-for-parsing (str)
  (make-t-state 1 (coerce str 'list) nil))

;; uiop:read-file-string to get the string from a file

(defmacro with-state (&body body)
  "Anaphoric macro that does all the railroading logic for you"
  `(lambda (state)
     (etypecase-of parser-state state
       (nil-state state)
       (t-state
        ,@body))))
