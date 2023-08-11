(defpackage combray/models
  (:use :cl :serapeum)
  (:export #:prepare-string-for-parsing
           #:t-state
           #:nil-state
           #:make-t-state
           #:make-nil-state
           #:parser-state
           #:with-state
           #:remaining
           #:line
           #:column
           #:message
           #:result
           #:parser-fn))

(in-package :combray/models)

;; Char lists
(deftype char-list ()
  `(satisfies char-list-p))

(-> char-list-p (list) boolean)
(defun char-list-p (xs)
  (every (lambda (x) (typep x 'character)) xs))

;; Parser state
;; Nil for error, t for success
(defclass p-state ()
  ((line
    :initarg :line
    :accessor line
    :type number)
   (column
    :initarg :column
    :accessor column
    :type number)
   (remaining
    :initarg :remaining
    :accessor remaining
    :type char-list)))

(defclass nil-state (p-state)
  ((message
    :initarg :message
    :accessor message
    :type string)))

(defclass t-state (p-state)
  ((result
    :initarg :result
    :accessor result)))

(deftype parser-state ()
  `(or t-state nil-state))

(defmethod print-object ((obj nil-state) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((message message)
                         (line line)
                         (column column))
            obj
          (format stream "Line ~a Column ~a:~% ~a" line column message))))

(defmethod print-object ((obj t-state) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((remaining remaining)
                         (column column)
                         (line line)
                         (result result))
            obj
          (format stream "Line: ~a Column ~a~%Remaining: ~a~%Result: ~a" line column remaining result))))

(-> make-t-state (number number char-list t) t-state)
(defun make-t-state (line column remaining result)
  (make-instance 't-state :line line
                          :column column
                          :remaining remaining
                          :result result))

(-> make-nil-state (number number char-list string) nil-state)
(defun make-nil-state (line column remaining message)
  (make-instance 'nil-state :line line
                            :column column
                            :remaining remaining
                            :message message))

;; Parser functions
(deftype parser-fn ()
  `(-> (parser-state) parser-state))

(deftype parser-list ()
  `(satisfies parser-list-p))

(-> parser-list-p (list) boolean)
(defun parser-list-p (xs)
  (every (lambda (x) (typep x 'function)) xs))

;; TODO a stream version
(-> read-in (string) t-state)
(defun prepare-string-for-parsing (str)
  (make-t-state 1 1 (coerce str 'list) nil))

;; uiop:read-file-string to get the string from a file
