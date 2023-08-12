(defpackage combray/combinators
  (:use :cl :serapeum :combray/models)
  (:export #:pchar
           #:pconcat
           #:plet*
           #:pchoice
           #:p*
           #:p+
           #:pnoresult
           #:pbetween))

(in-package :combray/combinators)

(defmacro with-state (&body body)
  "Anaphoric macro that does all the railroading logic for you"
  `(lambda (state)
     (etypecase-of parser-state state
       (nil-state state)
       (t-state
        (with-accessors ((line line)
                         (column column)
                         (remaining remaining)
                         (result result))
            state
          ,@body)))))

(-> pchar (character) parser-fn)
(defun pchar (c)
  (with-state
    (cond
      ((not remaining)
       (make-nil-state line
                       column
                       remaining
                       "EOF"))
      ((char= c (first remaining))
       (make-t-state (if (char= c #\linefeed)
                         (+ 1 line)
                         line)
                     (if (char= c #\linefeed)
                         1
                         (+ 1 column))
                     (rest remaining)
                     c))
      (t
       (make-nil-state line
                       column
                       remaining
                       (format nil "Couldn't parse ~a, expected ~a" (first remaining) c))))))

(-> pconcat (&rest parser-fn) parser-fn)
(defun pconcat (&rest parsers)
  (with-state
    (reduce
     (lambda (acc parser)
       (funcall
        (with-state
          (make-t-state
           line
           column
           remaining
           (append (result acc) (list result))))
        (funcall parser acc)))
     parsers
     :initial-value (make-t-state
                     line
                     column
                     remaining
                     nil))))

(defmacro plet* (bindings &body body)
  "Like let* but each body of the binding is a parser-fn that is applied to the state subsequently, the body is what will be contained in the result (should all the parsers pass)"
  `(with-state
     ,(let ((r-state (gensym)))
        (if (null bindings)
            `(make-t-state
              line
              column
              remaining
              ,@body)
            (destructuring-bind ((var parser) &rest remaining-bindings) bindings
              `(let ((,r-state (funcall ,parser state)))
                 (funcall (lambda (,var)
                            (funcall
                             (plet* ,remaining-bindings ,@body)
                             ,r-state))
                          (etypecase-of parser-state ,r-state
                            (t-state (result ,r-state))
                            (nil-state nil)))))))))


(-> pchoice (&rest parser-fn) parser-fn)
(defun pchoice (&rest parsers)
  (with-state
    (if parsers
        (let ((p-result (funcall (first parsers) state)))
          (etypecase-of parser-state p-result
            (t-state p-result)
            (nil-state
             (if (rest parsers)
                 (funcall (apply #'pchoice (rest parsers)) state)
                 p-result))))

        state)))

(-> p* (parser-fn &optional t) parser-fn)
(defun p* (parser &optional acc)
  (with-state
    (let ((p-result (funcall parser state)))
      (etypecase-of parser-state p-result
        (t-state (funcall (p* parser (append acc (list (result p-result))))
                          p-result))
        (nil-state (make-t-state
                    line
                    column
                    remaining
                    acc))))))

(-> p+ (parser-fn) parser-fn)
(defun p+ (parser)
  (with-state
    (let ((p-result (funcall parser state)))
      (etypecase-of parser-state p-result
        (t-state (funcall (p* parser (list (result p-result))) (funcall parser state)))
        (nil-state
         p-result)))))

(-> pnoresult (parser-fn) parser-fn)
(defun pnoresult (parser)
  (with-state
    (let ((p-result (funcall parser state)))
      (etypecase-of parser-state p-result
        (t-state (make-t-state
                  (line p-result)
                  (column p-result)
                  (remaining p-result)
                  (result state)))
        (nil-state p-result)))))

(-> pbetween (parser-fn parser-fn parser-fn) parser-fn)
(defun pbetween (parser-start parser-inter parser-end)
  (with-state
    (funcall (plet* ((r1 parser-start)
                     (r2 parser-inter)
                     (r3 parser-end))
               r2)
             state)))
