(defpackage combray/combinators
  (:use :cl :serapeum :combray/models)
  (:export
   #:state
   #:pchar
   #:with-state
   #:defparser
   #:plet*
   #:pconcat
   #:pchoice
   #:p*
   #:p+
   #:pnoresult
   #:pbetween
   #:poptional
   #:pfollowedby
   #:pprecededby
   #:palways
   #:pnot
   #:pexcept
   #:puntil
   #:preturn))

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

(defmacro defparser (name args &body body)
  `(defun ,name ,args
     (with-state ,@body)))

(-> pchar (character) parser-fn)
(defparser pchar (c)
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
                     (format nil "Couldn't parse ~a, expected ~a" (first remaining) c)))))

(-> pconcat (&rest parser-fn) parser-fn)
(defparser pconcat (&rest parsers)
  (reduce
   (lambda (acc parser)
     (funcall
      (with-state
        (make-t-state
         line
         column
         remaining
         (if result
             (append (result acc) (list result))
             (result acc))))
      (funcall parser acc)))
   parsers
   :initial-value (make-t-state
                   line
                   column
                   remaining
                   nil)))

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
(defparser pchoice (&rest parsers)
  (if parsers
      (let ((p-result (funcall (first parsers) state)))
        (etypecase-of parser-state p-result
          (t-state p-result)
          (nil-state
           (if (rest parsers)
               (funcall (apply #'pchoice (rest parsers)) state)
               p-result))))

      state))

(-> p* (parser-fn &optional t) parser-fn)
(defparser p* (parser &optional acc)
  (let ((p-result (funcall parser state)))
    (etypecase-of parser-state p-result
      (t-state (funcall (p* parser (append acc (list (result p-result))))
                        p-result))
      (nil-state (make-t-state
                  line
                  column
                  remaining
                  acc)))))

(-> p+ (parser-fn) parser-fn)
(defparser p+ (parser)
  (let ((p-result (funcall parser state)))
    (etypecase-of parser-state p-result
      (t-state (funcall (p* parser (list (result p-result))) (funcall parser state)))
      (nil-state
       p-result))))

(-> pnoresult (parser-fn) parser-fn)
(defparser pnoresult (parser)
  (let ((p-result (funcall parser state)))
    (etypecase-of parser-state p-result
      (t-state (make-t-state
                (line p-result)
                (column p-result)
                (remaining p-result)
                nil))
      (nil-state p-result))))

(-> pbetween (parser-fn parser-fn parser-fn) parser-fn)
(defparser pbetween (parser-start parser-inter parser-end)
  (funcall (plet* ((r1 parser-start)
                   (r2 parser-inter)
                   (r3 parser-end))
             r2)
           state))

(-> poptional (parser-fn) parser-fn)
(defparser poptional (parser)
  (let ((p-result (funcall parser state)))
    (etypecase-of parser-state p-result
      (t-state p-result)
      (nil-state state))))

(-> pfollowedby (parser-fn parser-fn) parser-fn)
(defparser pfollowedby (parser-1 parser-2)
  (funcall (plet* ((r1 parser-1)
                   (r2 parser-2))
             r1)
           state))

(-> pprecededby (parser-fn parser-fn) parser-fn)
(defparser pprecededby (parser-1 parser-2)
  (funcall (plet* ((r1 parser-1)
                   (r2 parser-2))
             r2)
           state))

(-> palways () parser-fn)
(defparser palways ()
  (cond
    ((not remaining)
     (make-nil-state line
                     column
                     remaining
                     "EOF"))
    (t
     (make-t-state line
                   column
                   (rest remaining)
                   (first remaining)))))

(-> puntil (parser-fn) parser-fn)
(defparser puntil (parser &optional acc)
  (etypecase-of parser-state (funcall parser state)
    (t-state (make-t-state
              line
              column
              remaining
              acc))
    (nil-state
     (let ((p-result (funcall (palways) state)))
       (funcall (puntil parser (append acc (list (result p-result)))) p-result)))))

(-> pnot (parser-fn) parser-fn)
(defparser pnot (parser)
  (let ((p-result (funcall parser state)))
    (etypecase-of parser-state p-result
      (t-state (make-nil-state
                line
                column
                remaining
                (format nil "Unexpectedly parsed ~a" (result p-result))))
      (nil-state state))))

(-> pexcept (parser-fn) parser-fn)
(defparser pexcept (parser)
  (funcall
   (plet* ((r1 (pnot parser))
           (r2 (palways)))
     r2)
   state))

(-> preturn (t) parser-fn)
(defparser preturn (x)
  (make-t-state
   line
   column
   remaining
   x))
