(defpackage combray/combinators
  (:use :cl :serapeum :combray/models)
  (:export :pchar :pconcat))

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
       (let ((r (funcall parser acc)))
         (funcall
          (with-state
            (make-t-state
             line
             column
             remaining
             (append (result acc) (list result))))
          r)))
     parsers
     :initial-value (make-t-state
                     line
                     column
                     remaining
                     nil))))

(defmacro plet*-aux (state-var bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((var parser) &rest remaining-bindings) bindings
        `(funcall (lambda (,var)
                    (plet*-aux ,var ,(cdr bindings) ,@body))
                  (funcall ,parser ,state-var)))))

(defmacro plet* (bindings &body body)
  `(with-state
     (plet*-aux ,'state ,bindings ,@body)))

;; (declaim (type parser-fn pnum))
;; (defvar pnum
;;   (pchoice (list (pchar #\0)
;;                  (pchar #\1)
;;                  (pchar #\2)
;;                  (pchar #\3)
;;                  (pchar #\4)
;;                  (pchar #\5)
;;                  (pchar #\6)
;;                  (pchar #\7)
;;                  (pchar #\8)
;;                  (pchar #\9))))


;; (declaim (type parser-fn pint))
;; (defvar pint
;;   (pcoerce
;;    (ptag
;;     :int
;;     (p+
;;      pnum))
;;    #'parse-integer))

;; (-> map-bool (string) boolean)
;; (defun map-bool (x)
;;   (cond
;;     ((string= x "false")
;;      nil)
;;     (t t)))

;; (declaim (type parser-fn pbool))
;; (defvar pbool
;;   (pcoerce (ptag :bool
;;                  (pchoice (list (pmany (list (pchar #\t) (pchar #\r) (pchar #\u) (pchar #\e)))
;;                                 (pmany (list (pchar #\f) (pchar #\a) (pchar #\l) (pchar #\s) (pchar #\e))))))
;;            #'map-bool))

;; (defvar plowercase
;;   (pchoice (list (pchar #\a)
;;                  (pchar #\b)
;;                  (pchar #\c)
;;                  (pchar #\d)
;;                  (pchar #\e)
;;                  (pchar #\f)
;;                  (pchar #\g)
;;                  (pchar #\h)
;;                  (pchar #\i)
;;                  (pchar #\j)
;;                  (pchar #\k)
;;                  (pchar #\l)
;;                  (pchar #\m)
;;                  (pchar #\n)
;;                  (pchar #\o)
;;                  (pchar #\p)
;;                  (pchar #\q)
;;                  (pchar #\r)
;;                  (pchar #\s)
;;                  (pchar #\t)
;;                  (pchar #\u)
;;                  (pchar #\v)
;;                  (pchar #\w)
;;                  (pchar #\x)
;;                  (pchar #\y)
;;                  (pchar #\z)
;;                  (pchar #\-))))

;; (defvar puppercase
;;   (pchoice (list (pchar #\A)
;;                  (pchar #\B)
;;                  (pchar #\C)
;;                  (pchar #\D)
;;                  (pchar #\E)
;;                  (pchar #\F)
;;                  (pchar #\G)
;;                  (pchar #\H)
;;                  (pchar #\I)
;;                  (pchar #\J)
;;                  (pchar #\K)
;;                  (pchar #\L)
;;                  (pchar #\M)
;;                  (pchar #\N)
;;                  (pchar #\O)
;;                  (pchar #\P)
;;                  (pchar #\Q)
;;                  (pchar #\R)
;;                  (pchar #\S)
;;                  (pchar #\T)
;;                  (pchar #\U)
;;                  (pchar #\V)
;;                  (pchar #\W)
;;                  (pchar #\X)
;;                  (pchar #\Y)
;;                  (pchar #\Z)
;;                  (pchar #\-))))

;; (defvar pletter
;;   (pchoice (list puppercase plowercase)))

;; (declaim (type parser-fn psym))
;; (defvar psym
;;   (pcoerce
;;    (ptag
;;     :sym
;;     (p+ pletter))
;;    #'read-from-string))

;; (-> pnoresult (parser-fn) parser-fn)
;; (defun pnoresult (parser)
;;   (with-state
;;       (let ((result (funcall parser state)))
;;         (etypecase-of parser-state result
;;                       (t-state (make-t-state
;;                                 (line result)
;;                                 (remaining result)
;;                                 (result state)))
;;                       (nil-state result)))))

;; (declaim (type parser-fn pparenleft))
;; (defvar pparenleft (pnoresult (pchar #\()))

;; (declaim (type parser-fn pparenright))
;; (defvar pparenright (pnoresult (pchar #\))))

;; (declaim (type parser-fn psquareparenleft))
;; (defvar psquareparenleft (pnoresult (pchar #\[)))

;; (declaim (type parser-fn psquareparenright))
;; (defvar psquareparenright (pnoresult (pchar #\])))


;; (-> pwithparens (parser-fn) parser-fn)
;; (defun pwithparens (parser)
;;   (tactile:compose pparenleft parser pparenright))

;; (-> pwithsquareparens (parser-fn) parser-fn)
;; (defun pwithsquareparens (parser)
;;   (tactile:compose psquareparenleft parser psquareparenright))


;; (-> preplacetag (keyword parser-fn) parser-fn)
;; (defun preplacetag (tag parser)
;;   (with-state
;;       (let ((r (funcall parser state)))
;;         (etypecase-of parser-state r
;;                       (t-state (funcall (update-last-result (partial #'replace-tag tag)) r))
;;                       (nil-state r)))))


;; (defvar pwhitespace (pnoresult (p* (pchar #\ ))))

;; ;; Parse words separated by whitespace
;; (-> pmanywords (t) parser-fn)
;; (defun pmanywords (parsers)
;;   (apply #'tactile:compose (mapcan
;;                             (lambda (parser)
;;                               (list parser pwhitespace))
;;                             parsers)))

;; (defvar pstr 
;;   (pcoerce
;;    (ptag
;;     :string
;;     (pmany
;;      (list
;;       (pnoresult (pchar #\"))
;;       (p* (pcharexcept #\"))
;;       (pnoresult (pchar #\")))))))

;; ;; TODO replace read-from-string with a more performant algorithm
;; (declaim (type parser-fn pfloat))
;; (defvar pfloat
;;   (pcoerce
;;    (ptag
;;     :float
;;     (pmany
;;      (p+
;;       pnum)
;;      (pnoresult (pchar #\.))
;;      (p+ pnum)))
;;    #'read-from-string))

;; (defvar pbool
;;   (pchoice
;;    (list
;;     (pstring "true")
;;     (pstring "false"))))
