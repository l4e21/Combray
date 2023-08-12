(defpackage combray/primitives
  (:use :cl :serapeum :combray/models :combray/combinators)
  (:export #:pnumber
           #:pinteger
           #:pbool
           #:puppercase
           #:plowercase
           #:pletter
           #:pwhitespace))

(in-package :combray/primitives)

(declaim (type pnumber parser-fn))
(defvar pnumber
  (pchoice (pchar #\0)
           (pchar #\1)
           (pchar #\2)
           (pchar #\3)
           (pchar #\4)
           (pchar #\5)
           (pchar #\6)
           (pchar #\7)
           (pchar #\8)
           (pchar #\9)))

(declaim (type pinteger parser-fn))
(defvar pinteger
  (p+ pnumber))

(declaim (type pinteger parser-fn))
(defvar pbool
  (pchoice
   (pconcat (pchar #\t)
            (pchar #\r)
            (pchar #\u)
            (pchar #\e))
   (pconcat (pchar #\f)
            (pchar #\a)
            (pchar #\l)
            (pchar #\s)
            (pchar #\e))))

(declaim (type plowercase parser-fn))
(defvar plowercase
  (pchoice
   (pchar #\a)
   (pchar #\b)
   (pchar #\c)
   (pchar #\d)
   (pchar #\e)
   (pchar #\f)
   (pchar #\g)
   (pchar #\h)
   (pchar #\i)
   (pchar #\j)
   (pchar #\k)
   (pchar #\l)
   (pchar #\m)
   (pchar #\n)
   (pchar #\o)
   (pchar #\p)
   (pchar #\q)
   (pchar #\r)
   (pchar #\s)
   (pchar #\t)
   (pchar #\u)
   (pchar #\v)
   (pchar #\w)
   (pchar #\x)
   (pchar #\y)
   (pchar #\z)))

(declaim (type puppercase parser-fn))
(defvar puppercase
  (pchoice
   (pchar #\A)
   (pchar #\B)
   (pchar #\C)
   (pchar #\D)
   (pchar #\E)
   (pchar #\F)
   (pchar #\G)
   (pchar #\H)
   (pchar #\I)
   (pchar #\J)
   (pchar #\K)
   (pchar #\L)
   (pchar #\M)
   (pchar #\N)
   (pchar #\O)
   (pchar #\P)
   (pchar #\Q)
   (pchar #\R)
   (pchar #\S)
   (pchar #\T)
   (pchar #\U)
   (pchar #\V)
   (pchar #\W)
   (pchar #\X)
   (pchar #\Y)
   (pchar #\Z)))

(declaim (type pletter parser-fn))
(defvar pletter
  (pchoice (list puppercase plowercase)))

(declaim (type pwhitespace parser-fn))
(defvar pwhitespace
  (pnoresult (p+ (pchar #\Space))))
