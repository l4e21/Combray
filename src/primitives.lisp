(defpackage combray/primitives
  (:use :cl :serapeum :combray/models :combray/combinators)
  (:export #:pnumber
           #:pinteger
           #:pbool
           #:puppercase
           #:plowercase
           #:pletter
           #:pwhitespace*
           #:pwhitespace+
           #:pcommasep
           #:pword
           #:pexactly))

(in-package :combray/primitives)

(-> pnumber () parser-fn)
(defparser pnumber ()
    (funcall
     (pchoice (pchar #\0)
              (pchar #\1)
              (pchar #\2)
              (pchar #\3)
              (pchar #\4)
              (pchar #\5)
              (pchar #\6)
              (pchar #\7)
              (pchar #\8)
              (pchar #\9))
     state))

(-> pinteger () parser-fn)
(defparser pinteger ()
  (funcall
   (plet* ((r1 (p+ (pnumber))))
     (coerce r1 'string))
   state))

(-> pbool () parser-fn)
(defparser pbool ()
  (funcall
   (plet*
       ((r1 (pchoice
             (pconcat (pchar #\t)
                      (pchar #\r)
                      (pchar #\u)
                      (pchar #\e))
             (pconcat (pchar #\f)
                      (pchar #\a)
                      (pchar #\l)
                      (pchar #\s)
                      (pchar #\e)))))
     (coerce r1 'string))
   state))

(-> plowercase () parser-fn)
(defparser plowercase ()
    (funcall
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
      (pchar #\z))
     state))

(-> puppercase () parser-fn)
(defparser puppercase ()
  (funcall
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
    (pchar #\Z))
   state))

(-> pletter () parser-fn)
(defparser pletter ()
  (funcall
   (pchoice (puppercase) (plowercase))
   state))

(-> pwhitespace* () parser-fn)
(defparser pwhitespace* ()
    (funcall
     (pnoresult (p* (pchar #\Space)))
     state))

(-> pwhitespace+ () parser-fn)
(defparser pwhitespace+ ()
  (funcall
   (pnoresult (p+ (pchar #\Space)))
   state))


(-> pcommasep (parser-fn) parser-fn)
(defparser pcommasep (parser)
  (funcall
   (p*
    (pchoice
     (pfollowedby parser (pconcat (pchar #\,)
                                  (pwhitespace*)))
     (pfollowedby parser (pwhitespace*))))
   state))

(-> pword () parser-fn)
(defparser pword ()
  (funcall
   (plet* ((word (p+ (pletter))))
     (coerce word 'string))
   state))

(-> pexactly (string) parser-fn)
(defparser pexactly (s)
  (funcall
   (plet* ((r1
            (apply #'pconcat (mapcar #'pchar (coerce s 'list)))))
     (coerce r1 'string))
   state))
