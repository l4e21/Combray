(defpackage combinator
  (:use :cl :serapeum)
  (:export #:prepare-string-for-parsing
           #:t-state
           #:nil-state
           #:make-t-state
           #:make-nil-state
           #:with-state
           #:with-states
           #:pchar
           #:pmany
           #:pchoice
           #:ptag
           #:poptional))
