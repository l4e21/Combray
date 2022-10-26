(defpackage combray/tests/main
  (:use :cl
        :combray
        :fiveam)
  (:export #:debug! #:run! #:main-suite #:test-main-suite))

(in-package :combray/tests/main)

(def-suite main-suite)
(in-suite main-suite)

(defun test-main-suite ()
  (debug! 'main-suite))

(test prepare-string 
  (let ((output (prepare-string-for-parsing "test")))
    (is
     (and
      (typep output 't-state)
      (with-slots ((line combray::line)
                   (remaining combray::remaining)
                   (result combray::result))
          output
        (and
         (= line 1)
         (equal remaining (list #\t #\e #\s #\t))
         (not result)))))))


(test pchar
  (is (typep (funcall (pchar #\a)
                      (make-t-state 1 (list #\a #\b) nil))
             't-state))
  (is (typep (funcall (pchar #\a)
                      (make-t-state 1 (list #\b #\a) nil))
             'nil-state)))

(test pmany
  (let ((parser (pmany (list (pchar #\a) (pchar #\linefeed)))))
    (is (and
         (let ((r1
                 (funcall parser (make-t-state 1 (list #\a #\linefeed) nil))))
           (and (typep r1 't-state)
                (= 2 (combray::line r1))))
         (typep
          (funcall parser (make-t-state 1 (list #\a #\b) nil))
          'nil-state)))))

(test pchoice
  (let ((parser (pchoice (list (pchar #\a) (pchar #\b)))))
    (is (and
         (typep
          (funcall parser (make-t-state 1 (list #\b #\c) nil))
          't-state)
         (typep
          (funcall parser (make-t-state 1 (list #\c #\b) nil))
          'nil-state)))))

(test ptag
  (let* ((parser (ptag :string (pmany (list (pchar #\h)
                                            (pchar #\i)))))
         (r1
           (funcall parser (make-t-state 1 (list #\h #\i #\t) nil))))
    (is
     (and (typep r1 't-state)
          (combray::result r1)
          (equal :string (combray::tag (car (combray::result r1))))))))

(test poptional
  (let ((example (funcall (poptional (pchar #\h))
                          (make-t-state 1 (list #\a) nil))))
    (is (= 1 1))))
