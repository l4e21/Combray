(in-package :combray/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :combray)' in your Lisp.

(fiveam:def-suite combray-suite)
(fiveam:in-suite combray-suite)

(defun test-combray-suite ()
  (fiveam:run! 'combray-suite))

(defun debug-combray-suite ()
  (fiveam:debug! 'combray-suite))

(test prepare-string 
  (let ((output (prepare-string-for-parsing "test")))
    (is
     (and
      (typep output 't-state)
      (with-slots ((line models::line)
                   (remaining models::remaining)
                   (result models::result))
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
                (= 2 (models::line r1))))
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
          (models::result r1)
          (equal :string (models::tag (car (models::result r1))))))))
-
(test poptional
  (let ((example (funcall (poptional (pchar #\h))
                          (make-t-state 1 (list #\a) nil))))
    (is (equal (list #\a) (models::remaining example)))))

(test pstring
  (let ((example (funcall (pstring "hi")
                          (make-t-state 1 (list #\h #\i #\t #\h #\e #\r #\e) nil))))
    (is (equal "hi" (first (models:content (first (last (models::result example)))))))))
