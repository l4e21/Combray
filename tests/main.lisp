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

(test update-last-result
  (is (eql :num
           (tag (last-result (funcall (update-last-result (tactile:partial #'replace-tag :num))
                                      (make-t-state 1 (list #\a #\b) (list (make-node :char (list #\1))))))))))


(test pchar
  (is (typep (funcall (pchar #\a)
                      (make-t-state 1 (list #\a #\b) nil))
             't-state))
  (is (typep (funcall (pchar #\a)
                      (make-t-state 1 (list #\b #\a) nil))
             'nil-state)))

(test pcharexcept
  (is (typep (funcall (pcharexcept #\a)
                      (make-t-state 1 (list #\b #\a) nil))
             't-state))
  (is (typep (funcall (pcharexcept #\a)
                      (make-t-state 1 (list #\a #\b) nil))
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

(test ptagd
  (let* ((parser (ptagd :string (pmany (list (pchar #\h)
                                             (pchar #\i)))))
         (parser-2 (ptagd :string (pstring "hi")))
         (r1
           (funcall parser (make-t-state 1 (list #\h #\i #\t) nil)))
         (r2
           (funcall parser-2 (make-t-state 1 (list #\h #\i #\t) nil))))
    (is
     (and (typep r1 't-state)
          (models:result r1)
          (equal :string (models:tag (car (models:result r1))))))
    (is (equal (list #\h #\i) (models:content (car (models:result r1)))))
    (is (string= "hi" (models:content (car (models:result r2)))))))

(test poptional
  (let ((example (funcall (poptional (pchar #\h))
                          (make-t-state 1 (list #\a) nil))))
    (is (equal (list #\a) (models:remaining example)))))

(test pstring
  (let ((example (funcall (pstring "hi")
                          (make-t-state 1 (list #\h #\i #\t #\h #\e #\r #\e) nil))))
    (is (equal "hi" (models:content (models:last-result example))))))

(test int
  (let ((example (funcall pint (make-t-state 1 (list #\0 #\1 #\2) nil))))
    (is (eql :number (tag (models:last-result example))))
    (is (= 12 (content (models:last-result example))))))

(test parse-bool
  (let ((example (models:last-result
                  (funcall pbool
                           (prepare-string-for-parsing "false")))))
    (is (not
         (content
          example)))
    (is (eql :bool (tag example)))))

(test psym
  (let ((example (models:last-result
                  (funcall psym
                           (prepare-string-for-parsing "false")))))
    (is (eql :sym (tag example)))))

(test pnoresult-and-parens
  (let ((example (funcall (pwithparens (pchar #\a)) (prepare-string-for-parsing "(a)"))))
    (is (not (models:remaining example)))
    (is (eql :char (tag (models:last-result example))))))


(test preplacetag
  (let ((example (funcall (preplacetag :directive psym) (prepare-string-for-parsing "directive"))))
    (is (eql :directive (tag (models:last-result example))))
    (is (eql 'directive (content (models:last-result example))))))

(test pwithwhitespace
  (let ((example (funcall (pmanywords (list (pstring "hi") (pstring "there")))
                          (prepare-string-for-parsing "hi  there  you "))))
    (is (eql 'models::t-state (class-name (class-of example))))))
