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
      (with-accessors ((line line)
                       (column column)
                       (remaining remaining)
                       (result result))
          output
        (and
         (= line 1)
         (= column 1)
         (equal remaining (list #\t #\e #\s #\t))
         (not result)))))))

(test pchar-pass
  (let* ((input (prepare-string-for-parsing "test"))
         (output (funcall (pchar #\t) input)))
    (is (typep output 't-state))
    (with-slots ((line line)
                 (column column)
                 (remaining remaining)
                 (result result))
        output
      (is (= line 1))
      (is (= column 2))
      (is (equal remaining (list #\e #\s #\t)))
      (is (char= result #\t)))))

(test pchar-fail-expected
  (let* ((input (prepare-string-for-parsing "est"))
         (output (funcall (pchar #\t) input)))
    (is (typep output 'nil-state))
    (with-slots ((line line)
                 (column column)
                 (remaining remaining))
        output
      (is (= line 1))
      (is (= column 1))
      (is (equal remaining (list #\e #\s #\t))))))

(test pchar-fail-eof
  (let* ((input (prepare-string-for-parsing ""))
         (output (funcall (pchar #\t) input)))
    (is (typep output 'nil-state))
    (with-slots ((line line)
                 (column column)
                 (message message))
        output
      (is (= line 1))
      (is (= column 1))
      (is (string= message "EOF")))))

(test pchar-newline
  (let* ((input (prepare-string-for-parsing "
ex"))
         (output (funcall (pchar #\linefeed) input)))
    (is (typep output 't-state))
    (with-slots ((line line)
                 (column column)
                 (remaining remaining)
                 (result result))
        output
      (is (= line 2))
      (is (= column 1))
      (is (char= result #\linefeed)))))

(test pconcat
  (let* ((input (prepare-string-for-parsing "test"))
         (pass (funcall (pconcat
                         (pchar #\t)
                         (pchar #\e)
                         (pchar #\s))
                        input))
         (fail (funcall (pconcat
                         (pchar #\t)
                         (pchar #\t)
                         (pchar #\s))
                        input)))
    (is (typep pass 't-state))
    (with-slots ((line line)
                 (column column)
                 (remaining remaining)
                 (result result))
        pass
      (is (= line 1))
      (is (= column 4))
      (is (equal remaining (list #\t)))
      (is (equal result (list #\t #\e #\s))))

    (is (typep fail 'nil-state))
    (with-slots ((line line)
                 (column column)
                 (remaining remaining)
                 (result result))
        fail
      (is (= line 1))
      (is (= column 2))
      (is (equal remaining (list #\e #\s #\t))))))

(test plet-pass-and-fail
      (let* ((input (prepare-string-for-parsing "test"))
             (pass (funcall (plet* ((c1 (pchar #\t))
                                    (c2 (pconcat (pchar #\e)
                                                 (pchar #\s))))
                              (list c1 c2))
                            input))
             (fail (funcall (plet* ((c1 (pchar #\t))
                                    (c2 (pconcat (pchar #\t)
                                                 (pchar #\s))))
                              (list c1 c2))
                            input)))
        (is (= (line pass) 1))
        (is (= (column pass) 4))
        (is (equal (result pass) (list #\t (list #\e #\s))))

        (is (typep fail 'nil-state))
        (is (= (line fail) 1))
        (is (= (column fail) 2))))

(test pchoice-pass-and-fail
  (let* ((input (prepare-string-for-parsing "test"))
         (pass (funcall (pchoice
                         (pchar #\e)
                         (pchar #\t)
                         (pchar #\u))
                        input))
         (fail (funcall (pchoice
                         (pchar #\u))
                        input)))
    (is (= (line pass) 1))
    (is (= (column pass) 2))
    (is (equal (result pass) #\t))

    (is (typep fail 'nil-state))))

(test p*-pass-and-fail
  (let* ((input (prepare-string-for-parsing "uuug"))
         (pass (funcall (p* (pchar #\u)) input))
         (fail (funcall (p* (pchar #\g)) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 4))
    (is (equal (result pass) (list #\u #\u #\u)))

    (is (typep fail 't-state))
    (is (= (column fail) 1))))

(test p+-fail
  (let* ((input (prepare-string-for-parsing "g"))
         (fail (funcall (p+ (pchar #\u)) input)))
    (is (typep fail 'nil-state))))

(test pnoresult-pass
  (let* ((input (prepare-string-for-parsing "test"))
         (pass (funcall (pnoresult (pchar #\t)) input)))
    (is (typep pass 't-state))
    (is (not (result pass)))))

(test pbetween-pass
  (let* ((input (prepare-string-for-parsing "(t)"))
         (pass (funcall (pbetween (pchar #\()
                                  (pchar #\t)
                                  (pchar #\)))
                        input)))
    (is (typep pass 't-state))
    (is (equal (result pass) #\t))))

(test poptional-pass
  (let* ((input (prepare-string-for-parsing "(a)"))
         (pass (funcall (poptional (pchar #\t))
                        input)))
    (is (typep pass 't-state))
    (is (equal (result pass) nil))))

(test pfollowedby-pass
  (let* ((input (prepare-string-for-parsing "a("))
         (pass (funcall (pfollowedby (pchar #\a) (pchar #\())
                        input)))
    (is (typep pass 't-state))
    (is (equal (result pass) #\a))))

(test pprecededby-pass
  (let* ((input (prepare-string-for-parsing "a("))
         (pass (funcall (pprecededby (pchar #\a) (pchar #\())
                        input)))
    (is (typep pass 't-state))
    (is (equal (result pass) #\())))

(test palways-pass-and-fail
  (let* ((input-1 (prepare-string-for-parsing "a"))
         (input-2 (prepare-string-for-parsing ""))
         (pass (funcall (palways) input-1))
         (fail (funcall (palways) input-2)))
    (is (typep pass 't-state))
    (is (equal (result pass) #\a))

    (is (typep fail 'nil-state))))

(test puntil
  (let* ((input (prepare-string-for-parsing "bbab"))
         (pass (funcall (puntil (pchar #\a)) input)))
    (is (typep pass 't-state))
    (is (equal (result pass) (list #\b #\b)))))

(test pnot-fail
  (let* ((input (prepare-string-for-parsing "bbab"))
         (pass (funcall (pexcept (pchar #\b)) input)))
    (is (typep pass 'nil-state))))

(test pexcept-pass
  (let* ((input (prepare-string-for-parsing "bbab"))
         (pass (funcall (pexcept (pchar #\a)) input)))
    (is (typep pass 't-state))
    (is (equal (result pass) #\b))))

(test preturn-pass
  (let* ((input (prepare-string-for-parsing "test"))
         (pass (funcall (preturn "blah") input)))
    (is (typep pass 't-state))
    (is (equal (remaining pass) (list #\t #\e #\s #\t)))
    (is (equal (result pass) "blah"))))

;;
;; Primitives
;;

(test pnumber-pass
  (let* ((input (prepare-string-for-parsing "123"))
         (pass (funcall (pnumber) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 2))
    (is (equal (result pass) #\1))))

(test pinteger-pass
  (let* ((input (prepare-string-for-parsing "123"))
         (pass (funcall (pinteger) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 4))
    (is (equal (result pass) "123"))))

(test pbool-pass
  (let* ((input (prepare-string-for-parsing "true"))
         (pass (funcall (pbool) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 5))
    (is (equal (result pass) "true"))))

(test pwhitespace+-pass
  (let* ((input (prepare-string-for-parsing "  true"))
         (pass (funcall (pwhitespace+) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 3))
    (is (equal (result pass) nil))))

(test pwhitespace*-pass
  (let* ((input (prepare-string-for-parsing "true"))
         (pass (funcall (pwhitespace*) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 1))
    (is (equal (result pass) nil))))

(test plowercase-pass
  (let* ((input (prepare-string-for-parsing "true"))
         (pass (funcall (plowercase) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 2))
    (is (equal (result pass) #\t))))

(test puppercase-pass
  (let* ((input (prepare-string-for-parsing "TRUE"))
         (pass (funcall (puppercase) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 2))
    (is (equal (result pass) #\T))))

(test pletter-pass
  (let* ((input (prepare-string-for-parsing "true"))
         (pass (funcall (pletter) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 2))))

(test pcommasep-pass
  (let* ((input (prepare-string-for-parsing "a,b, c "))
         (pass (funcall (pcommasep (pletter)) input)))
    (is (= (line pass) 1))
    (is (= (column pass) 8))
    (is (equal (result pass) '(#\a #\b #\c)))))

;;
;; Simplified JSON Example
;;

(defpackage :combray/tests/json/parser
  (:use :cl :fiveam  :serapeum :combray/models :combray/combinators :combray/primitives))

(in-package :combray/tests/json/parser)

(defparser json-vector ()
  (funcall
   (pbetween (pchar #\[)
             (pcommasep (json))
             (pchar #\]))
   state))

(defparser kv ()
  (funcall
   (pconcat (pinteger)
            (pnoresult (pconcat
                        (pwhitespace*)
                        (pchar #\:)
                        (pwhitespace*)))
            (json))
   state))

(defparser json-kv ()
  (funcall
   (pbetween (pchar #\{)
             (kv)
             (pchar #\}))
   state))

(defparser json ()
  (funcall
   (pchoice
    (json-kv)
    (json-vector)
    (pinteger))
   state))

(test json-pass
  (let* ((json-1 (prepare-string-for-parsing "{1:{11 : [{3: 5}, 3,5]}}"))
         (pass-1 (funcall (json) json-1)))
    (is (typep pass-1 't-state))))
