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

