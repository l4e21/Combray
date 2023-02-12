(defpackage combray/combinators
  (:use :cl :serapeum)
  (:import-from :combray/models :t-state :nil-state :make-t-state :make-nil-state :with-state :state :parser-fn :parser-state :parser-list :remaining :line :result :make-node :content)
  (:export :pchar :pmany :pchoice :ptag :poptional :pstring))

(in-package :combray/combinators)

(-> pchar (character) parser-fn)
(defun pchar (c)
  (with-state
    (with-accessors ((line line)
                    (remaining remaining)
                    (result result))
       state
     (cond
       ((not remaining)
        (make-nil-state line
                        remaining
                        (format nil "~%Parsing Error: ~%Line: ~a~%EOF" line)))
       ((char= c (first remaining))
        (make-t-state (if (char= c #\linefeed)
                          (+ 1 line)
                          line)
                      (rest remaining)
                      (append result (list (make-node :char (list c))))))
       (t
        (make-nil-state line
                        remaining
                        (format nil "~%Parsing Error: ~%Line: ~a~%Couldn't parse ~a, expected ~a"
                                line (first remaining) c)))))))

(-> pmany (parser-list) parser-fn)
(defun pmany (parsers)
  (with-state
      (reduce (lambda (acc parser)
                (etypecase-of parser-state acc
                  (t-state (funcall parser acc))
                  (nil-state acc)))
              parsers
              :initial-value state)))

(-> pchoice (parser-list) parser-fn)
(defun pchoice (parsers)
  (with-state
    (reduce (lambda (acc parser)
              (etypecase-of parser-state acc
                (t-state acc)
                (nil-state (funcall parser state))))
            (rest parsers)
            :initial-value (funcall (first parsers) state))))


(-> ptag (keyword parser-fn) parser-fn)
(defun ptag (tag parser)
  (with-state
    (let ((result (funcall parser
                           (make-t-state (line state)
                                         (remaining state)
                                         nil))))
      (etypecase-of parser-state result
        (t-state
         (make-t-state (line result)
                       (remaining result)
                       (append (result state)
                               (list (make-node tag (result result))))))
        (nil-state result)))))

(-> poptional (parser-fn) parser-fn)
(defun poptional (parser)
  (with-state
    (let ((result (funcall parser state)))
      (etypecase-of parser-state result
        (t-state
         result)
        (nil-state
         (make-t-state
          (line state)
          (remaining state)
          (result state)))))))

(-> pstring (string) parser-fn)
(defun pstring (s)
  (with-state
    (let ((result (funcall (pmany (mapcar #'pchar (coerce s 'list))) (make-t-state
                                                                      (line state)
                                                                      (remaining state)
                                                                      nil))))
      (etypecase-of parser-state result
        (t-state
         (make-t-state (line result)
                       (remaining result)
                       (append (result state)
                               (list (make-node :string (list (coerce (mapcan #'content (result result)) 'string)))))))
        (nil-state
         result)))))
