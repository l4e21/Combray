(defpackage combray/combinators
  (:use :cl :serapeum)
  (:import-from :combray/models :t-state :nil-state :make-t-state :make-nil-state :with-state :state :parser-fn :parser-state :parser-list :remaining :line :result :make-node :content :tag :update-last-result :replace-tag)
  (:export :pchar :pmany :pchoice :ptag :poptional :pstring :p* :p+ :pbool :pnum :psym))

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
                      (append result (list (make-node :char c)))))
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
                               (list (make-node :string (coerce (mapcar #'content (result result)) 'string))))))
        (nil-state
         result)))))

(-> p* (parser-fn) parser-fn)
(defun p* (parser)
  (with-state
    (let ((result (funcall parser state)))
      (etypecase-of parser-state result
        (t-state
         (funcall (p* parser) result))
        (nil-state
         (make-t-state
          (line state)
          (remaining state)
          (result state)))))))

(-> p+ (parser-fn) parser-fn)
(defun p+ (parser)
  (with-state
    (let ((result (funcall parser state)))
      (etypecase-of parser-state result
        (t-state
         (funcall (p* parser) result))
        (nil-state
         result)))))

(-> pcoerce (parser-fn) parser-fn)
(defun pcoerce (parser coercion-fn)
  (tactile:compose
   parser
   (update-last-result (lambda (node)
                         (make-node (tag node)
                                    (funcall coercion-fn
                                             (coerce (mapcar #'content (content node)) 'string)))))))

(defvar pnum
  (pcoerce
   (ptag
    :number
    (p+
     (pchoice (list (pchar #\0)
                    (pchar #\1)
                    (pchar #\2)
                    (pchar #\3)
                    (pchar #\4)
                    (pchar #\5)
                    (pchar #\6)
                    (pchar #\7)
                    (pchar #\8)
                    (pchar #\9)))))
   #'parse-integer))

(defun map-bool (x)
  (cond
    ((string= x "false")
     nil)
    (t t)))

(defvar pbool
  (pcoerce (ptag :bool
                 (pchoice (list (pmany (list (pchar #\t) (pchar #\r) (pchar #\u) (pchar #\e)))
                                (pmany (list (pchar #\f) (pchar #\a) (pchar #\l) (pchar #\s) (pchar #\e))))))
           #'map-bool))


(defvar psym
  (pcoerce
   (ptag
    :sym
    (p+
     (pchoice (list (pchar #\a)
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
                    (pchar #\z)
                    (pchar #\-)))))
   #'read-from-string))
