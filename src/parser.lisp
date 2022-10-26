(in-package :combray)

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
        (t-state result)
        (nil-state state)))))
