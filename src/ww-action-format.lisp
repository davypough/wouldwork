;;; Shared user-facing action phrases and their replay syntax.
(in-package :ww)

(define-condition invalid-action-phrase (simple-error) ())

(defun action-connective-words (string)
  "Split a template phrase into the words emitted between argument values."
  (remove "" (uiop:split-string string
                               :separator '(#\Space #\Tab #\Newline #\Return #\Page))
          :test #'string=))

(defun extract-annotated-values (template provided-args)
  "Check every phrase word and return its argument values, including nested lists."
  (let ((tokens provided-args)
        (values nil))
    (dolist (slot template)
      (if (stringp slot)
        (dolist (word (action-connective-words slot))
          (unless (and tokens (symbolp (first tokens))
                       (string-equal word (symbol-name (first tokens))))
            (error 'invalid-action-phrase
                   :format-control "Malformed action phrase: expected ~S, got ~S."
                   :format-arguments (list word (if tokens (first tokens) :end-of-form))))
          (pop tokens))
        (progn
          (unless tokens
            (error 'invalid-action-phrase
                   :format-control "Malformed action phrase: missing value for ~S."
                   :format-arguments (list slot)))
          (push (pop tokens) values))))
    (when tokens
      (error 'invalid-action-phrase
             :format-control "Malformed action phrase: unexpected trailing tokens ~S."
             :format-arguments (list tokens)))
    (nreverse values)))

(defun strip-display-connectives (action provided-args)
  "Accept a plain argument list or the exact unquoted words of ACTION's template."
  (let ((template (action.effect-format action)))
    (if (or (notany #'stringp template)
            (and (= (length provided-args) (length (action.effect-variables action)))
                 (not (action-phrase-prefix-p template provided-args))))
      provided-args
      (extract-annotated-values template provided-args))))

(defun action-phrase-prefix-p (template arguments)
  "Recognize the template's opening word even in a truncated phrase."
  (and (stringp (first template)) arguments (symbolp (first arguments))
       (let ((word (first (action-connective-words (first template)))))
         (and word (string-equal word (symbol-name (first arguments)))))))

(defun merge-effect-format (action-name instantiations)
  "Interleave template phrases with plain values; preserve nested values as data."
  (let ((action (find action-name *actions* :key #'action.name)))
    (if (and action (some #'stringp (action.effect-format action)))
      (let ((values instantiations))
        (mapcar (lambda (slot)
                  (if (stringp slot) slot (pop values)))
                (action.effect-format action)))
      instantiations)))

(defun write-action-phrase (action arguments stream)
  "Write template words literally, but escape actual values for Lisp read-back."
  (write-char #\( stream)
  (write (action.name action) :stream stream :escape t)
  (dolist (slot (action.effect-format action))
    (write-char #\Space stream)
    (if (stringp slot)
      (write-string slot stream)
      (write (pop arguments) :stream stream :escape t)))
  (write-char #\) stream))

(defun format-action-for-display (form)
  "Return a replay-readable phrase. Keep unknown or malformed forms visible verbatim."
  (let* ((*print-length* nil)
         (*print-level* nil)
         (*print-pretty* nil)
         (*print-case* :upcase)
         (action (find (first form) *actions* :key #'action.name)))
    (handler-case
        (let ((arguments (if action (strip-display-connectives action (rest form))
                            (rest form))))
          (if (and action (some #'stringp (action.effect-format action))
                   (= (length arguments) (length (action.effect-variables action))))
            (with-output-to-string (stream)
              (write-action-phrase action arguments stream))
            (write-to-string form :escape t)))
      (invalid-action-phrase () (write-to-string form :escape t)))))

(defun format-action-entry-for-display (entry)
  "Format an action or a timestamped (time action) report entry."
  (if (and (consp entry) (numberp (first entry)))
    (format nil "(~S ~A)" (first entry)
            (format-action-for-display (second entry)))
    (format-action-for-display entry)))
