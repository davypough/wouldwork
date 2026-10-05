;;; Filename: problem-crossword5-11-1.lisp

;;; Problem specification for 5x5 crossword, best filling.
;;; A simple illustration of the efficient representation for best-filling crosswords.

;;; Design notes:
;;; - One decision per step, in a fixed order.  Each step decides one slot, so every grid
;;;   is reached by exactly one path.  Letting any open slot be chosen next would make the
;;;   search revisit every ordering of the same choices.
;;; - Most constrained first.  Slots are ordered so each meets letters already placed;
;;;   conflicts then show up early, when they are cheap to undo.
;;; - Skipping is a choice.  In a best-value search, leaving a slot unfilled is an explicit
;;;   move, so every combination of filled and unfilled slots is reachable once.
;;; - Look ahead.  A word is rejected at once if a crossing it changes can no longer become
;;;   a dictionary word, rather than being discovered as a dead end later.
;;; - Indexed dictionary.  The dictionary is stored as one set of words per length,
;;;   position and letter; checking a pattern combines a few sets instead of walking a tree.
;;; - Optimistic bound.  A cheap estimate of the best a state could still reach cuts off
;;;   branches that cannot beat the best found so far; it must never underestimate.
;;; - Lean state.  Small facts and counts, rather than tables copied at every step, keep
;;;   each step cheap.
;;; - Neutral data order.  The words are listed alphabetically, so the order of the data
;;;   does not steer the search toward the answer.


(in-package :ww)


(ww-set *problem-name* crossword5-11-1)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* max-value)  ;maximize number of placed words


(defparameter *fields*  ;(field length)
  '((1across 4) (5across 4) (6across 5) (7across 4) (8across 4)
    (1down 5) (2down 5) (3down 5) (4down 3) (6down 3)))


(defparameter *words*  ;listed alphabetically
  '(epic iowa pique plate psst sound squid ssw suns swiss tad was weds))


(defparameter *crosscuts*  ;intersecting fields
  '((1across (1down 0 0 2down 0 1 3down 0 2 4down 0 3))
    (5across (1down 1 0 2down 1 1 3down 1 2 4down 1 3))
    (6across (6down 0 0 1down 2 1 2down 2 2 3down 2 3 4down 2 4))
    (7across (6down 1 0 1down 3 1 2down 3 2 3down 3 3))
    (8across (6down 2 0 1down 4 1 2down 4 2 3down 4 3))
    (1down (1across 0 0 5across 0 1 6across 1 2 7across 1 3 8across 1 4))
    (2down (1across 1 0 5across 1 1 6across 2 2 7across 2 3 8across 2 4))
    (3down (1across 2 0 5across 2 1 6across 3 2 7across 3 3 8across 3 4))
    (4down (1across 3 0 5across 3 1 6across 4 2))
    (6down (6across 0 0 7across 0 1 8across 0 2))))


(defparameter *crosscuts-ht*  ;(field cross-field) -> (index cross-index), for post-processing
  (let ((ht (make-hash-table :test #'equal)))
    (iter (for (field cuts) in *crosscuts*)
          (loop for (cross-field cross-index index) on cuts by #'cdddr
                do (setf (gethash (list field cross-field) ht)
                         (list index cross-index))))
    ht))


(defun crossing-fields (field)
  (loop for (cross-field) on (second (assoc field *crosscuts*)) by #'cdddr
        collect cross-field))


(defun field-length (field)
  (second (assoc field *fields*)))


(defun next-crossing-field (ordered remaining)
  ;The remaining field crossing the most ordered fields, longer first on ties.
  (let ((best nil) (best-key nil))
    (dolist (field remaining best)
      (let ((key (list (count-if (lambda (cross) (member cross ordered)) (crossing-fields field))
                       (field-length field))))
        (when (or (null best-key)
                  (> (first key) (first best-key))
                  (and (= (first key) (first best-key)) (> (second key) (second best-key))))
          (setf best field best-key key))))))


(defun crossing-order ()
  ;Fields ordered so each crosses as many earlier fields as possible, starting with the
  ;field that has the most crossings.
  (let* ((names (mapcar #'first *fields*))
         (first-field (next-crossing-field names names))
         (ordered (list first-field)))
    (loop for remaining = (set-difference names ordered)
          while remaining
          do (setf ordered (append ordered (list (next-crossing-field ordered remaining)))))
    ordered))


(defparameter *field-names* (crossing-order))


;-------------- dictionary -----------------------


(defparameter *dictionary* (make-hash-table)
  "Length -> vector of the dictionary words of that length, each also reversed.")


(defparameter *letter-sets* (make-hash-table :test #'equal)
  "(length position char) -> bit-vector marking the words of that length with char at position.")


(defun read-dictionary (dictionary-file)
  ;Collects the dictionary words of the field lengths, forwards and reversed, by length.
  (let ((lengths (remove-duplicates (mapcar #'second *fields*)))
        (by-length (make-hash-table)))
    (dolist (word (uiop:read-file-lines dictionary-file))
      (when (member (length word) lengths)
        (push word (gethash (length word) by-length))
        (push (reverse word) (gethash (length word) by-length))))
    (maphash (lambda (len words)
               (setf (gethash len *dictionary*) (coerce words 'vector)))
             by-length)))


(defun index-dictionary ()
  ;Marks, for each length, position and letter, the words with that letter there.
  (maphash (lambda (len words)
             (dotimes (pos len)
               (loop for word across words
                     for index from 0
                     do (setf (sbit (letter-set len pos (char word pos) (length words)) index) 1))))
           *dictionary*))


(defun letter-set (len pos chr size)
  (let ((key (list len pos chr)))
    (or (gethash key *letter-sets*)
        (setf (gethash key *letter-sets*) (make-array size :element-type 'bit :initial-element 0)))))


(read-dictionary (in-src "English-words-455K.txt"))
(index-dictionary)


(defun matching-words (pattern)
  ;Bit-vector of the dictionary words fitting a pattern of letters and ?s; nil if no letter
  ;is fixed.
  (let ((result nil)
        (len (length pattern)))
    (dotimes (pos len result)
      (let ((chr (char pattern pos)))
        (unless (char= chr #\?)
          (let ((letter-set (gethash (list len pos chr) *letter-sets*)))
            (unless letter-set
              (return-from matching-words (make-array 0 :element-type 'bit)))
            (setf result (if result
                           (bit-and result letter-set result)
                           (copy-seq letter-set)))))))))


(defun dictionary-compatible (pattern)  ;eg, "A?R??" of length 5
  "True if some dictionary word, forwards or reversed, fits the pattern."
  (let ((matches (matching-words pattern)))
    (if matches
      (find 1 matches)
      (gethash (length pattern) *dictionary*))))


(defun dictionary-compatible-all (pattern)
  "All dictionary words, forwards or reversed, that fit the pattern."
  (let ((words (gethash (length pattern) *dictionary*))
        (matches (matching-words pattern)))
    (if matches
      (loop for bit across matches
            for word across words
            when (= bit 1) collect word)
      (coerce words 'list))))


;---------------- types and relations ---------------------


(define-types
  field (compute *field-names*)
  word (compute *words*))


(define-dynamic-relations
  (text field $string)
  (used word)
  (open-fields $list)
  (placed $fixnum))


(define-static-relations
  (crosscuts field $list))


;------------------------- queries -----------------------


(define-query get-next-field? ()
  (do (bind (open-fields $open))
      (if $open
        (list (first $open))
        nil)))


(define-query word-compatible? (?word ?field)
   (and (bind (text ?field $field-string))
        (setf $word-string (string ?word))
        (= (length $word-string) (length $field-string))
        (every (lambda (char1 char2)
                  (or (char= char1 char2)
                      (char= char2 #\?)))
               $word-string $field-string)))


(define-query crosscuts-compatible? (?word ?field)
  ;Every crossing whose letter the word changes must still match a dictionary word.
  (and (bind (crosscuts ?field $crosscuts))
       (ww-loop for ($cross-field $cross-index $word-index) on $crosscuts by #'cdddr
         always (do (bind (text $cross-field $cross-str))
                    (or (char= (char $cross-str $cross-index) (char (string ?word) $word-index))
                        (dictionary-compatible
                          (replace (copy-seq $cross-str) (string ?word)
                                   :start1 $cross-index :start2 $word-index :end2 (1+ $word-index))))))))


(define-query bounding-function? ()
  ;(values cost upper), negated for max-value: at best every open field gets a word, and
  ;leaving them all to the dictionary keeps the words already placed.
  (do (bind (placed $placed))
      (bind (open-fields $open))
      (values (- (+ $placed (length $open))) (- $placed))))


;------------------------ actions ----------------------------


(define-update update-crosscut! (?cross-field ?cross-index ?word-index ?word-string)
  (do (bind (text ?cross-field $cross-str))
      (text ?cross-field (replace (copy-seq $cross-str) ?word-string
                                  :start1 ?cross-index :start2 ?word-index :end2 (1+ ?word-index)))))


(define-action fill
    1
    (?field (get-next-field?) ?word word)
    (and (not (used ?word))
         (word-compatible? ?word ?field)
         (crosscuts-compatible? ?word ?field))
    (?field ?word)
    (assert (setf $word-string (string ?word))
            (text ?field $word-string)
            (used ?word)
            (bind (crosscuts ?field $crosscuts))
            (ww-loop for ($cross-field $cross-index $word-index) on $crosscuts by #'cdddr
              do (update-crosscut! $cross-field $cross-index $word-index $word-string))
            (bind (open-fields $open))
            (open-fields (rest $open))
            (bind (placed $placed))
            (placed (1+ $placed))
            (assign $objective-value (1+ $placed))))


(define-action skip  ;leave the field for the dictionary
    1
    (?field (get-next-field?))
    (always-true)
    (?field)
    (assert (bind (open-fields $open))
            (open-fields (rest $open))
            (bind (placed $placed))
            (assign $objective-value $placed)))


;------------------ initializations ----------------


(define-init
  `(open-fields ,*field-names*)
  (placed 0))


(define-init-action initialize-crosscuts&text
  0
  ()
  (always-true)
  ()
  (assert (ww-loop for ($field $field-cuts) in *crosscuts*
            do (crosscuts $field $field-cuts))
          (ww-loop for ($field $field-length) in *fields*
            do (text $field (make-string $field-length :initial-element #\?)))))


;no goal: find the best (max-value) of all states


;------------- post-processing ----------------


(defun cull-best-states ()
  (remove-duplicates *best-states* :from-end t :test #'equalp :key #'problem-state.idb))


(defun corresponding-char-lists (word-list1 index1 word-list2 index2)
  ;Returns words in word-list1 that are compatible with the words in word-list2
  (let ((char-list2 (mapcar #'(lambda (word) (char word index2)) word-list2)))
    (remove-if-not #'(lambda (word) (member (char word index1) char-list2)) word-list1)))


(defun full-word ($word-string)
  (notany (lambda (char)
            (char= char #\?))
          $word-string))


(define-query collect-matches? ()
  (ww-loop for $field in *field-names*
    with $final-matches = nil
    do (bind (crosscuts $field $crosscuts))
       (bind (text $field $text))
       (if (full-word $text)
         (push (list $field (coerce $text 'list)) $final-matches)
         (ww-loop for $chr across $text
           with $corresponding = (dictionary-compatible-all $text)  ;progressively reduce list of field matches
           for ($cross-field $cross-index $index) on $crosscuts by #'cdddr
             when (char= $chr #\?)
               do (bind (text $cross-field $cross-text))
                  (setf $cross-matches (dictionary-compatible-all $cross-text))
                  (setf $corresponding (corresponding-char-lists $corresponding $index $cross-matches $cross-index))
           finally (push (cons $field $corresponding) $final-matches)))
    finally (return $final-matches)))


(defun fillin (state)  ;finish filling in puzzle with dictionary words
  (let ((matches (sort (collect-matches? state) #'< :key #'length)))
    (mapcar (lambda (match)
              (cond ((= (length match) 1) match)
                    ((listp (second match)) (list (first match) (coerce (second match) 'string)))
                    ((stringp (second match)) match)
                    (t (error "Unknown items in match = ~A" match))))
            matches)))


(defun compatible-words (option1 option2)
  ;Determines if two field+word optional fillings are compatible.
  (destructuring-bind (field1 word1) option1
    (destructuring-bind (field2 word2) option2
      (destructuring-bind (index1 index2) (gethash (list field1 field2) *crosscuts-ht* '(-1 -1))
        (or (= index1 -1)
            (char= (schar word1 index1) (schar word2 index2)))))))


(defun feasible-solutions (field-sets)
  (let ((memo (make-hash-table :test 'equal)))
    (labels ((collector (field-sets current-collection)
               (if (null field-sets)
                 (list current-collection)
                 (let ((current-set (car field-sets)))
                   (loop for word in (cdr current-set)
                         for field+word = (list (car current-set) word)
                         when (every #'identity
                                     (mapcar (lambda (x)
                                               (or (gethash (list field+word x) memo)
                                                   (setf (gethash (list field+word x) memo)
                                                         (compatible-words field+word x))))
                                             current-collection))
                         nconc (collector (cdr field-sets) 
                                          (cons field+word current-collection)))))))
      (collector field-sets nil))))


(defun analyze ()
  (feasible-solutions (fillin (first *best-states*))))
