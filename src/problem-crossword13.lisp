;;;; Filename: problem-crossword13.lisp

;;; Problem specification for 13x13 crossword.


(in-package :ww)  ;required

(ww-set *problem-name* crossword13)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)


(define-types
    field (7across 8across 9across 10across 12across 13across 14across 16across 18across 20across 22across
           1down 2down 3down 4down 5down 6down 11down 12down 15down 17down 19down 21down)
    word (amontillado gemini wealth atonce euston nun helmet tandem sparta across aristocracy
          date motion strident flaw calais postpone thespian entrance margin norway ante stye))


(define-dynamic-relations
    (contents field $string)
    (filled field)
    (used word)
    (word-count $fixnum))


(define-static-relations
    (crosscuts field $list))


(define-query compatible? (?word ?field)
   (and (bind (contents ?field $field-string))
        (assign $word-string (string ?word))
        (= (length $word-string) (length $field-string))
        (every (lambda (char1 char2)
                  (or (char= char1 char2)
                      (char= char2 #\Space)))
               $word-string $field-string)))


(define-update update-crosscut! (?cross-fld ?cross-idx ?wrd-idx ?word-string)
   (let ()
       (declare (special $counter))
       (bind (contents ?cross-fld $cross-str))
       (assign $new-cross-str   ;replace one letter from word-string into cross-str
             (replace (copy-seq $cross-str) ?word-string 
                      :start1 ?cross-idx :start2 ?wrd-idx :end2 (1+ ?wrd-idx)))
       (contents ?cross-fld $new-cross-str)
       (assign $new-cross-word (intern $new-cross-str))
       (if (and (notany (lambda (char) (char= char #\Space)) $new-cross-str)
                (exists (?word word)
                   (and (eql $new-cross-word ?word)
                        (not (used $new-cross-word)))))
         (do (filled ?cross-fld)
             (used $new-cross-word)
             (incf $counter)))))


(define-update install! (?word ?field)
   (let (($counter 0))
       (declare (special $counter))
       (bind (contents ?field $field-string))
       (assign $word-string (string ?word))
       (contents ?field $word-string)
       (used ?word)
       (filled ?field)
       (incf $counter)
       (bind (crosscuts ?field $crosscuts))
       (ww-loop for ($cross-fld $cross-idx $wrd-idx) on $crosscuts by #'cdddr
          do (update-crosscut! $cross-fld $cross-idx $wrd-idx $word-string))
       (bind (word-count $word-count))
       (word-count (+ $word-count $counter))))
                

(define-action fill
    1
    (?field field ?word word)
    (and (not (filled ?field))
         (not (used ?word))
         (compatible? ?word ?field))
    (?field ?word)
    (assert (install! ?word ?field)))


(define-init
    (word-count 0)
    (contents 7across  "           ")
    (contents 8across  "      ")
    (contents 9across  "      ")
    (contents 10across "      ")
    (contents 12across "      ")
    (contents 13across "   ")
    (contents 14across "      ")
    (contents 16across "      ")
    (contents 18across "      ")
    (contents 20across "      ")
    (contents 22across "           ")
    (contents 1down    "    ")
    (contents 2down    "      ")
    (contents 3down    "        ")
    (contents 4down    "    ")
    (contents 5down    "      ")
    (contents 6down    "        ")
    (contents 11down   "        ")
    (contents 12down   "        ")
    (contents 15down   "      ")
    (contents 17down   "      ")
    (contents 19down   "    ")
    (contents 21down   "    ")
    (crosscuts 7across  (1down 1 0 2down 1 2 3down 1 4 4down 1 6 5down 1 8 6down 1 10))
    (crosscuts 8across  (1down 3 1 2down 3 3 3down 3 5))
    (crosscuts 9across  (4down 3 0 5down 3 2 6down 3 4))
    (crosscuts 10across (11down 0 1 2down 5 3 3down 5 5))
    (crosscuts 12across (12down 0 0 5down 5 2 6down 5 4))
    (crosscuts 13across (3down 6 0 12down 1 2))
    (crosscuts 14across (11down 2 1 15down 0 3 3down 7 5))
    (crosscuts 16across (12down 2 0 17down 0 2))
    (crosscuts 18across (11down 4 1 15down 2 3 19down 0 5))
    (crosscuts 20across (12down 4 0 17down 2 2 21down 0 4))
    (crosscuts 22across (11down 6 0 15down 4 2 19down 2 4 12down 6 6 17down 4 8 21down 2 10))
    (crosscuts 1down    (7across 0 1 8across 1 3))
    (crosscuts 2down    (7across 2 1 8across 3 3 10across 3 5))
    (crosscuts 3down    (7across 4 1 8across 5 3 10across 5 5 14across 5 7))
    (crosscuts 4down    (7across 6 1 9across 0 3))
    (crosscuts 5down    (7across 8 1 9across 2 3 12across 2 5))
    (crosscuts 6down    (7across 10 1 9across 4 3 12across 4 5 16across 4 7))
    (crosscuts 11down   (10across 1 0 14across 1 2 18across 1 4 22across 0 6))
    (crosscuts 12down   (12across 0 0 13across 2 1 16across 0 2 20across 0 4 22across 6 6))
    (crosscuts 15down   (14across 3 0 18across 3 2 22across 2 4))
    (crosscuts 17down   (16across 2 0 20across 2 2 22across 8 4))
    (crosscuts 19down   (18across 5 0 22across 4 2))
    (crosscuts 21down   (20across 4 0 22across 10 2)))


   
(define-goal
  (and (bind (word-count $count))
       (= $count 23)))
