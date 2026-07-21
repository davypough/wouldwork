;;;; Filename: problem-crossword15-18.lisp

;;; Problem specification for 15x15 crossword, best filling

;;; Requires hashtable instead of list for set representation in relations.
;;; Adds trie structure for dictionary lookup.
;;; *Adds bounding function.
;;; Adds post-processing fill options with dictionary words
;;; Adds post-processing select only compatible across/down dictionary words
;;; Skip over words for *trie-ht* that are too short or too long + reverse words OK
;;; *Adds heuristic more filled is closer to goal
;;; *Pre-instantiation of equal length field & word
;;; Representing words initially as strings rather than symbols, "LIZ" vs LIZ
;;; Maintain used-fields and used-word-strings in database
;;; Generate pre-instantiations dynamically from get-remaining-fields? and get-remaining-word-strings?
;;; Fix crosscuts-compatible?
;;; Generate fill actions one field at a time
;;; Tree search
;;; Fields organized progressively maximizing across/down intersections

#|
file        #states     states/sec      time    best

11          40k         16              34      19           
            80          807             84      19

12          40          15              6       16
            80          8754            11      16     

13          40          14              7       16
            80          9182            11      16

14          40          5               837     24
            80          25              2464    24

15          40          11              769     24
            80          45              1649    24

16          40          27              544     25
            80          47              1388    25
|#


(in-package :ww)


(ww-set *problem-name* crossword15-18)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *randomize-search* t)

(ww-set *solution-type* max-value)  ;maximize number of used words

(ww-set *progress-reporting-interval* 10000000)




(defparameter *fields* '((1across 9) (1down 4) (15across 9) (2down 4) (3down 4)
 (17across 9) (4down 6) (19across 5) (5down 7) (6down 3) (7down 4) (8down 4) (9down 5) 
 (20across 3) (22across 3) (26across 8) (23down 7) (33across 6) (29down 4) (30down 5) (39across 5) (42across 6) (25down 7) 
 (34down 4) (47across 8) (43down 7) (24across 6) (51across 3) (48down 6) (56across 5) (61across 9) 
 (64across 9) (57down 4) (58down 4) (66across 9) (59down 4) (50down 5) (54down 4) (55down 4) (62down 3) (53across 3) 
 (49across 6) (27down 10) (28down 10) (32across 3) (37across 4) (26down 4) (41across 4) 
 (45across 5) (38down 8) (52across 5) (60across 5) (46down 6) (63across 5) (65across 5) (52down 4) 
 (10down 6) (31across 5) (11down 8) (12down 10) (13down 10) (10across 5) (16across 5) 
 (18across 5) (21across 5) (14down 4) (35across 4) (40across 4) (44across 3) (36down 4) ))


(defparameter *words* '(ADMIRAL ALAN ANN AQUABELLES AQUA ARCH ASA BELLES BILL BONNIE ATTIC ATTICFAN
 AUDREY AUNT AUNTAUDREY AUNTFRANK AUNTPAT AUNTPOLLY AVE AVENUE BEAR BEEHIVE BEES BETTY BIGBOB BLACK BOB BRER
 BRERBEAR BRERFOX BRERRABBIT BRISTOL BROWN BROWNS CANASTA CARDINALS CARL CAROL CAROLSUE CHRIS
 COOKIE COOKIES CROSS DAVE DEBBIE DEWART DOLLAR DUPLEX EDWARD ELAINE FALLS FAMILY FERGUSON FLORIDA FOOTE
 FOOTEAVE FOREST FORESTPARK FOX FRANK FRED FREDDIE GARDENS GEORGE GEORGIE GRACE GRACEAVE GRAMMY GROVES ICERINK INDIANA JAMIE
 JANSENS JEWELBOX JUNGLE KATHARINE KATHY KEY KIRKHAM LIZ LOCKWOOD LOUISE MAMA MAMAMARY MARCHILDEN MARY
 MARYPAYNE MEMA MISSREP MUM ORANGE
 PAMLICO PAT PAPAW PARK PERSIMMON PERSIMMONS PIANO PENOCHLE POLLY QUEENIE RABBIT RED REDCROSS
 REMUS REP RICHARD ROCKHILL SAINTLOUIS SCHOOL SCRABBLE SCRUGGS
 SHIRLEY SIESTA SIESTAKEY SISTER SKATING SKIPPY STEVE STEVEBROWN SUE SUGAR TENNESSEE TOM TOMMY TROUT UNCLE
 UNCLEBILL UNCLEREMUS UNITED UNITEDWAY WARNER WAY WEBSTER WICHITA WOLFE WOODLAND ZEROWESTE))


(defparameter *crosscuts*  ;intersecting fields
  '((1across (1down 0 0 2down 0 1 3down 0 2 4down 0 3 5down 0 4 6down 0 5 7down 0 6 8down 0 7 9down 0 8))
    (10across (10down 0 0 11down 0 1 12down 0 2 13down 0 3 14down 0 4))
    (15across (1down 1 0 2down 1 1 3down 1 2 4down 1 3 5down 1 4 6down 1 5 7down 1 6 8down 1 7 9down 1 8))
    (16across (10down 1 0 11down 1 1 12down 1 2 13down 1 3 14down 1 4))
    (17across (1down 2 0 2down 2 1 3down 2 2 4down 2 3 5down 2 4 6down 2 5 7down 2 6 8down 2 7 9down 2 8))
    (18across (10down 2 0 11down 2 1 12down 2 2 13down 2 3 14down 2 4))
    (19across (1down 3 0 2down 3 1 3down 3 2 4down 3 3 5down 3 4))
    (20across (7down 3 0 8down 3 1 9down 3 2))
    (21across (10down 3 0 11down 3 1 12down 3 2 13down 3 3 14down 3 4))
    (22across (4down 4 0 5down 4 1 23down 0 2))
    (24across (9down 4 0 25down 0 1 10down 4 2 11down 4 3 12down 4 4 13down 4 5))
    (26across (26down 0 0 27down 0 1 28down 0 2 4down 5 3 5down 5 4 23down 1 5 29down 0 6 30down 0 7))
    (31across (25down 1 0 10down 5 1 11down 5 2 12down 5 3 13down 5 4))
    (32across (26down 1 0 27down 1 1 28down 1 2))
    (33across (5down 6 0 23down 2 1 29down 1 2 30down 1 3 34down 0 4 25down 2 5))
    (35across (11down 6 0 12down 6 1 13down 6 2 36down 0 3))
    (37across (26down 2 0 27down 2 1 28down 2 2 38down 0 3))
    (39across (23down 3 0 29down 2 1 30down 2 2 34down 1 3 25down 3 4))
    (40across (11down 7 0 12down 7 1 13down 7 2 36down 1 3))
    (41across (26down 3 0 27down 3 1 28down 3 2 38down 1 3))
    (42across (23down 4 0 29down 3 1 30down 3 2 34down 2 3 25down 4 4 43down 0 5))
    (44across (12down 8 0 13down 8 1 36down 2 2))
    (45across (27down 4 0 28down 4 1 38down 2 2 46down 0 3 23down 5 4))
    (47across (30down 4 0 34down 3 1 25down 5 2 43down 1 3 48down 0 4 12down 9 5 13down 9 6 36down 3 7))
    (49across (27down 5 0 28down 5 1 38down 3 2 46down 1 3 23down 6 4 50down 0 5))
    (51across (25down 6 0 43down 2 1 48down 1 2))
    (52across (52down 0 0 27down 6 1 28down 6 2 38down 4 3 46down 2 4))
    (53across (50down 1 0 54down 0 1 55down 0 2))
    (56across (43down 3 0 48down 2 1 57down 0 2 58down 0 3 59down 0 4))
    (60across (52down 1 0 27down 7 1 28down 7 2 38down 5 3 46down 3 4))
    (61across (50down 2 0 54down 1 1 55down 1 2 62down 0 3 43down 4 4 48down 3 5 57down 1 6 58down 1 7 59down 1 8))
    (63across (52down 2 0 27down 8 1 28down 8 2 38down 6 3 46down 4 4))
    (64across (50down 3 0 54down 2 1 55down 2 2 62down 1 3 43down 5 4 48down 4 5 57down 2 6 58down 2 7 59down 2 8))
    (65across (52down 3 0 27down 9 1 28down 9 2 38down 7 3 46down 5 4))
    (66across (50down 4 0 54down 3 1 55down 3 2 62down 2 3 43down 6 4 48down 5 5 57down 3 6 58down 3 7 59down 3 8))
    (1down  (1across 0 0 15across 0 1 17across 0 2 18across 0 3))
    (2down  (1across 1 0 15across 1 1 17across 1 2 19across 1 3))
    (3down  (1across 2 0 15across 2 1 17across 2 2 19across 2 3))
    (4down  (1across 3 0 15across 3 1 17across 3 2 19across 3 3 22across 0 4 26across 3 5))
    (5down  (1across 4 0 15across 4 1 17across 4 2 19across 4 3 22across 1 4 26across 4 5 33across 0 6))
    (6down  (1across 5 0 15across 5 1 17across 5 2))
    (7down  (1across 6 0 15across 6 1 17across 6 2 20across 0 3))
    (8down  (1across 7 0 15across 7 1 17across 7 2 20across 1 3))
    (9down  (1across 8 0 15across 8 1 17across 8 2 20across 2 3 24across 0 4))
    (10down (10across 0 0 16across 0 1 18across 0 2 21across 0 3 24across 2 4 31across 1 5))
    (11down (10across 1 0 16across 1 1 18across 1 2 21across 1 3 24across 3 4 31across 2 5 35across 0 6 40across 0 7))
    (12down (10across 2 0 16across 2 1 18across 2 2 21across 2 3 24across 4 4 31across 3 5 35across 1 6 40across 1 7 44across 0 8 47across 5 9))
    (13down (10across 3 0 16across 3 1 18across 3 2 21across 3 3 24across 5 4 31across 4 5 35across 2 6 40across 2 7 44across 1 8 47across 6 9))
    (14down (10across 4 0 16across 4 1 18across 4 2 21across 4 3))
    (23down (22across 2 0 26across 5 1 33across 1 2 39across 0 3 42across 0 4 45across 4 5 49across 4 6))
    (25down (24across 1 0 31across 0 1 33across 5 2 39across 4 3 42across 4 4 47across 2 5 51across 0 6))
    (26down (26across 0 0 32across 0 1 37across 0 2 41across 0 3))
    (27down (26across 1 0 32across 1 1 37across 1 2 41across 1 3 45across 0 4 49across 0 5 52across 1 6 60across 1 7 63across 1 8 65across 1 9))
    (28down (26across 2 0 32across 2 1 37across 2 2 41across 2 3 45across 1 4 49across 1 5 52across 2 6 60across 2 7 63across 2 8 65across 2 9))
    (29down (26across 6 0 33across 2 1 39across 1 2 42across 1 3))
    (30down (26across 7 0 33across 3 1 39across 2 2 42across 2 3 47across 0 4))
    (34down (33across 4 0 39across 3 1 42across 3 2 47across 1 3))
    (36down (35across 3 0 40across 3 1 44across 2 2 47across 7 3))
    (38down (37across 3 0 41across 3 1 45across 2 2 49across 2 3 52across 3 4 60across 3 5 63across 3 6 65across 3 7))
    (43down (42across 5 0 47across 3 1 51across 1 2 56across 0 3 61across 4 4 64across 4 5 66across 4 6))
    (46down (45across 3 0 49across 3 1 52across 4 2 60across 4 3 63across 4 4 65across 4 5))
    (48down (47across 4 0 51across 2 1 56across 1 2 61across 5 3 64across 5 4 66across 5 5))
    (50down (49across 5 0 53across 0 1 61across 0 2 64across 0 3 66across 0 4))
    (52down (52across 0 0 60across 0 1 63across 0 2 65across 0 3))
    (54down (53across 1 0 61across 1 1 64across 1 2 66across 1 3))
    (55down (53across 2 0 61across 2 1 64across 2 2 66across 2 3))
    (57down (56across 2 0 61across 6 1 64across 6 2 66across 6 3))
    (58down (56across 3 0 61across 7 1 64across 7 2 66across 7 3))
    (59down (56across 4 0 61across 8 1 64across 8 2 66across 8 3))
    (62down (61across 3 0 64across 3 1 66across 3 2))))


(defparameter *crosscuts-ht*
  (let ((ht (make-hash-table :test #'equal)))
    (iter (for (field cuts) in *crosscuts*)
          (loop for (cross-field cross-index index) on cuts by #'cdddr
                do (setf (gethash (list field cross-field) ht)
                         (list index cross-index))))
    ht))


(defparameter *field-words-ht*  ;associates fields with all words of same length
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for (field len) in *fields*)
          (iter (for word in *words*)
                (for word-string = (string word))
                (when (= (length word-string) len)
                  (push word-string (gethash field ht)))))
    ht))


(defparameter *sorted-fields*
  ;(sort (copy-list *fields*) #'> :key #'second))  ;longer fields first
  (copy-list *fields*))


(defparameter *sorted-field-names*
  (cons 'start (iter (for (field nil) in *sorted-fields*)
                     (collect field))))


(defparameter *next-field-ht*
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for (field next-field) on *sorted-field-names* by #'cdr)
          (setf (gethash field ht) next-field))
    ht))


(defparameter *word-strings-ht*
  (let ((ht (make-hash-table :test #'equal)))
    (iter (for word-string in (mapcar #'string *words*))
          (setf (gethash word-string ht) t))
    ht))


(defparameter *fields-ht*
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for field in *sorted-field-names*)
          (setf (gethash field ht) t))
    ht))

;-------------- dictionary -----------------------


(defparameter *trie-ht* (make-hash-table))


(defun trie-insert (trie word-string)
  (let ((node trie))
    (dotimes (i (length word-string) node)
      (let ((c (char word-string i)))
        (setf node (or (gethash c node)
                       (setf (gethash c node) (make-hash-table))))))
    ;; set :word-string key for last node
    (setf (gethash :word-string node) t)))


(defun encode-dictionary (dictionary-file)
  ;Read in dictionary word strings from a file and store codes in *trie-ht*.
  (with-open-file (infile dictionary-file :direction :input :if-does-not-exist nil)
    (when (not (streamp infile)) (error "File does not exist!"))
    (let* ((word-strings (uiop:read-file-lines infile))
           (field-lengths (mapcar #'second *fields*))
           (max-field-length (reduce #'max field-lengths))
           (min-field-length (reduce #'min field-lengths)))
      (iter (for word-string in word-strings)
            (when (<= min-field-length (length word-string) max-field-length)
              (trie-insert *trie-ht* (reverse word-string))
              (trie-insert *trie-ht* word-string))))))


(encode-dictionary (in-src "English-words-455K.txt"))  ;encodes words in the src directory


(defun trie-search (trie pattern)
  ;Searches a trie of hts for the first word-string that matches a pattern.
  (labels ((search-node (node pattern current-word-string index)
             (if (= index (length pattern))
                 ;; check if node represents a valid word-string
                 (when (gethash :word-string node) current-word-string)
                 (let ((chr (char pattern index)))
                   (if (eql chr #\?)
                     ;; wildcard character
                     (loop for key being the hash-keys of node using (hash-value child-node)
                        do (when (hash-table-p child-node)
                             (let ((result (search-node  child-node pattern
                                                         (progn (vector-push-extend key current-word-string) current-word-string)
                                                         (1+ index))))
                               (when result
                                 (return result)))))
                     ;; regular character
                     (let ((child-node (gethash chr node)))
                       (when child-node
                         (search-node child-node pattern
                                      (progn (vector-push-extend chr current-word-string) current-word-string)
                                      (1+ index)))))))))
    ;; start search at root node
    (search-node trie pattern (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t) 0)))


(defun trie-search-all (trie pattern)  ;used in post-processing
  ;Searches a trie of hts for ALL word-strings that match a pattern.
  (labels ((search-node (node pattern current-word-string index)
             (if (= index (length pattern))
                 ;; check if node represents a valid word-string
                 (when (gethash :word-string node) (list current-word-string))
                 (let ((chr (char pattern index)))
                   (if (eql chr #\?)
                     ;; wildcard character
                     (loop for key being the hash-keys of node using (hash-value child-node)
                        when (hash-table-p child-node)
                        append (search-node child-node pattern
                                            (concatenate 'string current-word-string
                                                         (string key))
                                            (1+ index)))
                     ;; regular character
                     (let ((child-node (gethash chr node)))
                       (when child-node
                         (search-node child-node pattern
                                      (concatenate 'string current-word-string
                                                   (string chr))
                                      (1+ index)))))))))
    ;; start search at root node
    (search-node trie pattern "" 0)))


(defun dictionary-compatible ($new-cross-str)  ;eg, "A?R??" of length 5
  "Tests if a string (with uppercase alphabetic and ? characters)
   is compatible with any dictionary word-string in a hash-table."
  (trie-search *trie-ht* $new-cross-str))


(defun dictionary-compatible-all ($new-cross-str)  ;eg, "A?R??" of length 5
  "For a pattern string (with uppercase alphabetic and ? characters)
   returns all compatible dictionary word-strings in a hash-table."
  (trie-search-all *trie-ht* $new-cross-str))


;---------------- types ---------------------


(define-types
  field (compute *sorted-field-names*))


(define-dynamic-relations
  (current-field $field)
  (used-word-strings-ht $hash-table)
  (text field $string))


(define-static-relations
  (crosscuts field $list))


(define-query get-next-field? ()
  (do (bind (current-field $current-field))
      (list (gethash $current-field *next-field-ht*))))


(define-query get-next-field-word-strings? ()
  (do (bind (current-field $current-field))
      (setf $next-field (gethash $current-field *next-field-ht*))
      (setf $all-next-field-word-strings (gethash $next-field *field-words-ht*))
      (bind (used-word-strings-ht $used-word-strings-ht))
      (setf $used-word-strings (alexandria:hash-table-keys $used-word-strings-ht))
      (set-difference $all-next-field-word-strings $used-word-strings)))


;------------------------- action rule -----------------------


(define-query word-compatible? ($word-string $field)
   (and (bind (text $field $field-string))
        ;(= (length $word-string) (length $field-string))
        (every (lambda (char1 char2)
                  (or (char= char1 char2)
                      (char= char2 #\?)))
               $word-string $field-string)))


(define-update update-crosscuts! ($field $word-string)
  (do (bind (crosscuts $field $crosscuts))  
      (ww-loop for ($cross-field $cross-index $word-string-index) on $crosscuts by #'cdddr
        do (bind (text $cross-field $cross-str))
           (setf $cross-char (char $cross-str $cross-index))
           (if (char= $cross-char #\?)
             (do (setf $new-cross-str  ;replace one letter from word-string into cross-str
                       (replace (copy-seq $cross-str) $word-string 
                                :start1 $cross-index :start2 $word-string-index :end2 (1+ $word-string-index)))
                 (text $cross-field $new-cross-str))))))
 

(define-action fill
    1
    (?field (get-next-field?) ?word-string (get-next-field-word-strings?))
    (always-true)  ;(word-compatible? ?word-string ?field)
    (?field ?word-string)
    (assert (bind (used-word-strings-ht $used-word-strings-ht))
            (setf $new-used-word-strings-ht (alexandria:copy-hash-table $used-word-strings-ht))
            (if (word-compatible? ?word-string ?field)
              (do 
                 (text ?field ?word-string)  ;fills in a word given cross letters compatible
                 (update-crosscuts! ?field ?word-string)  ;update ? in cross fields
                 (setf (gethash ?word-string $new-used-word-strings-ht) t)))
            (used-word-strings-ht $new-used-word-strings-ht)
            (current-field ?field)
            ;(setf $objective-value (iter (for (word nil) in-hashtable $new-used-word-strings-ht)
            ;                             (sum (length word))))))
            (setf $objective-value (hash-table-count $new-used-word-strings-ht))))

     
;------------------ initializations ----------------


(define-init
  (current-field start)
  (used-word-strings-ht #.(make-hash-table :test #'equal)))


(define-init-action initialize-crosscuts&text
  0
  ()
  (always-true)
  ()
  (assert (ww-loop for ($field $field-cuts) in *crosscuts*
            do (crosscuts $field $field-cuts))
          (ww-loop for ($field $field-length) in *sorted-fields*
            do (text $field (make-string $field-length :initial-element #\?)))))


(define-goal  ;just find the best (max-value) of all states
  nil)

;-------------- pre-processing -----------------


(defun rewrite-dictionary (dictionary-file)
  ;Omit words with strange characters from dictionary
  (with-open-file (infile dictionary-file :direction :input :if-does-not-exist nil)
    (when (not (streamp infile)) (error "File does not exist!"))
    (with-open-file (outfile (concatenate 'string "new-" dictionary-file)
                             :direction :output :if-exists :supersede)
      (let ((word-strings (uiop:read-file-lines infile)))
        (iter (for word-string in word-strings)
              (when (every (lambda (chr) (find chr "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                           word-string)
                (format outfile "~A~%" word-string)))))))


;------------- post-processing ----------------


(define-query get-used-word-strings-ht? ()  ;for a state
  (do (bind (used-word-strings-ht $used-word-strings-ht))
      $used-word-strings-ht))


(define-query get-num-dictionary-words? ()  ;for a state
  (ww-loop for $field in (cdr *sorted-field-names*)
    do (bind (text $field $word-string))
    count (dictionary-compatible $word-string)))  


(defun cull-best-states ()
  (let* ((top-state (first *best-states*))
         (top-value (problem-state.value top-state))
         (top-states (remove-if (lambda (state) (< (problem-state.value state) top-value))
                                *best-states*))
         (unique-top-states (remove-duplicates top-states :key #'get-used-word-strings-ht? :test #'equalp))
         (states&word-counts (loop for state in unique-top-states
                                   for word-count = (get-num-dictionary-words? state)
                                   collect (list word-count state)))
         (max-count (apply #'max (mapcar #'first states&word-counts)))
         (max-word-count-states (remove-if (lambda (pair) (< (first pair) max-count)) 
                                           states&word-counts)))
    (iter (for item in max-word-count-states)
          (print item))
    nil))
   

#|
(defun corresponding-char-lists (word-list1 index1 word-list2 index2)
  ;Returns words in word-list1 that are compatible with the words in word-list2
  (let ((char-list2 (mapcar #'(lambda (word) (char word index2)) word-list2)))
    (remove-if-not #'(lambda (word) (member (char word index1) char-list2)) word-list1)))


(define-query collect-matches? ()
  (ww-loop for $field in *sorted-field-names*
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
|#
