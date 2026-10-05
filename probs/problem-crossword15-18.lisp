;;; Filename: problem-crossword15-18.lisp

;;; Problem specification for 15x15 crossword, best filling with a personal word list.
;;; The search is too large to finish: run it for a fixed time with *randomize-search*, and
;;; keep the best fill found.  (analyze) then fills the remaining slots from the dictionary,
;;; and (repair) keeps as many of the best grid's listed words as the dictionary can complete.
;;; Setting *search-completion-budget* after staging makes the search itself keep every grid
;;; completable, so (analyze) succeeds on the best grid directly.

;;; Design notes:
;;; - One decision per step, in a fixed order.  Each step decides one slot, so every grid
;;;   is reached by exactly one path.
;;; - Most constrained first.  Slots are ordered so each meets letters already placed;
;;;   conflicts then show up early, when they are cheap to undo.
;;; - Skipping is a choice.  Leaving a slot for the dictionary is an explicit move, so no
;;;   slot can block the search and no choice is generated twice.
;;; - Look ahead.  A word is rejected at once if a crossing it changes can no longer become
;;;   a dictionary word.  Each crossing is checked on its own, so a grid can still fail to
;;;   complete where open slots conflict with each other.
;;; - Completion as a separate solver.  Whether the open slots can all be filled together is
;;;   a search of its own: slots keep the sets of words still possible, a choice in one slot
;;;   narrows its crossings until nothing changes, the slot with fewest words is chosen next,
;;;   and regions that share no open cell are solved apart.  It is exact but costs about a
;;;   tenth of a second, so it either checks each placement (every grid completable, far
;;;   fewer grids) or runs once after the search (many grids, listed words dropped to fit).
;;; - Indexed dictionary.  The dictionary is stored as one set of words per length,
;;;   position and letter; checking a pattern combines a few sets instead of walking a tree.
;;; - Optimistic bound.  A cheap estimate of the best a state could still reach cuts off
;;;   branches that cannot beat the best found so far; it must never underestimate.
;;; - Lean state.  Small facts and counts, rather than tables copied at every step.
;;; - Fixed-time runs.  The search is far too large to finish, so each run is given a time
;;;   limit, and random word order lets repeated runs explore different parts of it.
;;; - No goal.  A best-value search records its best states only when no goal is defined.
;;;
;;; Results (5-minute runs, 16 threads; listed words in a grid the dictionary completes):
;;; - Checking every placement ((setf *search-completion-budget* 2000) and
;;;   (setf *min-tasks* 32 *tasks-per-thread* 2) after (ww-set *threads* 16)): 18, 19 and 19
;;;   words in three runs, about 50,000 program cycles each; (analyze) completed every best grid.
;;; - No check, then (repair): 24 words placed, 17 kept, about 22 million program cycles.
;;; - Grid size is the limit: each check fills the whole open grid, so states per minute fall
;;;   about 50 times from a 23-slot grid to this 70-slot one.  Doubling the word list adds
;;;   about one word.  Seed *random-state* before each run, or fresh sessions repeat a run.


(in-package :ww)


(ww-set *problem-name* crossword15-18)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *randomize-search* t)

(ww-set *solution-type* max-value)  ;maximize number of placed words


(defparameter *fields* '((1across 9) (1down 4) (15across 9) (2down 4) (3down 4)
 (17across 9) (4down 6) (19across 5) (5down 7) (6down 3) (7down 4) (8down 4) (9down 5) 
 (20across 3) (22across 3) (26across 8) (23down 7) (33across 6) (29down 4) (30down 5) (39across 5) (42across 6) (25down 7) 
 (34down 4) (47across 8) (43down 7) (24across 6) (51across 3) (48down 6) (56across 5) (61across 9) 
 (64across 9) (57down 4) (58down 4) (66across 9) (59down 4) (50down 5) (54down 4) (55down 4) (62down 3) (53across 3) 
 (49across 6) (27down 10) (28down 10) (32across 3) (37across 4) (26down 4) (41across 4) 
 (45across 5) (38down 8) (52across 5) (60across 5) (46down 6) (63across 5) (65across 5) (52down 4) 
 (10down 6) (31across 5) (11down 8) (12down 10) (13down 10) (10across 5) (16across 5) 
 (18across 5) (21across 5) (14down 4) (35across 4) (40across 4) (44across 3) (36down 4) ))


(defparameter *words*  ;listed alphabetically
  '(admiral alan ann aqua aquabelles arch asa attic atticfan audrey aunt auntaudrey
    auntfrank auntpat auntpolly ave avenue bear beehive bees belles betty bigbob bill
    black bob bonnie brer brerbear brerfox brerrabbit bristol brown browns canasta cardinals
    carl carol carolsue chris cookie cookies cross dave debbie dewart dollar duplex
    edward elaine falls family ferguson florida foote footeave forest forestpark fox frank
    fred freddie gardens george georgie grace graceave grammy groves icerink indiana jamie
    jansens jewelbox jungle katharine kathy key kirkham liz lockwood louise mama mamamary
    marchilden mary marypayne mema missrep mum orange pamlico papaw park pat penochle
    persimmon persimmons piano polly queenie rabbit red redcross remus rep richard rockhill
    saintlouis school scrabble scruggs shirley siesta siestakey sister skating skippy steve stevebrown
    sue sugar tennessee tom tommy trout uncle unclebill uncleremus united unitedway warner
    way webster wichita wolfe woodland zeroweste))


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
    (1down  (1across 0 0 15across 0 1 17across 0 2 19across 0 3))
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


(defparameter *field-lengths* (remove-duplicates (mapcar #'second *fields*)))


(defparameter *words-by-length*
  (let ((ht (make-hash-table)))
    (dolist (word *words* ht)
      (push word (gethash (length (string word)) ht)))))


(defparameter *search-completion-budget* nil
  "Words a completion check may try before a placement is rejected; nil for no check.")


(defparameter *listed-strings* (mapcar #'string *words*))


(defparameter *field-numbers*
  (let ((ht (make-hash-table)))
    (loop for field in *field-names*
          for n from 0
          do (setf (gethash field ht) n))
    ht)
  "Field -> its index in a vector of field texts, in *field-names* order.")


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


(defparameter *letter-table*
  (let ((ht (make-hash-table :test #'equal)))
    (maphash (lambda (len words)
               (declare (ignore words))
               (dotimes (pos len)
                 (setf (gethash (list len pos) ht)
                       (coerce (loop for code from (char-code #\A) to (char-code #\Z)
                                     collect (gethash (list len pos (code-char code)) *letter-sets*))
                               'vector))))
             *dictionary*)
    ht)
  "(length position) -> vector of the 26 letter bit-vectors, A first, nil for an absent letter.")


(defparameter *present-masks*
  (let ((ht (make-hash-table :test #'equal)))
    (maphash (lambda (key letter-vectors)
               (setf (gethash key ht)
                     (loop for letter-vector across letter-vectors
                           for c from 0
                           when letter-vector sum (ash 1 c))))
             *letter-table*)
    ht)
  "(length position) -> integer with bit c set for each letter present there.")


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


;-------------- dictionary completion -----------------------


(defvar *completion-budget* 0
  "Words still to try in the current completion search; bound afresh by each search.")


(defun complete-grid (texts budget)
  ;Fills every open field of texts (a vector of field texts by field number) with dictionary
  ;words.  Returns :complete and the fills as a list of (field word), :impossible if no
  ;completion exists, or :unknown if more than budget words were tried.
  (let ((*completion-budget* budget)
        (domains (initial-domains texts))
        (links (open-links texts)))
    (if (and (full-fields-valid texts)
             (loop for domain across domains never (and domain (zerop (bit-count domain))))
             (propagate domains links (open-numbers domains)))
      (let ((solved (catch :out-of-budget (search-completion domains links (open-numbers domains)))))
        (cond ((eq solved :unknown) :unknown)
              (solved (values :complete
                              (loop for domain across solved
                                    for field in *field-names*
                                    when domain
                                      collect (list field (svref (gethash (field-length field) *dictionary*)
                                                                 (position 1 domain))))))
              (t :impossible)))
      :impossible)))


(defun search-completion (domains links fields)
  ;Assigns the given open fields, one word at a time, propagating each.  Fields that share
  ;no open cell are solved separately, so a failure in one never retries the others.
  ;Returns the solved domains, or nil.
  (let* ((unresolved (remove-if (lambda (n) (= (bit-count (svref domains n) 1) 1)) fields))
         (components (components unresolved links)))
    (cond ((null unresolved) domains)
          ((rest components)
           (dolist (component components domains)
             (setf domains (search-completion domains links component))
             (unless domains
               (return nil))))
          (t (assign-fewest domains links unresolved)))))


(defun assign-fewest (domains links fields)
  ;Tries each word of the field with the fewest words left, then solves the rest.
  (let* ((field-number (first (sort (copy-list fields) #'< :key (lambda (n) (bit-count (svref domains n))))))
         (domain (svref domains field-number)))
    (loop for start = 0 then (1+ k)
          for k = (position 1 domain :start start)
          while k
          do (when (<= (decf *completion-budget*) 0)
               (throw :out-of-budget :unknown))
             (let ((trial (map 'vector (lambda (d) (and d (copy-seq d))) domains)))
               (fill (svref trial field-number) 0)
               (setf (sbit (svref trial field-number) k) 1)
               (when (propagate trial links (list field-number))
                 (let ((solved (search-completion trial links fields)))
                   (when solved
                     (return solved))))))))


(defun components (fields links)
  ;The fields grouped into sets connected through open cells shared among them.
  (let ((remaining (copy-list fields))
        (groups nil))
    (loop while remaining
          do (let ((group nil)
                   (frontier (list (pop remaining))))
               (loop while frontier
                     do (let ((n (pop frontier)))
                          (push n group)
                          (loop for (cross-number) in (svref links n)
                                when (member cross-number remaining)
                                  do (setf remaining (delete cross-number remaining))
                                     (push cross-number frontier))))
               (push group groups)))
    groups))


(defun propagate (domains links queue)
  ;Removes from each open field the words whose letter at a crossing cell the crossing field
  ;no longer allows, until nothing changes.  Starts from the field numbers in queue.  Returns
  ;nil if some field is left with no word.
  (loop while queue
        do (let ((source (pop queue)))
             (loop for (target index cross-index length cross-length) in (svref links source)
                   for target-domain = (svref domains target)
                   when (restrict target-domain cross-length cross-index
                                  (letter-mask (svref domains source) length index))
                     do (unless (find 1 target-domain)
                          (return-from propagate nil))
                        (unless (member target queue)
                          (push target queue)))))
  t)


(defun initial-domains (texts)
  ;A vector by field number: for each open field, a bit-vector of the dictionary words
  ;fitting its text; nil for full fields.
  (map 'vector (lambda (text)
                 (when (find #\? text)
                   (or (matching-words text)
                       (make-array (length (gethash (length text) *dictionary*))
                                   :element-type 'bit :initial-element 1))))
       texts))


(defun full-fields-valid (texts)
  ;True if every full field holds a listed word or a dictionary word.
  (loop for text across texts
        always (or (find #\? text)
                   (member text *listed-strings* :test #'string=)
                   (dictionary-compatible text))))


(defun open-links (texts)
  ;A vector by field number: for each open field, (cross-number index cross-index length
  ;cross-length) for each open cell it shares with a crossing field.  The lengths are the
  ;two fields' word lengths.
  (let ((links (make-array (length texts) :initial-element nil)))
    (loop for field in *field-names*
          for n from 0
          for text = (svref texts n)
          do (loop for (cross-field cross-index index) on (second (assoc field *crosscuts*)) by #'cdddr
                   when (char= (char text index) #\?)
                     do (push (list (gethash cross-field *field-numbers*) index cross-index
                                    (length text) (field-length cross-field))
                              (svref links n))))
    links))


(defun open-numbers (domains)
  (loop for domain across domains
        for n from 0
        when domain collect n))


(defun letter-mask (domain len pos)
  ;Integer with bit c set when some word in domain, of words of length len, has letter c
  ;(A = 0) at pos.  A domain of up to 1000 words is read word by word, stopping once every
  ;letter present there is found; a larger one is tested letter by letter.
  (if (<= (bit-count domain 1000) 1000)
    (let ((words (gethash len *dictionary*))
          (present (gethash (list len pos) *present-masks*))
          (mask 0))
      (loop for start = 0 then (1+ k)
            for k = (position 1 domain :start start)
            while (and k (/= mask present))
            do (let ((code (- (char-code (char (svref words k) pos)) (char-code #\A))))
                 (when (< -1 code 26)
                   (setf mask (logior mask (ash 1 code))))))
      mask)
    (let ((letter-vectors (gethash (list len pos) *letter-table*))
          (mask 0))
      (dotimes (c 26 mask)
        (let ((letter-vector (svref letter-vectors c)))
          (when (and letter-vector (bits-intersect-p domain letter-vector))
            (setf mask (logior mask (ash 1 c)))))))))


(defun restrict (domain len pos mask)
  ;Keeps in domain, of words of length len, only the words whose letter at pos is in mask:
  ;by removing the other letters when few are excluded, else by keeping the allowed ones.
  ;Returns true if any word was removed.
  (let* ((key (list len pos))
         (letter-vectors (gethash key *letter-table*))
         (excluded (logandc2 (gethash key *present-masks*) mask))
         (changed nil))
    (cond ((zerop excluded))
          ((<= (logcount excluded) (logcount mask))
           (dotimes (c 26)
             (when (and (logbitp c excluded) (bits-intersect-p domain (svref letter-vectors c)))
               (bit-andc2 domain (svref letter-vectors c) domain)
               (setf changed t))))
          (t (let ((keep (make-array (length domain) :element-type 'bit :initial-element 0))
                   (before (bit-count domain)))
               (dotimes (c 26)
                 (when (and (logbitp c mask) (svref letter-vectors c))
                   (bit-ior keep (svref letter-vectors c) keep)))
               (bit-and domain keep domain)
               (setf changed (< (bit-count domain) before)))))
    changed))


(defun bits-intersect-p (a b)
  (declare (simple-bit-vector a b) (optimize speed))
  (loop for i of-type fixnum below (ceiling (length a) sb-vm:n-word-bits)
        thereis (/= 0 (logand (sb-kernel:%vector-raw-bits a i) (sb-kernel:%vector-raw-bits b i)))))


(defun bit-count (bits &optional (limit most-positive-fixnum))
  ;The number of 1s in bits, or a number above limit once that is exceeded.
  (declare (simple-bit-vector bits) (fixnum limit) (optimize speed))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (ceiling (length bits) sb-vm:n-word-bits) sum)
      (incf sum (logcount (sb-kernel:%vector-raw-bits bits i)))
      (when (> sum limit)
        (return sum)))))


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


(define-query get-field-texts? ()
  ;A fresh vector of the field texts, by field number.
  (do (setf $texts (make-array (length *field-names*)))
      (ww-loop for $field in *field-names*
               for $n from 0
        do (bind (text $field $text))
           (setf (svref $texts $n) $text))
      $texts))


(define-query completable? (?word ?field)
  ;True if the grid with the word placed can be completed from the dictionary within
  ;*search-completion-budget* tries.
  (eq (complete-grid (place-word (string ?word) ?field (get-field-texts?))
                     *search-completion-budget*)
      :complete))


(define-query bounding-function? ()
  ;(values cost upper), negated for max-value.  Each open field can still get at most one
  ;word, and each length no more words than remain unused at that length; leaving every open
  ;field to the dictionary keeps the words already placed.
  (do (bind (placed $placed))
      (bind (open-fields $open))
      (setf $possible 0)
      (ww-loop for $len in *field-lengths*
        do (setf $possible
                 (+ $possible
                    (min (count $len $open :key #'field-length)
                         (ww-loop for $word in (gethash $len *words-by-length*)
                                  count (not (used $word)))))))
      (values (- (+ $placed $possible)) (- $placed))))


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
         (crosscuts-compatible? ?word ?field)
         (or (null *search-completion-budget*)
             (completable? ?word ?field)))
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


(defun analyze (&optional (budget 100000))
  ;Completes the best grid from the dictionary: the fills as (field word), or :impossible,
  ;or :unknown if more than budget words were tried.
  (multiple-value-bind (status fills) (complete-grid (get-field-texts? (first *best-states*)) budget)
    (if (eq status :complete) fills status)))


(defun repair (&optional (budget 20000))
  ;Rebuilds the best grid from its listed words, in search order, keeping each word only if
  ;the dictionary can still complete the grid.  Returns the listed words kept and the
  ;dictionary fills, each as (field word).
  (let ((kept nil)
        (fills nil))
    (dolist (placement (listed-placements (get-field-texts? (first *best-states*))))
      (multiple-value-bind (status trial-fills) (complete-grid (grid-texts (cons placement kept)) budget)
        (when (eq status :complete)
          (push placement kept)
          (setf fills trial-fills))))
    (values (reverse kept) (or fills (nth-value 1 (complete-grid (grid-texts nil) budget))))))


(defun listed-placements (texts)
  ;The fields holding listed words, as (field word).
  (loop for text across texts
        for field in *field-names*
        when (member text *listed-strings* :test #'string=)
          collect (list field text)))


(defun grid-texts (placements)
  ;A vector of field texts by field number, with only the given (field word) placements.
  (let ((texts (map 'vector (lambda (field) (make-string (field-length field) :initial-element #\?))
                    *field-names*)))
    (loop for (field word) in placements
          do (setf texts (place-word word field texts)))
    texts))


(defun place-word (word field texts)
  ;A copy of texts with word written into field and its crossing cells.
  (let ((new (copy-seq texts)))
    (setf (svref new (gethash field *field-numbers*)) word)
    (loop for (cross-field cross-index index) on (second (assoc field *crosscuts*)) by #'cdddr
          for cross-number = (gethash cross-field *field-numbers*)
          do (setf (svref new cross-number)
                   (replace (copy-seq (svref new cross-number)) word
                            :start1 cross-index :start2 index :end2 (1+ index))))
    new))
