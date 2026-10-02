;;; Filename: constraint-probe-battery.lisp

;;; PB -- PROBE BATTERY (T23, I7).  Specification: doc/constraint-led-solving/Extractor-Specifications.md
;;; section 12.  Small searches from the staged start, one per landmark or resource, deepening
;;; one cutoff at a time to the problem's maximum depth: P1 an agent at a landmark location, P2
;;; cargo set down outside its start region, P3 a primitive controller changed from its start
;;; reading, P4 a fixed relay paired with a connector; and P5, D's own probes (T29, section
;;; 12.8), stated as data.  The result is the Briefing's probe map (Problem-Solving Guide,
;;; Optional tools section).
;;;
;;; A LOADABLE DIAGNOSTIC, like tech/constraint-profile.lisp: plain Common Lisp in :WW, never
;;; named in an INCLUDE-TECH directive, never an ASDF component, no DEFINE-* forms.  Load it
;;; AFTER tech/constraint-profile.lisp, whose helpers it calls.  Definitions are callees-first.
;;; It is the only diagnostic that searches, and it is not part of the profile.
;;;
;;;   (report-probe-battery-list)                               ; the probes; no search
;;;   (run-probe-battery max-depth pathname)                    ; threads 16; writes PATHNAME
;;;   (report-probe-battery pathname)                           ; PB from the results file
;;;   (d-probes '((subject goal "provenance") ...))              ; P5 probes, passed to the runner
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relations HAS-LOCATION, AIMED-AT and PAIRED, the
;;; connectives OR, NOT and EXISTS of a goal form, the types AGENT, CARGO, LOCATION, CONNECTOR,
;;; FLOOR-REPEATER and WALL-REPEATER, and the helpers of tech/constraint-profile.lisp are
;;; named; each is a tech/ or engine interface or that file's own.  No problem object name
;;; appears; every instance comes from the staged databases.

(in-package :ww)


;;;; Generator (spec 12.2) ;;;;


(defun probe-landmark-locations (controls names static)
  "P1's landmark locations, by name: every S4 reach site of a switch primitive, every
   AIMED-AT destination (MC's lift destinations), and every S4 explicit goal destination."
  (sort (remove-duplicates
          (append (loop for primitive in (control-primitives controls)
                        when (eq (keeper-controller-kind primitive) :switch)
                          append (mapcar #'first (keeper-reach-sites primitive static names)))
                  (loop for fact in static
                        when (eq (first fact) 'aimed-at)
                          collect (third fact))
                  (mapcar #'third (keeper-goal-destinations (get 'goal-fn :form)))))
        #'string< :key #'symbol-name))


(defun probe-landmark-probes (landmarks facts)
  "P1: for each agent located in the start FACTS, by name, one probe per landmark other
   than its start location."
  (loop for agent in (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name)
        for start = (keeper-fact-value 'has-location agent facts)
        when start
          append (loop for location in landmarks
                       unless (eq location start)
                         collect (list :family 1 :subject agent
                                       :goal (list 'has-location agent location)))))


(defun probe-resource-probes (names blocks facts)
  "P2: for each cargo object located in the start FACTS, by name, one probe asking it to
   rest at some location outside its start location's S3 region.  None when that region
   holds every location."
  (let ((locations (sort (copy-list (census-type-instances 'location)) #'string< :key #'symbol-name)))
    (loop for object in (sort (copy-list (census-type-instances 'cargo)) #'string< :key #'symbol-name)
          for start = (keeper-fact-value 'has-location object facts)
          for region = (when start
                         (second (find (gethash start names) blocks :key #'first :test #'string=)))
          for outside = (when start
                          (remove-if (lambda (location) (member location region)) locations))
          when outside
            collect (list :family 2 :subject object
                          :goal (cons 'or (mapcar (lambda (location)
                                                    (list 'has-location object location))
                                                  outside))))))


(defun probe-controller-probes (controls facts)
  "P3: for each S1 primitive controller, by name, one probe asking for the opposite of its
   status relation's reading in the start FACTS."
  (loop for primitive in (sort (copy-list (control-primitives controls)) #'string< :key #'symbol-name)
        for literal = (list (control-status-relation primitive) primitive)
        collect (list :family 3 :subject primitive
                      :goal (if (hint-primitive-active-p primitive facts)
                              (list 'not literal)
                              literal))))


(defun probe-relay-probes ()
  "P4: for each fixed relay, by name, one probe asking some connector to be paired with it."
  (loop for relay in (sort (append (copy-list (census-type-instances 'floor-repeater))
                                   (copy-list (census-type-instances 'wall-repeater)))
                           #'string< :key #'symbol-name)
        collect (list :family 4 :subject relay
                      :goal (list 'exists '(?c connector) (list 'paired '?c relay)))))


(defun probe-battery-probes ()
  "Every probe of the staged problem in family order (spec 12.2), each a plist with :ID
   (P<family>.<index>), :FAMILY, :SUBJECT and :GOAL."
  (let* ((controls (control-facts))
         (context (hint-route-context controls))
         (names (getf context :names))
         (facts (from-here-facts *start-state*))
         (families (list (probe-landmark-probes
                           (probe-landmark-locations controls names (list-static-db)) facts)
                         (probe-resource-probes names (getf context :blocks) facts)
                         (probe-controller-probes controls facts)
                         (probe-relay-probes))))
    (loop for probes in families
          append (loop for probe in probes
                       for index from 1
                       collect (list* :id (format nil "P~D.~D" (getf probe :family) index)
                                      probe)))))


;;;; P5 -- D's own probes (spec 12.8, T29) ;;;;


(defparameter *probe-goal-connectives* '(and or not)
  "Goal connectives whose arguments are all goal forms.")


(defparameter *probe-goal-quantifiers* '(exists forall)
  "Goal quantifiers: a variable list, then goal forms.")


(defun d-probe-goal-heads (form)
  "Every literal head in goal FORM: AND, OR and NOT are descended through their arguments,
   EXISTS and FORALL through their bodies, skipping the variable list."
  (cond ((member (first form) *probe-goal-connectives*)
         (loop for subform in (rest form) append (d-probe-goal-heads subform)))
        ((member (first form) *probe-goal-quantifiers*)
         (loop for subform in (cddr form) append (d-probe-goal-heads subform)))
        (t (list (first form)))))


(defun d-probe-check-entry (entry)
  "Signals an error unless ENTRY is (SUBJECT GOAL PROVENANCE) -- a non-NIL symbol, a cons and
   a string -- and every literal head of GOAL is a declared relation or a function."
  (unless (and (listp entry)
               (= (length entry) 3)
               (first entry)
               (symbolp (first entry))
               (consp (second entry))
               (stringp (third entry)))
    (error "A D probe is (SUBJECT GOAL PROVENANCE): a symbol, a goal form and a string; got ~S."
           entry))
  (dolist (head (d-probe-goal-heads (second entry)))
    (unless (or (gethash head *relations*)
                (gethash head *static-relations*)
                (fboundp head))
      (error "D probe ~S: ~S is neither a declared relation nor a query." (first entry) head))))


(defun d-probes (entries)
  "P5, D's own probes (spec 12.8): one probe per entry (SUBJECT GOAL PROVENANCE), numbered
   P5.1 ... in entry order.  Every entry is checked first, so a bad one signals before any
   search.  Run them with (run-probe-battery max-depth pathname (d-probes ...)),
   alone or appended to (probe-battery-probes)."
  (dolist (entry entries)
    (d-probe-check-entry entry))
  (loop for entry in entries
        for index from 1
        collect (list :id (format nil "P5.~D" index) :family 5 :subject (first entry)
                      :goal (second entry) :provenance (third entry))))


;;;; Runner (spec 12.3, 12.4) ;;;;


(defun probe-goal-holds-at-start-p (goal checkpoint)
  "Whether GOAL holds at CHECKPOINT's endpoint.  Installs GOAL as GOAL-FN; the entry point
   restores the staged goal afterwards."
  (install-compiled-goal goal)
  (and (funcall (symbol-function 'goal-fn) (search-checkpoint-state checkpoint)) t))


(defun probe-run-once (checkpoint goal cutoff)
  "One standalone search for GOAL from CHECKPOINT at CUTOFF, solution type FIRST.  Both are
   set globally, since parallel workers do not see a thread's LET bindings; the entry point
   restores them.  A plist: :CUTOFF, :FOUND, :STATUS and :REASON of the planner's outcome,
   :TRUNCATED, :STATES, and :PLAN when found."
  (setf *depth-cutoff* cutoff
        *solution-type* 'first)
  (multiple-value-bind (result found) (solve-search-checkpoint checkpoint goal)
    (list :cutoff cutoff
          :found found
          :status (search-outcome-status *last-search-outcome*)
          :reason (search-outcome-reason *last-search-outcome*)
          :truncated (and *depth-cutoff-truncated* t)
          :states *total-states-processed*
          :plan (when found
                  (goal-chain-cumulative-path
                    (goal-chain-session-phases (search-checkpoint-session result)))))))


(defun probe-deepen (checkpoint probe max-depth)
  "PROBE with its :LABEL and :RUNS added.  START when its goal holds at CHECKPOINT's
   endpoint; otherwise cutoffs 1 to MAX-DEPTH, stopping at the first run that finds a plan
   (CHEAP), or is not truncated (EXHAUSTED). Otherwise NOT-FOUND at MAX-DEPTH,
   including a false start goal at depth zero. State counts are measurements only."
  (if (probe-goal-holds-at-start-p (getf probe :goal) checkpoint)
    (append probe (list :label :start :runs nil))
    (let ((runs nil))
      (loop for cutoff from 1 to max-depth
            for run = (probe-run-once checkpoint (getf probe :goal) cutoff)
            do (push run runs)
               (format t "~&PB ~A  cutoff ~D  ~:[no plan~*~;plan of ~D~]  truncated ~A  states ~:D~%"
                       (getf probe :id) cutoff (getf run :found) (length (getf run :plan))
                       (getf run :truncated) (getf run :states))
            until (or (getf run :found)
                      (not (getf run :truncated))))
      (let ((last (first runs)))
        (append probe
                (list :label (cond ((getf last :found) :cheap)
                                   ((and last (not (getf last :truncated))) :exhausted)
                                   (t :not-found))
                      :runs (reverse runs)))))))


(defun write-probe-battery-results (pathname settings records)
  "Writes SETTINGS and the probe RECORDS to PATHNAME as one readable plist."
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*package* (find-package :ww))
            (*print-readably* nil))
        (prin1 (list :settings settings :probes records) stream)
        (terpri stream))))
  pathname)


;;;; Reporter (spec 12.5) ;;;;


(defparameter *probe-family-titles*
  '("landmark" "resource" "controller" "relay" "D's own")
  "The five PB family titles, in print order: P1-P4 of section 12.2 of the specification,
   and P5, D's own probes, of section 12.8.")


(defun read-probe-battery-results (pathname)
  "The plist WRITE-PROBE-BATTERY-RESULTS wrote to PATHNAME."
  (with-open-file (stream pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :ww)))
        (read stream)))))


(defun probe-label-text (record max-depth)
  "RECORD's label with its numbers and grade (spec 12.4), and the last run's states."
  (let ((last (car (last (getf record :runs)))))
    (format nil "~A~@[; states ~:D at the last cutoff~]"
            (ecase (getf record :label)
              (:start "START  [grade 1]")
              (:cheap (format nil "CHEAP ~D @ ~D  [grade 1 on replay]"
                              (length (getf last :plan)) (getf last :cutoff)))
              (:not-found (format nil "NOT FOUND <= ~D  [grade 3]" max-depth))
              (:exhausted (format nil "EXHAUSTED <= ~D  [grade 3]" (getf last :cutoff)))
              ;; Read-only compatibility with historical result files.
              (:stopped (format nil "LEGACY STOPPED at ~D  [none]" (getf last :cutoff))))
            (getf last :states))))


(defun report-probe-record (record max-depth)
  "One PB row: id, subject, goal, a P5 row's provenance, and label; a CHEAP row adds its
   plan, one action a line."
  (format t "    ~A  ~(~A  ~S~)~@[  <~A>~]  ~A~%"
          (getf record :id) (getf record :subject) (getf record :goal)
          (getf record :provenance) (probe-label-text record max-depth))
  (when (eq (getf record :label) :cheap)
    (dolist (step (getf (car (last (getf record :runs))) :plan))
      (format t "      ~(~S~)~%" step))))


(defun report-probe-battery-families (records max-depth)
  "The count line by label, then one block per family, NONE when a family is empty."
  (format t "~%  probes (~D):~{ ~D ~A~^,~}~%"
          (length records)
          (loop for (label text) in '((:start "START") (:cheap "CHEAP") (:not-found "NOT FOUND")
                                      (:exhausted "EXHAUSTED"))
                append (list (count label records :key (lambda (record) (getf record :label)))
                             text)))
  (let ((legacy-count (count :stopped records :key (lambda (record) (getf record :label)))))
    (when (plusp legacy-count)
      (format t "  historical rows: ~D LEGACY STOPPED (no bound claimed)~%" legacy-count)))
  (loop for title in *probe-family-titles*
        for family from 1
        for members = (remove-if-not (lambda (record) (= family (getf record :family))) records)
        do (format t "~%  P~D ~A (~D)~%" family title (length members))
           (unless members
             (format t "    none~%"))
           (dolist (record members)
             (report-probe-record record max-depth))))


(defun report-probe-battery-list (&optional (probes (probe-battery-probes)))
  "PROBES, by family, without searching (spec 12.2); by default the staged problem's P1-P4.
   Pass (append (probe-battery-probes) (d-probes ...)) to list D's own probes too."
  (let ((*print-pretty* nil))
    (format t "~2%PB  PROBE BATTERY -- probe list, no search~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  probes (~D)~%" (length probes))
    (loop for title in *probe-family-titles*
          for family from 1
          for members = (remove-if-not (lambda (probe) (= family (getf probe :family))) probes)
          do (format t "~%  P~D ~A (~D)~%" family title (length members))
             (unless members
               (format t "    none~%"))
             (dolist (probe members)
               (format t "    ~A  ~(~A  ~S~)~@[  <~A>~]~%" (getf probe :id) (getf probe :subject)
                       (getf probe :goal) (getf probe :provenance))))
    (values)))


(defun run-probe-battery (max-depth pathname &optional (probes (probe-battery-probes)))
  "PB's searches (spec 12.3): each of PROBES from the fresh staging's checkpoint, deepening
   to non-negative MAX-DEPTH. Requires *THREADS* 16. PATHNAME is
   rewritten after every probe, so a crash keeps the probes already finished.  The staged
   start, goal, undo stack, depth cutoff and solution type are restored however it ends."
  (check-type max-depth (integer 0 *))
  (unless (= *threads* 16)
    (error "The probe battery runs at *THREADS* 16: stage, then (ww-set *threads* 16)."))
  (let ((checkpoint (capture-search-checkpoint))
        (settings (list :problem *problem-name* :max-depth max-depth
                        :threads *threads* :solution-type 'first
                        :staged (loop for symbol in (append *goal-chain-setting-symbols*
                                                            '(*max-recorder-cycles*
                                                              *max-connector-pairings*))
                                      collect (cons symbol (symbol-value symbol)))))
        (depth-cutoff *depth-cutoff*)
        (solution-type *solution-type*)
        (undo-stack *undo-stack*)
        (records nil)
        (saved nil))
    (save-undo-checkpoint)
    (setf saved (pop *undo-stack*))
    (unwind-protect
        (dolist (probe probes)
          (push (probe-deepen checkpoint probe max-depth) records)
          (write-probe-battery-results pathname settings (reverse records)))
      (restore-undo-checkpoint saved)
      (setf *depth-cutoff* depth-cutoff
            *solution-type* solution-type
            *undo-stack* undo-stack))
    pathname))


(defun report-probe-battery (pathname)
  "PB, grade per row (spec 12.5), printed from the results file at PATHNAME; no search."
  (let* ((*print-pretty* nil)
         (results (read-probe-battery-results pathname))
         (settings (getf results :settings)))
    (format t "~2%PB  PROBE BATTERY  [grade per row]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  READING: each probe searches from the staged start for one subgoal, deepening one ~
               cutoff at a time.  CHEAP n @ d: a plan of n actions found at cutoff d, none below ~
               it; n is not claimed least.  NOT FOUND and EXHAUSTED are grade-3 cost bounds, never ~
               impossibility: pruning also limits the explored states.~%")
    (format t "  settings: problem ~(~A~), maximum depth ~D, threads ~D, solution ~
               type ~A (staged ~A)~%"
            (getf settings :problem) (getf settings :max-depth) (getf settings :threads)
            (getf settings :solution-type)
            (cdr (assoc '*solution-type* (getf settings :staged))))
    (report-probe-battery-families (getf results :probes) (getf settings :max-depth))
    (values)))
