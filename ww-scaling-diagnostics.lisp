;;; REPL-only diagnostics for parallel scaling of SBCL on this machine.
;;; Load explicitly, then enter (run-scaling-diagnostics), which is independent of
;;; wouldwork search, or (profile-search-consing depth-cutoff) in serial mode, which
;;; profiles per-function consing of the staged problem, or (profile-search-calls depth-cutoff)
;;; in serial mode, which reports calls and exclusive time per function per state.
(in-package :ww)


(sb-ext:defglobal **diag-counter** 0)
(declaim (type fixnum **diag-counter**))


(defvar *diag-table* nil
  "Shared hash table read by DIAG-HASH-READ-WORK.")


(defparameter *diag-thread-counts* '(1 2 4 8 16 24 32)
  "Thread counts for each scaling test. The first entry is the speedup baseline.")


(defun run-scaling-diagnostics ()
  "Run six strong-scaling tests, each splitting a fixed workload across
   *DIAG-THREAD-COUNTS* threads: pure CPU, allocation with the default nursery,
   allocation with a larger nursery, synchronized and unsynchronized hash-table
   reads, and a shared atomic counter. Comparing their speedup curves separates
   hardware limits from GC, lock, and cache-contention limits."
  (report-diagnostic-system-info)
  (diag-scaling "1. Pure CPU, no allocation" #'diag-cpu-work 4000000000)
  (diag-scaling "2. Allocation-heavy, default nursery" #'diag-alloc-work 200000000)
  (let ((entry-nursery (sb-ext:bytes-consed-between-gcs))
        (large-nursery (floor (sb-ext:dynamic-space-size) 8)))
    (unwind-protect
        (progn
          (setf (sb-ext:bytes-consed-between-gcs) large-nursery)
          (diag-scaling (format nil "3. Allocation-heavy, ~:D MB nursery"
                                (floor large-nursery (* 1024 1024)))
                        #'diag-alloc-work 200000000))
      (setf (sb-ext:bytes-consed-between-gcs) entry-nursery)))
  (setf *diag-table* (make-diag-table t))
  (diag-scaling "4. Synchronized hash-table reads" #'diag-hash-read-work 40000000)
  (setf *diag-table* (make-diag-table nil))
  (diag-scaling "5. Unsynchronized hash-table reads" #'diag-hash-read-work 40000000)
  (setf *diag-table* nil)
  (diag-scaling "6. Shared atomic counter" #'diag-atomic-counter-work 200000000)
  (values))


(defun profile-search-consing (depth-cutoff)
  "Profile per-function consing for one silent serial SOLVE of the staged problem at
   DEPTH-CUTOFF, with *SOLUTION-TYPE* EVERY and *RANDOMIZE-SEARCH* off (as in
   TEST-THREADS). Uses SB-PROFILE on every function in the WOULDWORK package, whose
   consing is exclusive of nested profiled calls. Prints the 40 largest consers with
   percent of total, calls, and bytes per call. Requires *THREADS* 0, since SB-PROFILE
   measures consing with the process-wide byte counter. Parameters are restored and
   all functions unprofiled on exit."
  (assert (zerop *threads*) ()
    "Enter (ww-set *threads* 0) before calling PROFILE-SEARCH-CONSING.")
  (let ((entry-solution-type *solution-type*)
        (entry-depth-cutoff *depth-cutoff*)
        (entry-randomize-search *randomize-search*)
        (seconds 0.0)
        (states 0)
        (entries nil))
    (unwind-protect
        (progn
          (setf *solution-type* 'every
                *depth-cutoff* depth-cutoff
                *randomize-search* nil)
          (sb-profile:unprofile)
          (eval `(sb-profile:profile ,(package-name (find-package :ww))))
          (sb-profile:reset)
          (multiple-value-setq (seconds states) (timed-silent-solve))
          (setf entries (collect-profile-consing)))
      (sb-profile:unprofile)
      (setf *solution-type* entry-solution-type
            *depth-cutoff* entry-depth-cutoff
            *randomize-search* entry-randomize-search))
    (let ((total (reduce #'+ entries :key #'second)))
      (format t "~2&Consing profile: ~A, serial, depth-cutoff ~D~%" *problem-name* depth-cutoff)
      (format t "  ~,2F sec (profiled), ~:D states, ~:D MB consed, ~:D bytes/state~2%"
              seconds states (floor total (* 1024 1024)) (round total states))
      (format t "  ~15@A  ~6@A  ~13@A  ~11@A  ~A~%" "Bytes" "%" "Calls" "Bytes/call" "Function")
      (loop for (name consing calls) in entries
            repeat 40
            do (format t "  ~15:D  ~5,1F%  ~13:D  ~11:D  ~S~%"
                       consing (/ (* 100.0 consing) total) calls (round consing calls) name)))
    (values)))


(defun profile-search-calls (depth-cutoff)
  "Count calls per function for one silent serial SOLVE of the staged problem at
   DEPTH-CUTOFF, with *SOLUTION-TYPE* EVERY and *RANDOMIZE-SEARCH* off (as in
   TEST-THREADS). Uses SB-PROFILE on every function in the WOULDWORK package. Prints
   the 60 functions with the most exclusive run time (excluding nested profiled calls),
   with calls per state and microseconds per state. Per-call profiling overhead inflates
   tiny, frequently called functions, so compare functions of similar call counts.
   Requires *THREADS* 0. Parameters are restored and all functions unprofiled on exit."
  (assert (zerop *threads*) ()
    "Enter (ww-set *threads* 0) before calling PROFILE-SEARCH-CALLS.")
  (let ((entry-solution-type *solution-type*)
        (entry-depth-cutoff *depth-cutoff*)
        (entry-randomize-search *randomize-search*)
        (seconds 0.0)
        (states 0)
        (entries nil))
    (unwind-protect
        (progn
          (setf *solution-type* 'every
                *depth-cutoff* depth-cutoff
                *randomize-search* nil)
          (sb-profile:unprofile)
          (eval `(sb-profile:profile ,(package-name (find-package :ww))))
          (sb-profile:reset)
          (multiple-value-setq (seconds states) (timed-silent-solve))
          (setf entries (sort (collect-profile-calls) #'> :key #'fourth)))
      (sb-profile:unprofile)
      (setf *solution-type* entry-solution-type
            *depth-cutoff* entry-depth-cutoff
            *randomize-search* entry-randomize-search))
    (format t "~2&Call profile: ~A, serial, depth-cutoff ~D~%" *problem-name* depth-cutoff)
    (format t "  ~,2F sec (profiled), ~:D states~2%" seconds states)
    (format t "  ~13@A  ~11@A  ~9@A  ~9@A  ~A~%" "Calls" "Calls/state" "Seconds" "us/state" "Function")
    (loop for (name nil calls ticks) in entries
          repeat 60
          do (format t "  ~13:D  ~11,2F  ~9,3F  ~9,2F  ~S~%"
                     calls (/ calls (float states))
                     (/ ticks (float internal-time-units-per-second))
                     (/ (* 1000000.0 ticks) internal-time-units-per-second states)
                     name))
    (values)))


(defun collect-profile-consing ()
  "Return a list of (name consing calls) for every profiled function that was called,
   sorted by decreasing consing. Reads SB-PROFILE's internal statistics table."
  (let ((entries nil))
    (maphash (lambda (name info)
               (multiple-value-bind (calls ticks consing)
                   (funcall (sb-profile::profile-info-read-stats-fun info))
                 (declare (ignore ticks))
                 (when (plusp calls)
                   (push (list name consing calls) entries))))
             sb-profile::*profiled-fun-name->info*)
    (sort entries #'> :key #'second)))


(defun collect-profile-calls ()
  "Return a list of (name consing calls ticks) for every profiled function that was
   called. Reads SB-PROFILE's internal statistics table; TICKS are exclusive internal
   run-time units."
  (let ((entries nil))
    (maphash (lambda (name info)
               (multiple-value-bind (calls ticks consing)
                   (funcall (sb-profile::profile-info-read-stats-fun info))
                 (when (plusp calls)
                   (push (list name consing calls ticks) entries))))
             sb-profile::*profiled-fun-name->info*)
    entries))


(defun report-diagnostic-system-info ()
  "Print the SBCL build, platform, heap, nursery, and threading features."
  (format t "~&SBCL ~A on ~A ~A~%"
          (lisp-implementation-version) (software-type) (software-version))
  (format t "Machine: ~A~%" (machine-version))
  (format t "Dynamic space: ~:D MB~%" (floor (sb-ext:dynamic-space-size) (* 1024 1024)))
  (format t "Nursery (bytes-consed-between-gcs): ~:D MB~%"
          (floor (sb-ext:bytes-consed-between-gcs) (* 1024 1024)))
  (format t "Features: sb-thread ~A, sb-safepoint ~A~%"
          (not (null (member :sb-thread *features*)))
          (not (null (member :sb-safepoint *features*))))
  (finish-output))


(defun diag-scaling (label work-function total-units)
  "Run WORK-FUNCTION with TOTAL-UNITS split evenly across each thread count in
   *DIAG-THREAD-COUNTS*, reporting wall seconds, speedup and efficiency relative
   to the first thread count, and GC seconds."
  (format t "~2&~A  (~:D units total)~%" label total-units)
  (format t "  Threads   Seconds   Speedup   Efficiency    GC-sec~%")
  (let ((base-n (first *diag-thread-counts*))
        (base-seconds nil))
    (dolist (n *diag-thread-counts*)
      (sb-ext:gc :full t)
      (multiple-value-bind (seconds gc-seconds)
          (diag-timed-threads work-function (floor total-units n) n)
        (unless base-seconds
          (setf base-seconds seconds))
        (format t "  ~7D  ~8,2F  ~8,2F  ~10,1F%  ~8,2F~%"
                n seconds (/ base-seconds seconds)
                (/ (* 100 base-seconds base-n) (* seconds n)) gc-seconds)
        (finish-output)))))


(defun diag-timed-threads (work-function units-per-thread n)
  "Run WORK-FUNCTION on UNITS-PER-THREAD in each of N threads and join them.
   Returns (values wall-seconds gc-seconds)."
  (let* ((gc-start sb-ext:*gc-run-time*)
         (start (get-internal-real-time))
         (threads (loop repeat n
                        collect (sb-thread:make-thread
                                  (lambda () (funcall work-function units-per-thread))))))
    (mapc #'sb-thread:join-thread threads)
    (values (/ (- (get-internal-real-time) start) (float internal-time-units-per-second))
            (/ (- sb-ext:*gc-run-time* gc-start) (float internal-time-units-per-second)))))


(defun diag-cpu-work (count)
  "Integer LCG loop: CPU only, no allocation, no shared memory."
  (declare (optimize (speed 3) (safety 0)) (type fixnum count))
  (let ((x 1))
    (declare (type (unsigned-byte 32) x))
    (dotimes (i count x)
      (setf x (logand #xFFFFFFFF (+ (* x 1664525) 1013904223))))))


(defun diag-alloc-work (count)
  "Allocate COUNT short-lived 64-byte vectors, retaining only the latest 1024."
  (declare (optimize (speed 3) (safety 0)) (type fixnum count))
  (let ((ring (make-array 1024)))
    (dotimes (i count ring)
      (setf (svref ring (logand i 1023)) (make-array 6 :initial-element i)))))


(defun make-diag-table (synchronized)
  "Build a 16384-entry fixnum-to-fixnum EQL hash table."
  (let ((table (make-hash-table :test 'eql :size 16384 :synchronized synchronized)))
    (dotimes (i 16384 table)
      (setf (gethash i table) i))))


(defun diag-hash-read-work (count)
  "Perform COUNT GETHASH reads on the shared *DIAG-TABLE*."
  (declare (optimize (speed 3) (safety 0)) (type fixnum count))
  (let ((table *diag-table*)
        (sum 0))
    (declare (type hash-table table) (type fixnum sum))
    (dotimes (i count sum)
      (setf sum (logand most-positive-fixnum
                        (+ sum (the fixnum (gethash (logand i 16383) table))))))))


(defun diag-atomic-counter-work (count)
  "Perform COUNT ATOMIC-INCFs on one shared global counter."
  (declare (optimize (speed 3) (safety 0)) (type fixnum count))
  (dotimes (i count)
    (sb-ext:atomic-incf **diag-counter**)))
