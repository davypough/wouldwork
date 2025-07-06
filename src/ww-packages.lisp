(defpackage :utilities
  (:use :cl)
  (:nicknames :ut))

(defpackage :hstack
  (:use :cl)
  (:nicknames :hs))

(defpackage :wouldwork
  (:use :cl :iterate :sb-ext)
  (:nicknames :ww)
  (:shadowing-import-from :iterate)
  (:export #:main
           #:help
           #:run-test-problems
           #:run-all
           #:list-all
           #:run
          ;#:*test-problem-names*
           #:*problem-folder-paths*
	   #:get-src-folder-path
	   #:add-problem-folder
           #:remove-problem-folder
           #:save-globals
           #:read-globals
           #:*globals-file*
           #:*keep-globals-p*
           #:toggle-globals
           #:set-globals
           #:display-globals))

