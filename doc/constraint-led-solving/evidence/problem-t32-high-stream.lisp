;;; Isolated T32 wall-stream fixture. No solve; call the sweep on a copied state.
(in-package :ww)
(ww-set *problem-name* t32-high-stream)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 0)
(define-types
  agent (observer)
  box (low-box high-box)
  wall-blower (high-drive)
  location (source target safe))
(include-tech box)
(include-tech wall-blower)
(define-init
  (has-location observer safe)
  (has-location low-box source)
  (has-location high-box source)
  (on high-box low-box)
  (has-position high-drive source)
  (aimed-at high-drive target)
  (has-elevation high-drive 2))
;;; No init propagation: the copied test state exercises the sweep explicitly.
(define-goal (has-location high-box target))
