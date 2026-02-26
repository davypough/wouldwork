#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

XDG_CACHE_HOME=/tmp sbcl --dynamic-space-size 24000 --eval "(progn
  (require :asdf)
  (pushnew #P\"${ROOT_DIR}/\" asdf:*central-registry* :test #'equal)
  (unwind-protect
      (progn
        (ql:quickload :wouldwork)
        (in-package :ww))
    (let ((pkg (find-package :ww)))
      (when pkg
        (let ((sym (find-symbol \"CLEANUP-RESOURCES\" pkg)))
          (when (and sym (fboundp sym))
            (funcall sym)))))))"
