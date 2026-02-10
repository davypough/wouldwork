#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

XDG_CACHE_HOME=/tmp sbcl --dynamic-space-size 24000 --eval "(progn
  (require :asdf)
  (pushnew #P\"${ROOT_DIR}/\" asdf:*central-registry* :test #'equal)
  (ql:quickload :wouldwork)
  (in-package :ww))"
