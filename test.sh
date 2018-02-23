#/bin/sh

sbcl --non-interactive \
     --eval '(ql:quickload :cl-selenium)' \
     --eval '(asdf:test-system :cl-selenium)'
