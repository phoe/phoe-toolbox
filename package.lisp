;;;; package.lisp

(defpackage #:phoe-toolbox
  (:use #:cl
        #:alexandria)
  (:export
   ;; Constants
   #:+universal-unix-time-difference+
   ;; Macros
   #:define-constructor
   #:define-print
   #:define-readable-print
   #:with-input-from-binary
   #:with-output-to-binary
   #:wait
   #:wait-until
   #:finalized-let*
   #:with-temp-package
   #:check-slot-boundp
   #:fbind
   #:signals*
   #:handler-case*
   ;; Modify macros
   #:mod-incf
   #:mod-decf
   #:notf
   ;; Functions
   #:count-digits
   #:cat
   #:catn
   #:zip
   #:bound-slot-names
   #:vector-times
   #:assoc-value-or-die
   #:rassoc-value-or-die
   #:print-hash-table-readably
   #:read-data-file
   #:robust-subseq
   #:*whitespace*
   #:trim-whitespace
   #:hexadecimal-string-p
   #:sleepcar
   #:fformat
   #:replace-all
   #:get-unix-time
   #:unix-time-to-datestring
   #:keywordize
   #:string=-getf
   #:peek-char-no-hang
   #:unintern-all-symbols
   #:whitespacep
   #:print-instance-readably
   #:multiple-value-mapcar
   #:urls-pathnames
   #:url-filename
   #:dsubseq
   #:gethash-or-die
   #:nth-funcall
   #:constantly*
   #:identity*
   #:graph-roots
   #:circular-graph-p
   #:split
   #:make-dumped-stream
   #:alternatingly
   #:downcase-lisp-file
   #:upcase-lisp-file
   #:shallow-copy-object))
