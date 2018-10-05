;;;; package.lisp

(defpackage #:phoe-toolbox
  (:use #:cl
        #:alexandria)
  (:export
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
   #:check-boundp
   #:fbind
   #:signals*
   ;; Functions
   #:count-digits
   #:cat
   #:catn
   #:zip
   #:bound-slots-values
   #:vector-times
   #:assoc-value-or-die
   #:rassoc-value-or-die
   #:print-hash-table-readably
   #:read-data-file
   #:robust-subseq
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
   ;; Modify macros
   #:mod-incf
   #:mod-decf
   #:notf))
