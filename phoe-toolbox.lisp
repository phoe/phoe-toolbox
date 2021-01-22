;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHOE-TOOLBOX
;;;; © Michał "phoe" Herda 2017
;;;; phoe-toolbox.lisp

(in-package #:phoe-toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(trivial-indent:define-indentation uiop:define-package (4 2 &rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-constructor ((class . keys) &body body)
  "Defines an INITIALIZE-INSTANCE :AFTER method on the given object."
  `(defmethod initialize-instance :after ((,class ,class)
                                          &key ,@keys &allow-other-keys)
     ,@body))

(defmacro define-print
    ((object stream &key (type t) (identity t)) &body body)
  "Defines a PRINT-OBJECT method on the given object with inserting BODY inside
PRINT-UNREADABLE-OBJECT."
  `(defmethod print-object ((,object ,object) ,stream)
     (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
       ,@body)))

(defmacro define-readable-print
    ((object stream &key (type t) (identity t)) &body body)
  "Defines a PRINT-OBJECT method on the given object, which depends on the value
of *PRINT-READABLY*. If it is true, then the object is printed using
PRINT-INSTANCE-READABLY; else, it is printed with inserting BODY inside
PRINT-UNREADABLE-OBJECT."
  `(defmethod print-object ((,object ,object) ,stream)
     (if *print-readably*
         (print-instance-readably ,object ,stream)
         (print-unreadable-object
             (,object ,stream :type ,type :identity ,identity)
           ,@body))))

(defmacro with-input-from-binary ((stream filespec) &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for reading from binary."
  `(with-open-file (,stream ,filespec :direction :input
                                      :if-does-not-exist :error
                                      :element-type '(unsigned-byte 8))
     ,@body))

(defmacro with-output-to-binary ((stream filespec
                                  &key (if-does-not-exist :create))
                                 &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for wriiting to binary."
  `(with-open-file (,stream ,filespec :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist ,if-does-not-exist
                                      :element-type '(unsigned-byte 8))
     ,@body))

(defmacro wait ((&optional (timeout 1) (step 0.001)) &body body)
  "Evaluates BODY each STEP seconds until it evaluates to true, at which point
it returns the value of BODY, or until TIMEOUT seconds pass, at which point it
returns NIL."
  (with-gensyms (begin-time end-time temp)
    `(let* ((units internal-time-units-per-second)
            (,begin-time (get-internal-real-time))
            (,end-time (+ ,begin-time (* ,timeout units))))
       (loop
         (let (,temp)
           (cond ((progn (setf ,temp (progn ,@body))
                         ,temp)
                  (return ,temp))
                 ((> (get-internal-real-time) ,end-time)
                  (return nil))
                 (t
                  (sleep ,step))))))))

(defmacro wait-until (form &optional (step 0.01))
  "Evaluates BODY each STEP seconds until it evaluates to true, at which point
it returns the value of BODY."
  (with-gensyms (result)
    `(loop for ,result = ,form
           if ,result return ,result
             else do (sleep ,step))))

(defmacro finalized-let* ((&rest bindings) &body body)
  "Like LET*, except each variable binding is of form (var initform . forms)
where FORMS will be evaluated when leaving the LET* by means of UNWIND-PROTECT.
These forms will be evaluated from last binding to first. Each binding has its
own individual UNWIND-PROTECT cleanup."
  (if bindings
      (destructuring-bind ((var &optional val &rest forms) . rest) bindings
        `(let ((,var ,val))
           (unwind-protect (finalized-let* ,rest ,@body) ,@forms)))
      `(progn ,@body)))

(defmacro with-temp-package (&body body)
  "Evaluates BODY with *PACKAGE* bound to a freshly created temporary package
that will be deleted once control leaves the BODY, uninterning all symbols that
were interned into it during that time."
  (let* ((now (format nil "~S" (get-internal-run-time)))
         (package-name (gensym (cat "TEMP-PKG-" now "-")))
         (package-var (gensym)))
    `(let ((,package-var (or (find-package ',package-name)
                             (make-package ',package-name :use nil))))
       (unwind-protect (let ((*package* ,package-var))
                         ,@body)
         (delete-package ,package-var)))))

(defmacro check-slot-boundp (object slot-name)
  "Asserts that the provided slot is bound."
  (let* ((name (string slot-name))
         (result (if (char= (aref name 0) #\%) (subseq name 1) name)))
    `(unless (slot-boundp ,object ',slot-name)
       (error "Must provide ~A." ,result))))

(defmacro fbind (bindings &body body)
  "Binds the function objects in the function namespace."
  (loop for (name function) in bindings
        for let-gensym = (gensym)
        for arg-gensym = (gensym)
        collect `(,let-gensym ,function)
          into let-bindings
        collect `(,name (&rest ,arg-gensym) (apply ,let-gensym ,arg-gensym))
          into flet-bindings
        collect name
          into declare-names
        finally (return `(let ,let-bindings
                           (flet ,flet-bindings
                             (declare (inline ,@declare-names))
                             ,@body)))))

(defmacro signals* (condition-type n form)
  "Testing macro analogous to 1AM:SIGNALS, except it does not perform a
non-local transfer of control and therefore is not suitable for any code that
makes calls to INVOKE-DEBUGGER. Instead, it allows the caller to assert that
a condition of type CONDITION-TYPE was signaled exactly N times."
  (check-type condition-type symbol)
  (check-type n (integer 1))
  (with-gensyms (count handler failer)
    (let ((error-msg-1 "Expected to signal ~A, but ~S was signaled instead.")
          (error-msg-2 "Expected to signal ~A, but got nothing.")
          (error-msg-3 "Expected to signal ~A once, but got it ~D times."))
      `(let* ((,count 0)
              (,failer (lambda (e) (error ,error-msg-1 ',condition-type e)))
              (,handler (lambda (e) (declare (ignore e)) (incf ,count))))
         (multiple-value-prog1
             (handler-bind ((,condition-type ,handler)
                            ((and condition (not ,condition-type)) ,failer))
               ,form)
           (when (= 0 ,count)
             (error ,error-msg-2 ',condition-type))
           (unless (= 1 ,n)
             (error ,error-msg-3 ',condition-type ,count)))))))

(defmacro handler-case* (form &rest cases)
  "A variant of HANDLER-CASE, in which the case forms are evaluating before
performing a transfer of control. This ensures that the case forms are evaluated
in the dynamic scope of the signaling form."
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-case*-without-no-error-case form cases))
      (1 (make-handler-case*-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE*.")))))

(defun make-handler-case*-with-no-error-case (form cases)
  (let* ((no-error-case (assoc :no-error cases))
         (other-cases (remove no-error-case cases)))
    (let ((normal-return (gensym "NORMAL-RETURN"))
          (error-return  (gensym "ERROR-RETURN")))
      `(block ,error-return
         (multiple-value-call (lambda ,@(cdr no-error-case))
           (block ,normal-return
             (return-from ,error-return
               (handler-case* (return-from ,normal-return ,form)
                 ,@other-cases))))))))

(defun make-handler-case*-without-no-error-case (form cases)
  (let ((block-name (gensym "HANDLER-CASE*-BLOCK")))
    (flet ((make-handler-bind-case (case)
             (destructuring-bind (type lambda-list . body) case
               `(,type (lambda ,lambda-list
                         (return-from ,block-name (locally ,@body)))))))
      (let ((bindings (mapcar #'make-handler-bind-case cases)))
        `(block ,block-name (handler-bind ,bindings ,form))))))

(define-modify-macro notf () not
  "Sets the value of PLACE to its logical negation and returns the new value.")

;;; The following implementations of MOD-INCF and MOD-DECF have been adapted
;;; from SICL by Robert Strandh and are subject to the following license:

;;; Copyright (c) 2010 - 2016
;;;
;;;     Robert Strandh (robert.strandh@gmail.com)
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the following
;;; conditions are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials
;;;    provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defmacro mod-incf (&environment env place divisor &optional (delta-form 1))
  "Like INCF, except the final value set in PLACE is always a modulus modulo
DIVISOR."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (if (null vars)
        `(let ((,(first store-vars)
                 (mod (+ ,reader-form ,delta-form) ,divisor)))
           ,writer-form)
        `(let* ((,(first vars) ,(first vals))
                (,(first store-vars)
                  (mod (+ ,reader-form ,delta-form) ,divisor)))
           ,writer-form))))

(defmacro mod-decf (&environment env place divisor &optional (delta-form 1))
  "Like DECF, except the final value set in PLACE is always a modulus modulo
DIVISOR."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (if (null vars)
        `(let ((,(first store-vars)
                 (mod (- ,reader-form ,delta-form) ,divisor)))
           ,writer-form)
        `(let* ((,(first vars) ,(first vals))
                (,(first store-vars)
                  (mod (- ,reader-form ,delta-form) ,divisor)))
           ,writer-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun count-digits (integer)
  "Returns the number of digits in an integer, sans any sign."
  (check-type integer integer)
  (if (= 0 integer)
      1
      (values (ceiling (log (1+ (abs integer)) 10)))))

(defun cat (&rest strings)
  "Concatenates targets into a string."
  (apply #'concatenate 'string strings))

(defun catn (&rest strings)
  "Concatenates targets into a string, inserting a newline between each of
them."
  (let ((strings (loop for cons on strings collect (car cons)
                       when (cdr cons) collect #.(format nil "~%"))))
    (apply #'concatenate 'string strings)))

(defun zip (&rest lists)
  "Collects a list of lists, where the first list contains the first element of
the argument lists, the second list - second, etc.. The lists are as long as the
shortest list."
  (apply #'mapcar #'list lists))

(defun bound-slot-names (instance)
  "Given a generalized instance of STANDARD-OBJECT, returns a list of all slot
names which are bound in that instance."
  (check-type instance standard-object)
  (loop for slot in (c2mop:class-direct-slots (class-of instance))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp instance name)
          collect name))

(defun vector-times (vector n)
  "Returns a fresh vector which is VECTOR concatenated to itself N times."
  (let* ((length (length vector))
         (result (make-array (* length n)
                             :element-type (array-element-type vector))))
    (loop for i from 0 upto (* length n) by length
          do (replace result vector :start1 i)
          finally (return result))))

(defun rassoc-value-or-die (alist key &key (test 'eql))
  "Like ALEXANDRIA:RASSOC-VALUE, except it signals an error if the value is
not found."
  (multiple-value-bind (value foundp)
      (rassoc-value alist key :test test)
    (if foundp value
        (error "RASSOC of ~S was not found in ~S." key alist))))

(defun assoc-value-or-die (alist key &key (test 'eql))
  "Like ALEXANDRIA:ASSOC-VALUE, except it signals an error if the value is
not found."
  (multiple-value-bind (value foundp) (assoc-value alist key :test test)
    (if foundp value
        (error "ASSOC of ~S was not found in ~S." key alist))))

(defun gethash-or-die (key hash-table &optional default)
  "Like GETHASH, except it signals an error if the value is not found."
  (multiple-value-bind (value foundp) (gethash key hash-table default)
    (if foundp value
        (error "GETHASH of ~S was not found in ~S." key hash-table))))

(defun read-data-file (system pathname)
  "Reads the data file from the provided pathname. The pathname should be
a system relative pathname."
  (let ((full-pathname (asdf:system-relative-pathname system pathname)))
    (with-input-from-file (stream full-pathname) (read stream))))

;;; The following function, ROBUST-SUBSEQ, was taken from
;;; https://github.com/death/gnusdumps and is MIT-licensed.

(defun robust-subseq (sequence start &optional end)
  "Like SUBSEQ, but handles out-of-range bounding index designators
gracefully."
  (let* ((length (length sequence))
         (start (max 0 (min start length)))
         (end (max 0 start (min length (or end length)))))
    (subseq sequence start end)))

(defvar *whitespace*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout)
  "Standard whitespace characters.")

(defun trim-whitespace (string)
  "Trims whitespace characters from both sides of a string."
  (string-trim *whitespace* string))

(defun hexadecimal-string-p (string)
  "Returns true if the string contains only digits 0-9 and lowercase/uppercase
characters #\A-#\F, false otherwise."
  (let ((chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                 #\a #\b #\c #\d #\e #\f
                 #\A #\B #\C #\D #\E #\F)))
    (loop for char across string
          always (member char chars))))

(defun sleepcar (function list &optional (n 100) (sleep-interval 1))
  "Collects the element of funcalling FUNCTION on successive elements of LIST,
sleeping for SLEEP-INTERVAL seconds every N elements."
  (loop for elt in list
        for i from 1
        collect (funcall function elt)
        when (zerop (mod i n))
          do (sleep sleep-interval)))

(defun fformat (stream format-string &rest format-args)
  "Acts like FORMAT, except it calls FORCE-OUTPUT on STREAM afterwards."
  (apply #'format stream format-string format-args)
  (force-output stream))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defconstant +universal-unix-time-difference+ 2208988800
  "The difference between universal time and Unix time in seconds.")

(defun get-unix-time ()
  "Returns the current Unix timestamp."
  (- (get-universal-time) +universal-unix-time-difference+))

(defun unix-time-to-datestring (unix-time)
  "Decodes the unix time and returns its textual form in format
\"YYYY-MM-DD HH:MM:SS\"."
  (let* ((universal-time (+ unix-time +universal-unix-time-difference+))
         (time (multiple-value-list (decode-universal-time universal-time))))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            (nth 5 time) (nth 4 time) (nth 3 time)
            (nth 2 time) (nth 1 time) (nth 0 time))))

(defun string=-getf (plist indicator)
  "Like GETF, except it tests with STRING= and is there suitable for use
with strings and symbols."
  (loop for (key value) on plist by #'cddr
        when (and (string= key indicator))
          return value))

(defun peek-char-no-hang (&optional (input-stream *standard-input*)
                            (eof-error-p t) eof-value recursive-p)
  "Like PEEK-CHAR, except it immediately returns NIL if no character is
available for reading on the input stream."
  (let ((character (read-char-no-hang input-stream eof-error-p
                                      eof-value recursive-p)))
    (when (and character (not (eql character eof-value)))
      (unread-char character input-stream))
    character))

(defun unintern-all-symbols (tree)
  "Returns a copy of the provided tree, in which all symbols have been
uninterned. The tree must not contain improper lists."
  (cond ((consp tree)
         (mapcar #'unintern-all-symbols tree))
        ((symbolp tree)
         (make-symbol (symbol-name tree)))
        (t
         tree)))

(defun whitespacep (char)
  "Returns true if the provided character is whitespace."
  (check-type char character)
  (member char *whitespace*))

(defgeneric print-instance-readably (object &optional stream)
  (:documentation
   "Prints an instance readably, using the #. notation.
\
The standard method is a hack. Its functioning depends on all direct slots of a
class being of form %FOO or FOO and having an initarg keyword called :FOO. All
values stored in these slots need to be printable readably. Moreover, indirect
slots are not restored. Before you use it, make sure you know what you are
doing.
\
The hash-table method uses ALEXANDRIA:ALIST-HASH-TABLE.
\
Programmers may define methods on this generic function in case the standard
method is not enough to print the object they have.")
  (:method (object &optional (stream *standard-output*))
    (check-type object standard-object)
    (flet ((internal-slot-keyword (symbol)
             (let* ((string (string symbol))
                    (percentp (eql (aref string 0) #\%))
                    (result (if percentp (subseq (string symbol) 1) string)))
               (values (intern result :keyword)))))
      (let* ((class (class-name (class-of object)))
             (slots (bound-slot-names object))
             (values (mapcar (curry #'slot-value object) slots))
             (keywords (mapcar #'internal-slot-keyword slots))
             (plist (apply #'nconc (zip keywords values)))
             (result `(make-instance ',class ,@plist)))
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (format stream "#.~S" result))))))
  (:method ((hash-table hash-table) &optional (stream *standard-output*))
    (let ((test (hash-table-test hash-table))
          (*print-circle* t)
          (*print-readably* t))
      (format stream "#.(~A:~A '(~%" :alexandria :alist-hash-table)
      (maphash (lambda (k v) (format stream "   (~S . ~S)~%" k v)) hash-table)
      (format stream "   ) :TEST '~S)" test)
      hash-table)))

(defun print-hash-table-readably (hash-table
                                  &optional (stream *standard-output*))
  "Prints a hash table readably.
\
Deprecated: please use (PRINT-INSTANCE-READABLY HASH-TABLE &OPTIONAL STREAM)
instead."
  (print-instance-readably hash-table stream))

(defun multiple-value-mapcar (function &rest lists)
  "Returns multiple lists of all multiple values returned by repeatedly
applying FUNCTION to consecutive arguments from LISTS."
  (assert (not (null lists)))
  (let* ((values (loop for l = lists then (mapcar #'cdr l)
                       until (some #'endp l)
                       collecting (multiple-value-list
                                   (apply function (mapcar #'car l)))))
         (max-values (loop for vl in values maximizing (length vl)))
         (lists (make-list max-values)))
    (loop for vl in values
          do (loop for i from 0 below max-values
                   do (push (nth i vl) (nth i lists))))
    (values-list (mapcar #'nreverse lists))))

(defun urls-pathnames (urls directory)
  "Provided a list of URLs and a valid directory pathname, returns a list of
pathnames that end with the filenames suitable for downloaded files."
  (let* ((filenames (mapcar #'url-filename urls))
         (pathnames (mapcar (rcurry #'merge-pathnames directory) filenames)))
    pathnames))

(defun url-filename (url)
  "Given a URL, returns everything after its last slash."
  (assert (stringp url))
  (let* ((position (position #\/ url :from-end t)))
    (if position
        (subseq url (1+ position))
        "")))

(defun dsubseq (sequence start &optional end)
  "Like SUBSEQ, except the created array is an array displaced to SEQUENCE ~
and therefore shares structure with it."
  (unless end
    (setf end (array-total-size sequence)))
  (make-array (- end start) :displaced-to sequence
                            :displaced-index-offset start))

(defun nth-funcall (function count argument)
  "Funcall FUNCTION composed COUNT times with itself on ARGUMENT."
  (loop repeat (1+ count)
        for result = argument then (funcall function result)
        finally (return result)))

(defun constantly* (&rest args)
  "Like CONSTANTLY, except the returned closure may return multiple values,
denoted by ARGS."
  (lambda (&rest lambda-args)
    (declare (ignore lambda-args))
    (values-list args)))

(defun identity* (&rest args)
  "Like CONSTANTLY, except the list of ARGS is returned instead."
  args)

(defun graph-roots (edges)
  "Accepts a list of directed edges (two-element lists of EQL-comparable nodes).
Returns a list of root vertices (vertices that are not pointed at)."
  (let* ((vertices (remove-duplicates (mapcar #'first edges)))
         (starts (mapcar (rcurry #'cons t) vertices)))
    (loop with hash-table = (alist-hash-table starts)
          for end in (mapcar #'second edges)
          do (remhash end hash-table)
          finally (return (hash-table-keys hash-table)))))

(defun circular-graph-p (edges)
  "Accepts a list of directed edges (two-element lists of EQL-comparable nodes).
If the graph does not contain a cycle, returns (VALUES NIL NIL). If the graph
contains a cycle, returns (VALUES T VERTEX), where VERTEX is one of the vertices
in the cycle."
  (flet ((die (vertex) (return-from circular-graph-p (values t vertex))))
    (let* ((stack (graph-roots edges))
           (visited (make-hash-table)))
      (when (null stack) (die (caar edges)))
      (do ((vertex (pop stack) (pop stack)))
          ((and (null vertex) (null stack)))
        (when (gethash vertex visited) (die vertex))
        (setf (gethash vertex visited) t)
        (let ((new (remove-if-not (curry #'eq vertex) edges :key #'first)))
          (unionf stack (mapcar #'second new))))
      (values nil nil))))

(defun split (predicate list)
  "Separates the list into a sublist of elements for which the predicate returns
true and a sublist of elements for which the predicate returns false. Returns
them as two values."
  (loop for elt in list
        if (funcall predicate elt)
          collect elt into true
        else collect elt into false
        finally (return (values true false))))

(defun make-dumped-stream
    (input-output-stream &key dump-input-stream dump-output-stream)
  "Returns a wrapper stream around the original stream. All data read from the
wrapper stream is additionally sent to DUMP-INPUT-STREAM. All data written to
the wrapper stream is additionally sent to the DUMP-OUTPUT-STREAM."
  (if (or dump-input-stream dump-output-stream)
      (make-two-way-stream
       (if dump-input-stream
           (make-echo-stream input-output-stream dump-input-stream)
           input-output-stream)
       (if dump-output-stream
           (make-broadcast-stream input-output-stream dump-output-stream)
           input-output-stream))
      input-output-stream))

(defun alternatingly (value-when-true &optional
                                        (value-when-false nil)
                                        (initially-true-p nil))
  "Returns a function that acts like CL:CONSTANTLY, but its return value
alternates between VALUE-WHEN-TRUE and VALUE-WHEN-FALSE (default NIL).
INITIALLY-TRUE-P specifies whether VALUE-WHEN-TRUE is returned as the first
value from the returned function."
  (let ((x (not initially-true-p)))
    (lambda (&rest rest)
      (declare (ignore rest))
      (if (notf x) value-when-true value-when-false))))

(defun case-lisp-file (pathname case-function)
  (with-input-from-file (input pathname)
    (with-output-to-string (output)
      (loop for char = (read-char input nil nil)
            while char
            do (case char
                 (#\#
                  (princ char output)
                  (let ((next-char (read-char input)))
                    (case next-char
                      (#\\
                       (princ next-char output)
                       (princ (read-char input) output))
                      (t (unread-char next-char input)))))
                 (#\;
                  (unread-char char input)
                  (princ (read-line input) output)
                  (terpri output))
                 ((#\" #\|)
                  (unread-char char input)
                  (prin1 (read input) output))
                 (t (write-char (funcall case-function char) output)))))))

(defun upcase-lisp-file (pathname)
  "Upcases a Common Lisp source file."
  (case-lisp-file pathname #'char-upcase))

(defun downcase-lisp-file (pathname)
  "Downcases a Common Lisp source file."
  (case-lisp-file pathname #'char-downcase))

(defun shallow-copy-object (original &rest initargs)
  "Creates a shallow copy of a standard object, copying the values of all
slots, before calling REINITIALIZE-INSTANCE on it with the provided
keyword-value pairs."
  (let* ((class (class-of original))
         (copy (allocate-instance class))
         (slots (c2mop:class-slots class))
         (slot-names (mapcar #'c2mop:slot-definition-name slots)))
    (dolist (slot slot-names)
      (when (slot-boundp original slot)
        (setf (slot-value copy slot) (slot-value original slot))))
    (apply #'reinitialize-instance copy initargs)))

(defpackage #:phoe-toolbox/list-of (:use))

(deftype list-of (type)
  "A type specifier that matches a list whose all elements are of the given
type."
  (check-type type symbol) ; Kludge - maybe we'll fix that later.
  (let* ((package-name (package-name (symbol-package type)))
         (symbol-name (symbol-name type))
         (predicate-name (format nil "LIST-OF ~S ~S" package-name symbol-name))
         (package (find-package '#:phoe-toolbox/list-of))
         (predicate (intern predicate-name package)))
    (setf (fdefinition predicate)
          (lambda (list) (every (rcurry #'typep type) list)))
    `(satisfies ,predicate)))

(defvar *call-with-handler-cache* (make-hash-table :test #'equal))

(defun ensure-call-with-handler-function (condition-type)
  (multiple-value-bind (value foundp)
      (gethash condition-type *call-with-handler-cache*)
    (if foundp
        value
        (let ((lambda-form
                `(lambda (handler-function thunk)
                   (handler-bind ((,condition-type handler-function))
                     (funcall thunk)))))
          (setf (gethash condition-type *call-with-handler-cache*)
                (coerce lambda-form 'function))))))

(defun call-with-handler (thunk condition-type handler-function)
  "The functional variant of HANDLER-BIND."
  (funcall (ensure-call-with-handler-function condition-type)
           handler-function thunk))

(defvar *call-with-restart-cache* (make-hash-table :test #'equal))

(defun ensure-call-with-restart-function
    (restart-name interactive-p report-p test-p)
  (let ((key (list restart-name interactive-p report-p test-p)))
    (multiple-value-bind (value foundp) (gethash key *call-with-restart-cache*)
      (if foundp
          value
          (let ((lambda-form
                  `(lambda (restart-function thunk interactive report test)
                     (declare (ignorable interactive report test))
                     (restart-bind
                         ((,restart-name
                            restart-function
                            ,@(when interactive-p
                                `(:interactive-function interactive))
                            ,@(when report-p `(:report-function report))
                            ,@(when test-p `(:test-function test))))
                       (funcall thunk)))))
            (setf (gethash key *call-with-restart-cache*)
                  (coerce lambda-form 'function)))))))

(defun call-with-restart (thunk restart-name restart-function
                          &key (interactive-function nil interactive-p)
                            (report-function nil report-p)
                            (test-function nil test-p))
  "The functional variant of RESTART-BIND."
  (let ((function (ensure-call-with-restart-function
                   restart-name
                   (and interactive-p t) (and report-p t) (and test-p t))))
    (funcall function restart-function thunk
             interactive-function report-function test-function)))
