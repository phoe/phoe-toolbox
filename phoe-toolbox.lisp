;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHOE-TOOLBOX
;;;; © Michał "phoe" Herda 2017
;;;; phoe-toolbox.lisp

(in-package #:phoe-toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-constructor ((class . keys) &body body)
  "Defines an INITIALIZE-INSTANCE :AFTER method on the given object."
  `(defmethod initialize-instance :after ((,class ,class)
                                          &key ,@keys &allow-other-keys)
     ,@body))

(defmacro define-print ((object stream) &body body)
  "Defines a PRINT-OBJECT method on the given object with
PRINT-UNREADABLE-OBJECT."
  `(defmethod print-object ((,object ,object) ,stream)
     (print-unreadable-object (,object ,stream :type t :identity t)
       ,@body)))

(defmacro with-input-from-binary ((stream filespec) &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for reading from binary."
  `(with-open-file (,stream ,filespec :direction :input
                                      :if-does-not-exist :error
                                      :element-type 'octet)
     ,@body))

(defmacro with-output-to-binary ((stream filespec) &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for wriiting to binary."
  `(with-open-file (,stream ,filespec :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :element-type 'octet)
     ,@body))

(defmacro wait ((&optional (timeout 2) (step 0.01)) &body body)
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
These forms will be evaluated from last binding to first."
  (if bindings
      `(let (,(first (first bindings)))
         (unwind-protect
              (progn (setf ,(first (first bindings))
                           ,(second (first bindings)))
                     (finalized-let* ,(rest bindings) ,@body))
           (when ,(first (first bindings))
             (progn ,@(cddr (first bindings))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun count-digits (integer)
  "Returns the number of digits in an integer, sans any sign."
  (if (= 0 integer)
      1
      (values (ceiling (log (abs integer) 10)))))

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
  (apply (curry #'mapcar #'list) lists))

(defun bound-slots-values (instance)
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
        (error "RASSOC of ~A was not found in ~A." key alist))))

(defun assoc-value-or-die (alist key &key (test ''eql))
  "Like ALEXANDRIA:ASSOC-VALUE, except it signals an error if the value is
not found."
  (multiple-value-bind (value foundp)
      (assoc-value alist key :test test)
    (if foundp value
        (error "ASSOC of ~A was not found in ~A." key alist))))

(defun print-hash-table-readably (hash-table
                                  &optional (stream *standard-output*))
  "Prints a hash table readably using ALEXANDRIA:ALIST-HASH-TABLE."
  (let ((test (hash-table-test hash-table))
        (*print-circle* t))
    (format stream "#.(ALEXANDRIA:ALIST-HASH-TABLE~%")
    (format stream "'~S~%" (hash-table-alist hash-table))
    (format stream "  :TEST '~A)" test)
    nil))

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

(defun trim-whitespace (string)
  "Trims whitespace characters from both sides of a string."
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-trim whitespace string)))

(defun hexadecimal-string-p (string)
  "Returns true if the string contains only digits 0-9 and lowercase/uppercase
characters #\A-#\F, false otherwise."
  (let ((chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                 #\a #\b #\c #\d #\e #\f
                 #\A #\B #\C #\D #\E #\F)))
    (loop for char across string
          unless (member char chars) return nil
            finally (return t))))

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

(defun get-unix-time ()
  "Returns the current Unix timestamp."
  (- (get-universal-time) 2208988800))

(defun unix-time-to-datestring (unix-time)
  "Decodes the unix time and returns its textual form in format
\"YYYY-MM-DD HH:MM:SS\"."
  (let* ((universal-time (+ unix-time 2208988800))
         (time (multiple-value-list (decode-universal-time universal-time))))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            (nth 5 time) (nth 4 time) (nth 3 time)
            (nth 2 time) (nth 1 time) (nth 0 time))))

(defun keywordize (string)
  "Given a string, upcases it returns a keyword with the upcased name."
  (intern (string-upcase string) :keyword))

(defun string=-getf (plist indicator)
  "Like GETF, except it tests with STRING= and is there suitable for use
with strings and symbols."
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (and (string= key indicator))
          return value))

(defun peek-char-no-hang (&optional (input-stream *standard-input*)
                            (eof-error-p t) eof-value recursive-p)
  "Like PEEK-CHAR, except it immediately returns NIL if no character is
available for reading on the input stream."
  (let ((character (read-char-no-hang input-stream eof-error-p
                                      eof-value recursive-p)))
    (when character
      (unread-char character input-stream)
      character)))

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
  (member char '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)))

(defun print-instance-readably (object &optional (stream *standard-output*))
  "Prints an instance readably using the #. notation with MAKE-INSTANCE.
This function is a hack. Its functioning depends on all direct slots of a class
being of form %FOO or FOO and having an initarg keyword called :FOO. All values
stored in these slots need to be printable readably. Moreover, indirect slots
are not restored."
  (check-type object standard-object)
  (flet ((internal-slot-keyword (symbol)
           (let* ((string (string symbol))
                  (percentp (eql (aref string 0) #\%))
                  (result (if percentp (subseq (string symbol) 1) string)))
             (values (intern result :keyword)))))
    (let* ((class (class-name (class-of object)))
           (slots (bound-slots-values object))
           (values (mapcar (curry #'slot-value object) slots))
           (keywords (mapcar #'internal-slot-keyword slots))
           (plist (apply #'nconc (zip keywords values)))
           (result `(make-instance ',class ,@plist)))
      (with-standard-io-syntax
        (let ((*print-pretty* t))
          (format stream "#.~S" result))))))
