

(;; Nuno Rocha
;; Berlin Lispers Meetup, Jan 24, 2012 @ co.up 
;; A suggestion for parentheses representation in a Common Lisp Editor
(pretty-LISP ))

(:CONCERNS )

(defun issues-and-concerns (&optional (issue nil ))(let ((some-issues '((:security (;; the main concern
"Possible vulnerabilities trought Javascript.
In theory any LISP code can be edited and then executed in the server." ))(:debugging (;; other big concern
"There is only a  minimal feedback on code execution and bugs.
The executed code is EVAL by Hunchentoot threads." ))(:file-format ("After saving in a plain text file, 
the code has valid lisp syntax,
but no nice, pleasant format for human reading." ))(:implementations (;; test other implementations
"Was developed with SBCL, but 
should run where Hunchentoot runs." ))(:browsers ("Works only on Chrome" ;; is this a problem?
)))))(format nil "There is an issue: ~A - ~A" issue (rest (find issue some-issues :key #'car )))))

((issues-and-concerns :security )(issues-and-concerns :debugging )(issues-and-concerns :file-format )(issues-and-concerns :browsers )(issues-and-concerns :implementations ))

;;; issues and concerns => work TODO 
