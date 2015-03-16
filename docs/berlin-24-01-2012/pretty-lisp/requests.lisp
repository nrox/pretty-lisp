

;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :pretty-lisp )

(defparameter +EVAL+ "eval" )

(defparameter +MP-REQUEST+ "mprequest" )

(defparameter +MP-RESPONSE+ "mpresponse" )

(defparameter +PKG+ "pkg" )

(defparameter +ERROR+ "error" )

(defparameter *debug2* "ii" )

(defparameter *allowed-functions* (list (cons "fileoperation" #'fileoperation )(cons "hint" #'hint )(cons "editelement" #'editelement )(cons "topsubmenu" #'topsubmenu )(cons "chromex" #'chromex )))

(defun not-recognized (xml )"If the called function is not in the list,
return this error." (make-instance 'xml-node :tag +ERROR+ :children (list (format nil "Not allowed function call: ~A" (xml-tag xml )))))

(defun get-allowed-function (function-name )"Gets the function object from the list of allowed functions.
This is a security procedure, to avoid execution of arbitrary code
from a Javascript request." (or (rest (assoc function-name *allowed-functions* :test #'string-equal ))#'not-recognized ))

(defun filter-errors (resp xml )"takes the multiple value list resulting from ignore-erros and retrieve the car if the length is 1
  otherwise a xml node having a child as a print of the error" (declare (ignore xml ))(if (= 1 (length resp ))(car resp )(make-instance 'xml-node :tag +ERROR+ :children (list (format nil "~A" resp )))))

(defun mprequest (xml )"call functions that are children of xml. the functions 
are the children name and takes as argument the children in xml format.
the return value of each function should be also an s-xml or rox-xml element" (let ((children (xml-children xml ))(responses nil )(curresp nil )(fname nil ))(dolist (child children nil )(setf fname (get-allowed-function (xml-tag child )))(setf *debug2* fname )(setf curresp (filter-errors (multiple-value-list (if *error-alerts* ;; alert errors in browser
(ignore-errors (funcall fname child ));; log errors normaly to std out (?!)
(funcall fname child )))child ))(push curresp responses ))(make-instance 'xml-node :tag +MP-RESPONSE+ :children (reverse responses ))))

(defun error-xml-undefined ()(xml-parse-from-string "<b class='errors'>no-action-match</b>" ))

(defun forward-request (xml )(let ((name (xml-tag xml )))(cond ((string= name +MP-REQUEST+ )(mprequest xml ))(t (error-xml-undefined )))))

(defun request-handler-xml (xmldata )(xml-to-string (forward-request (xml-parse-from-string (octets-to-string xmldata :external-format :utf-8 )))))