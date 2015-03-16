

;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :cl-user )

(mapcar #'ql:quickload '(:hunchentoot :s-xml ))

(defpackage "pretty-lisp" (:nicknames :pretty-lisp )(:use :cl :hunchentoot :s-xml :flexi-streams ))

(in-package :pretty-lisp )

(defparameter *source-files* '("utils" "xml" "parser" "transform" "edit" "extension" "requests" "server" )"source files for this package, without their .lisp extension" )

(defparameter +source-folder+ (directory-namestring *load-pathname* )"The folder where this file is when loaded" )

(defparameter *test* nil )

(defparameter *error-alerts* t "Alert errors in browser (t) or normal log to std out (nil)" )

(defparameter *see-logs* nil "Logs are printed to std out (t) or not printed at all (nil)" )

(defparameter *compile-files* nil "Files are compiled and then the .fasl loaded (t) or just the .lisp loaded (nil)" )

;;; compile and/or load source files


(mapcar #'(lambda (filename )(let ((file-path (concatenate 'string +source-folder+ filename ".lisp" )))(if *compile-files* (load (namestring (compile-file file-path )))(load file-path ))))*source-files* )