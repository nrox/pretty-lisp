

;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :pretty-lisp )

(defvar +ide-port+ 4555 )

(defparameter +web-source-folder+ (concatenate 'string +source-folder+ "public/" ))

(if *see-logs* (hunchentoot:start (make-instance 'easy-acceptor :port +ide-port+ ))(hunchentoot:start (make-instance 'easy-acceptor :port +ide-port+ :access-log-destination nil :message-log-destination nil )))

;;; easy handler for ajax requests


(define-easy-handler (exchange-xml :uri "/ExchangeXML" )((xmldata :init-form (raw-post-data )))(setf (content-type* )"text/xml" )(request-handler-xml xmldata ))

;;; This creates an handler for incoming file requests.
;;; TODO Not the best method, use folder dispatcher instead


(defun register-file (name root )(push (create-static-file-dispatcher-and-handler (concatenate 'string "/" name )(concatenate 'string root name ))*dispatch-table* ))

;; register all public files


(dolist (file (list-folder-filenames +web-source-folder+ ))(register-file file +web-source-folder+ ))

;; register index


(push (create-static-file-dispatcher-and-handler "/" (concatenate 'string +web-source-folder+ "index.html" ))*dispatch-table* )