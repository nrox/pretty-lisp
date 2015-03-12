

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


;;; This file concerns what is server related, the firts processing of ajax requests and registration of public files 


( in-package :pretty-lisp )


( defparameter +web-source-folder+
  ( concatenate 'string +source-folder+ "public/" )
  "The source folder for the .html, .css, .js files" )


( defclass pretty-lisp-acceptor ( easy-acceptor ) ( )
          ( :documentation
           "This is the acceptor of the pretty-lisp framework. Inherits from Hunchentoot easy-acceptor and has no additional slots." ) )


( defvar +pretty-lisp-acceptor-name+ "pretty-lisp-acceptor-name" )


( setf *pretty-lisp-acceptor*
        ( if *see-logs* 
            ;; log errors to std out
             ( make-instance 'pretty-lisp-acceptor :name
             +pretty-lisp-acceptor-name+ :port *ide-port* :document-root
             +web-source-folder+ )
            
            ;; do not log errors to std out, errors will be alerted with browser
             ( make-instance 'pretty-lisp-acceptor :name
             +pretty-lisp-acceptor-name+ :port *ide-port* :document-root
             +web-source-folder+ :access-log-destination nil
             :message-log-destination nil ) ) )


;; easy handler for user interaction requests 


( define-easy-handler
 ( exchange-xml :uri "/ExchangeXML" :acceptor-names
  ( list +pretty-lisp-acceptor-name+ ) )
 ( ( xmldata :init-form ( raw-post-data ) ) ) ( setf ( content-type* ) "text/xml" )  
 ;; request-handler-xml is the main target to process
 ;; incomming ajax requests. It then forwards to other functions.
  ( request-handler-xml xmldata ) )
