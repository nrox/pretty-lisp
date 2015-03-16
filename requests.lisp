

;;; pretty-LISP Editor (beta) 

#|
 Copyright (c) 2012, Nuno Rocha.  All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials
     provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|# 

#|
All ajax requests are handled in this sequence:

request-event-handler ---> forward-request ---> {interactive-request, ...}

interactive-request ---> {fileoperation, hint, editelement, topsubmenu, chromex}
|# 

( in-package :pretty-lisp )

( defvar *editor-port* 4555 )

( defvar *pretty-lisp-acceptor*
  nil
  "The unique instance of the acceptor intended to be used." )

( defparameter +interactive-request-tag+ "irequest" "The tag for xml requests" )

( defparameter +extension-request-tag+
  "chromex"
  "The tag for chrome extension xml requests" )

( defparameter +custom-request-tag+
  "customreq"
  "The tag for custom requests, not essencial to this package." )

( defparameter +response-tag+ "response" "Tag for xml responses" )

( defparameter +error+
  "error"
  "Tag for xml responses, in the event of an error." )

( defparameter +web-source-folder+
  ( concatenate 'string +source-folder+ "public/" )
  "The source folder for the .html, .css, .js files" )

( defparameter *custom-functions*
  nil
  "This is an a-list of elements (\"function\" . #'function) 
to register functions which are not essential to the package, 
but are used by some in-development functionalities." )

( defun message ( xml )
  "Retrives the message stored to acceptor :message-log-destination "
  ( declare ( ignore xml ) )
  ( if *multi-user-demo*
      ( progn
       
       ;;flush
        ( get-output-stream-string *acceptor-message-stream* )
       ( js-predefined "showServerMessage"
        "This is a multi-user demo. 
Some features are disabled, including 
showing server internal errors." ) )
      ( js-predefined "showServerMessage"
       ( get-output-stream-string *acceptor-message-stream* ) ) ) )

( defparameter *allowed-functions*
  ( list ( cons "fileoperation" #'fileoperation ) ( cons "hint" #'hint )
        ( cons "editelement" #'editelement ) ( cons "topsubmenu" #'topsubmenu )
        ( cons "message" #'message ) )
  "These are the functions that can be called from ajax requests." )

( defun not-recognized ( xml )
  "If the called function is not in the 
*allowed-functions* list, return this error."
  ( js-predefined "alert"
   ( format nil "Not allowed function call: ~A" ( xml-tag xml ) ) ) )

( defun get-allowed-function ( function-name )
  "Gets the function object from the list of allowed functions."
  ( or ( rest ( assoc function-name *allowed-functions* :test #'string-equal ) )
      #'not-recognized ) )

( defun get-custom-function ( function-name )
  "Gets the function from *custom-functions*. If not in the list return #'not-recognized."
  ( or ( rest ( assoc function-name *custom-functions* :test #'string-equal ) )
      #'not-recognized ) )

( defun set-custom-function ( function-name function-symbol )
  "Sets the function in in *custom-functions*"
  ( pushnew ( cons function-name function-symbol ) *custom-functions* :test
           #'string-equal :key #'car )
  ( setf ( rest ( assoc function-name *custom-functions* :test #'string-equal ) )
          function-symbol ) )

( defun filter-errors ( resp xml )
  "takes the multiple value list resulting from ignore-erros and retrieve the car if the length is 1
  otherwise a xml node having a child as a print of the error"
  ( declare ( ignore xml ) )
  ( if ( = 1 ( length resp ) )
      ( car resp )
      ( js-predefined "alert"
       ( format nil "Lisp side ERROR: ~{~A~}"
               ( list #\Newline #\Newline ( first resp ) #\Newline #\Newline
                     ( second resp ) ) ) ) ) )

( defun interactive-request ( xml )
  "Loops trough the children of xml and calls the functions which has the same name as the tag of the children."
  
  #|
Such a xml request is like:
<irequest>
<editelement>
  <text roxid='roxid45' operation='Update'>defparameter</text>
</editelement>
</irequest>
|#
   ( let*
   ( ( children ( xml-children xml ) ) ( responses nil ) ( curresp nil ) ( fname nil )
    ( current-session ( start-session ) ) 
    ;; this is a global variable, replace with the session value
     ( *ID-COUNTER* ( or ( session-value 'id-counter current-session ) 0 ) ) 
    ;; this is a global variable, replace with the session value
     ( *EDITING-FILES* ( session-value 'editing-files current-session ) )
    ( *hints-history* ( session-value 'hints-history current-session ) ) ;; TODO code repetition, consider using macros
                                                                      )
   
   ;; for each children, try to process it
    ( dolist ( child children nil )
     ( setf fname ( get-allowed-function ( xml-tag child ) ) )
     ( setf curresp ( funcall fname child ) )
     ( push curresp responses ) )
    ;; save session values
     ( setf ( session-value 'id-counter current-session ) *ID-COUNTER* )
   ( setf ( session-value 'editing-files current-session ) *EDITING-FILES* )
   ( setf ( session-value 'hints-history current-session ) *hints-history* )
   ( setf ( session-max-time current-session ) *sessions-timeout* ) 
   ;; flush messages
    ( get-output-stream-string *acceptor-message-stream* ) 
   ;; return the xml response
    ( make-instance 'xml-node :tag +response-tag+ :children ( reverse responses ) ) ) )

( defun custom-request ( xml )
  "Working in progress..."
  ( let ( ( responses '( ) ) )
    ( dolist ( child ( xml-children xml ) )
      ( push ( funcall ( get-custom-function ( xml-tag child ) ) child ) responses ) )
    
    ;; return the xml response
     ( make-instance 'xml-node :tag +response-tag+ :children
     ( reverse responses ) ) ) )

( defparameter *debug-ext* t )

( defun chrome-extension-request ( xml )
  "Calls the chrome extension handler."
  
  #|
Such a xml request looks like this:
<chromex>
<preid0>
  %28defun+hello-world+%28%29+%22hello+world%21%22%29
</preid0>
<preid1>
  %28defun+hello-%28hello-world%29
</preid1>
</chromex>
|#
   ( let ( ;; this is a global variable
          ( *ID-COUNTER* 0 ) )
    ( make-instance 'xml-node :tag +response-tag+ :children
     ( if *debug-ext*
         ( list ( chromex xml ) )
         ( list
          ( filter-errors ( multiple-value-list ( ignore-errors ( chromex xml ) ) )
           xml ) ) ) ) ) )

( defun forward-request ( xml )
  "Depending on the tag of the xml request,
The request is forwarded to the proper function."
  ( let ( ( name ( xml-tag xml ) ) )
    ( cond
     ( ( string-equal name +interactive-request-tag+ ) ( interactive-request xml ) )
     ( ( string-equal name +extension-request-tag+ )
      ( chrome-extension-request xml ) )
     ( ( string-equal name +custom-request-tag+ ) ( custom-request xml ) )
     ( t ( not-recognized xml ) ) ) ) )

( defun request-handler-xml ( xmldata )
  "This function is called by the server event-handler, to process all ajax requests."
  ( xml-to-string
   ( forward-request
    ( xml-parse-from-string
     ( if ( stringp xmldata )
         xmldata
         ( octets-to-string xmldata :external-format :utf-8 ) ) ) ) ) )

( defclass pretty-lisp-acceptor ( easy-acceptor ) ( )
          ( :documentation
           "This is the acceptor of the pretty-lisp framework. Inherits from Hunchentoot easy-acceptor and has no additional slots." ) )

( defvar +pretty-lisp-acceptor-name+ "pretty-lisp-acceptor-name" )

( defun up ( &key ( port *editor-port* ) )
  ( setf *editor-port* port )
  ( setf *pretty-lisp-acceptor*
          ( make-instance 'pretty-lisp-acceptor :name
           +pretty-lisp-acceptor-name+ :port *editor-port* :document-root
           +web-source-folder+ :access-log-destination nil
           :message-log-destination *acceptor-message-stream* ) )
  ( define-easy-handler
   ( exchange-xml :uri "/ExchangeXML" :acceptor-names
    ( list +pretty-lisp-acceptor-name+ ) )
   ( ( xmldata :init-form ( raw-post-data ) ) ) ( setf ( content-type* ) "text/xml" )  
   ;; request-handler-xml is the main target to process
   ;; incomming ajax requests. It then forwards to other functions.
    ( request-handler-xml xmldata ) )
  ( or
   ( and *pretty-lisp-acceptor* ( hunchentoot:start *pretty-lisp-acceptor* )
        ( format nil "~%~% Running on ~%~% http://localhost:~A/ ~%~%" port ) )
   ( format nil "Server startup failed on port ~A." port ) ) )

( defun down ( )
  ( format nil "~%~% server down ~%~% ~A ~%~%"
          ( and *pretty-lisp-acceptor*
               ( hunchentoot:stop *pretty-lisp-acceptor* ) ) ) )

( unless *auto-start*
  ( format t
          "~%~% to a quickstart on port ~A, execute ~%~% ( pretty-lisp:up :port ~A) ~%~%"
          *editor-port* *editor-port* ) )