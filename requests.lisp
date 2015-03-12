

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


#|
All ajax requests are handled in this sequence:

request-event-handler ---> forward-request ---> {interactive-request, ...}

interactive-request ---> {fileoperation, hint, editelement, topsubmenu, chromex}
|# 


( in-package :pretty-lisp )


( defparameter +interactive-request-tag+ "irequest" "The tag for xml requests" )


( defparameter +response-tag+ "response" "Tag for xml responses" )


( defparameter +error+
  "error"
  "Tag for xml responses, in the event of an error." )


( defparameter *allowed-functions*
  ( list ( cons "fileoperation" #'fileoperation ) ( cons "hint" #'hint )
        ( cons "editelement" #'editelement ) ( cons "topsubmenu" #'topsubmenu )
        ( cons "chromex" #'chromex ) )
  "These are the functions that can be called from ajax requests." )


( defun not-recognized ( xml )
  "If the called function is not in the 
*allowed-functions* list, return this error."
  ( js-predefined "alert"
   ( format nil "Not allowed function call: ~A" ( xml-tag xml ) ) ) )


( defun get-allowed-function ( function-name )
  "Gets the function object from the list of allowed functions.
This is a security procedure, to avoid execution of arbitrary code
from a Javascript request."
  ( or ( rest ( assoc function-name *allowed-functions* :test #'string-equal ) )
      #'not-recognized ) )


( defun filter-errors ( resp xml )
  "takes the multiple value list resulting from ignore-erros and retrieve the car if the length is 1
  otherwise a xml node having a child as a print of the error"
  ( declare ( ignore xml ) )
  ( if ( = 1 ( length resp ) ) ( car resp )
      ( js-predefined "alert"
       ( format nil "Lisp side ERROR: ~{~A~}"
               ( list #\Newline #\Newline ( first resp ) #\Newline #\Newline
                     ( second resp ) ) ) ) ) )


( defun interactive-request ( xml )
  "Loops trough the children of xml and calls the functions which has the same name as the tag of the children."
  
  #|
Such a xml request has the example form:
<mprequest>
<editelement>
  <text roxid='roxid45' operation='Update'>defparameter</text>
</editelement>
</mprequest>
|#
   ( let ( ( children ( xml-children xml ) ) ( responses nil ) ( curresp nil ) ( fname nil ) )
    
    ;; for each children, try to process it
     ( dolist ( child children nil )
      ( setf fname ( get-allowed-function ( xml-tag child ) ) )
      ( setf curresp
              ( filter-errors
               ( multiple-value-list
                ( if *error-alerts*  ;; alert errors in browser
                                     ( ignore-errors ( funcall fname child ) ) 
                    ;; log errors normaly to std out (?!)
                     ( funcall fname child ) ) )
               child ) )
      ( push curresp responses ) )
    ( make-instance 'xml-node :tag +response-tag+ :children
     ( reverse responses ) ) ) )


( defun forward-request ( xml )
  "Depending on the tag of the xml request,
The request is forwarded to the proper function."
  ( let ( ( name ( xml-tag xml ) ) )
    ( cond ( ( string= name +interactive-request-tag+ ) ( interactive-request xml ) )
          ( t ( not-recognized xml ) ) ) ) )


( defun request-handler-xml ( xmldata )
  "This function is called by the server event-handler, to process all ajax requests."
  ( xml-to-string
   ( forward-request
    ( xml-parse-from-string
     ( octets-to-string xmldata :external-format :utf-8 ) ) ) ) )
