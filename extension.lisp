

;;;; pretty-LISP - Common LISP Editor 


;;;;   Nuno Rocha 2012 


;;;; 


;;;; this is the server part of a chrome extension to transform code in web pages 


;;;; it works in association with the extension files 


;;;; in the converted page, the code must be between <pre> tags 


( in-package :pretty-lisp )


( defun insert-js-actions ( pretty-nodes-list preid )
  "For the list of pretty nodes, javascript instructions will be prepared to 
replace the element with id==preid, with these pretty nodes.
The javascript/jquery instructions must be latter interpreted from the returned xml structure."
  ( let ( ( actions ( make-instance 'xml-node :tag "dig" ) )
        ( selector ( concatenate 'string "#" preid ) )
        ( node-svg nil ) )
    
    ;; instruction to replace the code (pre tag)  with and empty div and the same id
     ( xml-append-child actions
     ( jquery-1 selector "replaceWith"
      ( make-instance 'xml-node :tag "div" :attributes
       ( list ( cons :id preid ) ( cons :class "prettycontainer" ) ) ) ) )
    ( dolist ( root-node pretty-nodes-list )
     
      ;; transform the xml node to svg, with proper layout and formats
       ( progn
       ( set-id-all-family root-node )
       ( ;; DELETEME (need this?)
          setf ( xml-parent root-node ) nil )
       ( setf node-svg ( process-dimensioning root-node ) )
       ( setf node-svg ( wrap-with-root-svg node-svg ) ) )
     
      ;; append the pretty node to the div container
       ( xml-append-child actions
       ( jquery-1 selector "append"
        ( make-instance 'xml-node :tag "div" :attributes
         ( list
          ( cons :id
                ( concatenate 'string "p" ( xml-attribute-get root-node :roxid ) ) )
          ( cons :class "pcodecontainer" ) )
         :children ( list node-svg ) ) ) ) )
    
    ;; return the to-be-interpreted js actions, in xml format
     actions ) )


( defun chromex ( xml )
  "This functiosn received the extension request, a xml strucure which has all the code to be
 converted and loops trough it to set the pretty convertion."
  ( make-instance 'xml-node :tag "dig" :children
   ( mapcar
    #'( lambda ( xml-item )
      ( insert-js-actions 
       ;; parse the code text to pretty-nodes
        ( parse-pretty-atom
        ( make-instance 'pretty-atom :children
         ( list ( format nil "~A" ( ;; the code is url encoded
                                   url-decode ( xml-value xml-item ) ) ) ) ) )
        ;; the id of the pre element wich has the code text, is the xml-item tag
         ( format nil "~A" ( xml-tag xml-item ) ) ) )
      ;; each children of the request is a piece of code to be independently converted.
      ;; on the web page, each has its on pre tag and id
       ( xml-children xml ) ) ) )
