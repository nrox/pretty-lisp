

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

( when nil
  ( defmethod parse-pretty-atom ( ( xml pretty-atom ) )
             ( let ( ( str ( xml-value xml ) ) )
               ( setf str ( ignore-errors ( code-string-to-xml str ) ) )
               ( cond ( ( consp str ) ( mapcar #'xml-node-to-pretty-node str ) )  
                     ;; FIXME remove because the parsing should return
                     ;; always a list
                      ( ( and ( xml-node-p str )
                           ( setf str ( xml-node-to-pretty-node str ) )
                           ( pretty-list-p str ) )
                      ( xml-children str ) )
                      ;; in case of error
                       ( t ( parse-pretty-atom-2 xml ) ) ) ) ) )

( defun chromex-aux ( xml )
  ( make-instance 'xml-node :tag "dig" :children
   ( mapcar
    #'( lambda ( xml-item )
      ( insert-js-actions 
       ;; parse the code text to pretty-nodes
        ( pretty-nodes-to-xml-nodes
        ( parse-to-pretty-lisp
         ( format nil "~A" ( ;; the code is url encoded
                             url-decode ( xml-value xml-item ) ) ) :for-extension
         nil ) )
        ;; the id of the pre element wich has the code text, is the xml-item tag
         ( format nil "~A" ( xml-tag xml-item ) ) ) )
      ;; each children of the request is a piece of code to be independently converted.
      ;; on the web page, each has its on pre tag and id
       ( xml-children xml ) ) ) )

( defun chromex ( xml )
  "This functiosn received the extension request, a xml strucure which has all the code to be
 converted and loops trough it to set the pretty convertion."
  ( chromex-aux xml ) )

( defun pretty-code ( code &key ( with-classes t ) )
  "Transform text code to pretty code, to be displayed in a web page."
  ( let ( ( *ID-COUNTER* 0 ) )
    ( xml-to-string
     ( make-instance 'xml-node :tag "div" :attributes
      ( list ( cons :class "pretty-code" ) ) :children
      ( mapcar
       #'( lambda ( node )
         ( progn
          ( set-id-all-family node )
          ( progn
           ( set-predefined-layout node )
           ( set-all-positioning-properties node )
           ( setf node ( set-pretty-dimensions node ) )
           ( setf node ( set-absolute-pretty-dimensions node ) )
           ( if with-classes ( set-atom-classes node ) ) )
          ( setf node ( wrap-with-root-svg node ) ) )
         ( make-instance 'xml-node :tag "div" :attributes
          ( if with-classes ( list ( cons :class "pcodecontainer" ) ) ) :children
          ( list node ) ) )
        
       ;; parse-pretty-atom
       ;; the results is a list
        ( parse-pretty-atom
        ( make-instance 'pretty-atom :children
         ( list ( format nil "~A" code ) ) ) ) ) ) ) ) )