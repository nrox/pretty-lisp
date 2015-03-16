

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

( in-package :pretty-lisp )

( defparameter +max-hints+ 200 "max. number os items in the hints list" )

( defun trim-for-comparison ( str )
  "The tokens are compared trimming some of their char. 
This function trims according to this."
  ( string-trim "#\"',`;: " str ) )

( defgeneric get-all-texts
    ( xml &optional buffer ) )

( defmethod get-all-texts ( ( xml xml-node ) &optional buffer )
  ( declare ( optimize ( speed 3 ) ) )
  ( dolist ( child ( xml-children xml ) buffer )
    ( setf buffer ( union buffer ( get-all-texts child ) :test #'string= ) ) ) )

( defmethod get-all-texts ( xml &optional buffer )
  ( declare ( optimize ( speed 3 ) ) )
  ( cond
   ( ( consp xml )
    ( dolist ( child xml buffer )
      ( setf buffer ( union buffer ( get-all-texts child ) :test #'string= ) ) ) )
   ( ( pfile-manager-p xml )
    ( setf buffer
            ( union buffer ( get-all-texts ( pfile-xml xml ) ) :test #'string= ) ) )
   ( t ( list ( format nil "~A" xml ) ) ) ) )

( defgeneric get-all-atoms-with-text
    ( xml text &optional buffer ) )

( defmethod get-all-atoms-with-text ( ( xml pretty-atom ) text &optional buffer )
  ( declare ( optimize ( speed 3 ) )
           ( ignore buffer ) )
  ( if ( equalp ( trim-for-comparison ( xml-value xml ) ) text )
      ( list xml )
      nil ) )

( defmethod get-all-atoms-with-text ( xml text &optional buffer )
  ( cond
   ( ( consp xml )
    ( dolist ( child xml buffer )
      ( setf buffer ( append buffer ( get-all-atoms-with-text child text ) ) ) ) )
   ( ( pfile-manager-p xml )
    ( setf buffer
            ( append buffer ( get-all-atoms-with-text ( pfile-xml xml ) text ) ) ) )
   ( ( pretty-list-p xml )
    ( dolist ( child ( xml-children xml ) buffer )
      ( setf buffer ( append buffer ( get-all-atoms-with-text child text ) ) ) ) )
   ( t nil ) ) )

( defun hint-item-attributes ( fun arg1 arg2 )
  "Prepares the attributes for the item in the hints results list.
fun must be a format in the form 'function(~A,~A)'
arg1 and arg2 are used to format the fun string."
  ( append ( list ( cons "class" "hint-complete" ) )
          ( and fun
               ( list
                ( cons "onclick"
                      ( format nil fun
                              ( string-replace ( string-replace arg1 "\\" "\\\\" )
                               "\"" "\\\"" )
                              ( string-replace ( string-replace arg2 "\\" "\\\\" )
                               "\"" "\\\"" ) ) ) ) ) ) )

( defun hint-complete ( texto )
  ( setf texto ( format-and-ucase texto ) )
  ( let*
   ( ( atoms-list
     ( remove-duplicates
      ( append ( get-all-texts *EDITING-FILES* ) +HYPERSPEC-LIST+ ) :test
      #'string= ) )
    ( completions
     ( truncate-list
      ( sort
       ( remove-if-not #'( lambda ( txt ) ( search texto ( format-and-ucase txt ) ) )
        atoms-list )
       #'string< )
      +max-hints+ ) ) )
   ( mapcar
    #'( lambda ( item )
      ( make-instance 'xml-node :tag "div" :attributes
       ( hint-item-attributes "hintComplete(\"~A\", ~A)" item "true" ) :children
       ( list item ) ) )
    completions ) ) )

( defun hint-similar ( texto )
  ( setf texto ( format-and-ucase texto ) )
  ( let*
   ( ( atoms-list
     ( remove-duplicates
      ( append ( get-all-texts *EDITING-FILES* ) +HYPERSPEC-LIST+ ) :test #'string=
      :key #'format-and-ucase ) )
    ( len-texto ( length texto ) ) ( ref-len ( * len-texto 1.34 ) )
    ( completions
     ( sort
      ( remove-if-not
       #'( lambda ( txt )
         ( setf txt ( format-and-ucase txt ) )
         ( and ( < ( length txt ) ref-len ) ( < ( levenshtein-ratio txt texto ) 0.34 ) ) )
       atoms-list )
      #'string< ) ) )
   ( mapcar
    #'( lambda ( item )
      ( make-instance 'xml-node :tag "div" :attributes
       ( hint-item-attributes "hintComplete(\"~A\", ~A)" item "true" ) :children
       ( list item ) ) )
    completions ) ) )

( defun hint-history ( )
  ( mapcar
   #'( lambda ( item )
     ( make-instance 'xml-node :tag "div" :attributes
      ( hint-item-attributes "hintComplete(\"~A\", ~A)" item "true" ) :children
      ( list item ) ) )
   *hints-history* ) )

( defun hint-hyperspec ( term )
  ( let ( ( href ( gethash ( format-and-ucase term ) ( get-hyperspec ) ) ) )
    ( if href
        ( js-predefined "window.open"
         ( string-replace href "../" +LISPWORKS-HYPERSPEC-URL+ )
         "menubar=yes,toolbar=yes" )
        ( js-predefined "alert"
         ( format nil "~A: not found in HyperSpec." term ) ) ) ) )

( defun hint-search ( pfile texto )
  ( setf texto ( trim-for-comparison texto ) )
  ( let ( ( in-current-file
         ( mapcar
          #'( lambda ( node )
            ( cons ( xml-attribute-get node :roxid ) ( xml-value node ) ) )
          ( get-all-atoms-with-text pfile texto ) ) )
        ( in-other-files
         ( sort
          ( remove-duplicates
           ( reduce
            #'( lambda ( cur p-file )
              ( append cur
                      ( if ( eq pfile p-file )
                          nil
                          ( if ( get-all-atoms-with-text p-file texto )
                              ( list ( pfile-name p-file ) ) ) ) ) )
            *EDITING-FILES* :initial-value nil )
           :test #'string= )
          #'string< ) ) )
    ( append
     ( mapcar
      #'( lambda ( item )
        ( let ( ( roxid ( auto-id "sc" ) ) )
          ( make-instance 'xml-node :tag "div" :attributes
           ( append ( list ( cons "id" roxid ) )
                   ( hint-item-attributes "searchItemClick('~A','~A');"
                    ( first item ) roxid ) )
           :children ( list ( rest item ) "(" ( pfile-name pfile ) ")" ) ) ) )
      in-current-file )
     ( mapcar
      #'( lambda ( item )
        ( make-instance 'xml-node :tag "div" :attributes
         ( hint-item-attributes nil nil nil ) :children ( list item ) ) )
      in-other-files ) ) ) )

( defun hint-suggest ( texto suggest-function )
  ( setf texto ( trim-for-comparison texto ) )
  ( let*
   ( ( suggestions
     ( sort
      ( truncate-list
       ( remove-duplicates
        ( mapcar #'( lambda ( at ) ( funcall suggest-function at ( xml-parent at ) ) )
                ( get-all-atoms-with-text *EDITING-FILES* texto ) )
        :test #'string= )
       +max-hints+ )
      #'( lambda ( x y ) ( < ( length x ) ( length y ) ) ) ) ) )
   ( mapcar
    #'( lambda ( item )
      ( make-instance 'xml-node :tag "div" :attributes
       ( hint-item-attributes "hintComplete(\"~A\", ~A)" item "false" ) :children
       ( list item ) ) )
    suggestions ) ) )

( defgeneric make-suggestion
    ( elem xml ) )

( defmethod make-suggestion ( elem xml ) "nil" )

( defmethod make-suggestion ( ( elem pretty-atom ) ( xml pretty-list ) )
  ( concatenate 'string "("
               ( reduce
                #'( lambda ( str next )
                  ( concatenate 'string str
                               ( cond ( ( eq elem next ) ( xml-value next ) )
                                     ( ( pretty-atom-p next ) "nil" )
                                     ( ( pretty-list-p next ) "(nil)" ) ( t nil ) )
                               " " ) )
                ( xml-children xml ) :initial-value " " )
               ")" ) )

( defgeneric make-examples
    ( elem xml ) )

( defmethod make-examples ( elem xml ) "nil" )

( defmethod make-examples ( ( elem pretty-atom ) ( xml pretty-list ) )
  ( concatenate 'string "("
               ( reduce
                #'( lambda ( str next )
                  ( concatenate 'string str
                               ( cond ( ( pretty-atom-p next ) ( xml-value next ) )
                                     ( ( pretty-list-p next )
                                      ( concatenate 'string "("
                                                   ( reduce
                                                    #'( lambda ( str node )
                                                      ( concatenate 'string str
                                                                   ( if ( pretty-atom-p
                                                                        node )
                                                                       ( xml-value
                                                                        node )
                                                                       "(#|...|#)" )
                                                                   " " ) )
                                                    ( xml-children next )
                                                    :initial-value " " )
                                                   " )" ) ) )
                               " " ) )
                ( xml-children xml ) :initial-value " " )
               ")" ) )

( defgeneric make-def
    ( elem xml ) )

( defmethod make-def ( elem xml ) )

( defmethod make-def ( ( elem pretty-atom ) ( xml pretty-list ) )
  ( let*
   ( ( def-atom ( car ( xml-children xml ) ) )
    ( len-children ( length ( xml-children xml ) ) )
    ( def-atom-text
     ( if ( pretty-atom-p def-atom )
         ( xml-value def-atom ) ) )
    ; (len-def-atom-text (length def-atom-text ))
     )
   ( if ( and def-atom-text ( > len-children 2 ) ( > ( length def-atom-text ) 3 )
            ( string= "def" ( subseq def-atom-text 0 3 ) )
            ( string/= ( xml-value elem ) def-atom-text ) )
       ( concatenate 'string "("
                    ( reduce
                     #'( lambda ( str next )
                       ( concatenate 'string str " " ( xml-to-code-string next ) ) )
                     ( xml-children xml ) :initial-value " " :end 3 )
                    ( if ( > len-children 3 )
                        "(#|...|#)" )
                    ")" )
       ( xml-value elem ) ) ) )

( defun hint ( xml )
  "Processes the hint operation. xml is the hint button which activated the request."
  
  #|
The xml has the example form:
<hint>
  <button operation='search' texto='my-fun' file='/home/rookie/file.lisp'>search</button>
</hint>
|#
   ( let*
   ( ( button ( car ( xml-children xml ) ) )
    ( operation ( xml-attribute-get button :operation ) )
    ( file ( xml-attribute-get button :file ) )
    ( texto ( xml-attribute-get button :texto ) )
    ( pfile ( find file *EDITING-FILES* :key #'pfile-path :test #'string= ) )
    ( xml-status ( jquery-1 "#bottomstatus" "html" operation texto ) )
    ( xml-answer ) )
   ( unless ( find operation +allowed-hint-operations+ :test #'string-equal )
     ( return-from hint
      ( jquery-1 "#bottomstatus" "html" operation
       ": disabled for this session!" ) ) )
   ( unless ( string-equal operation :history )
     ( setf *hints-history* ( remove texto *hints-history* :test #'string= ) )
     ( if ( not ( string= texto "" ) )
         ( push texto *hints-history* ) )
     ( setf *hints-history* ( truncate-list *hints-history* +max-hints+ ) ) )
   ( when ( string-equal operation :complete )
     ( setf xml-answer ( hint-complete texto ) ) )
   ( when ( string-equal operation :similar )
     ( setf xml-answer ( hint-similar texto ) ) )
   ( when ( string-equal operation :search )
     ( setf xml-answer ( hint-search pfile texto ) ) )
   ( when ( string-equal operation :history ) ( setf xml-answer ( hint-history ) ) )
   ( when ( string-equal operation :suggest )
     ( setf xml-answer ( hint-suggest texto #'make-suggestion ) ) )
   ( when ( string-equal operation :examples )
     ( setf xml-answer ( hint-suggest texto #'make-examples ) ) )
   ( when ( string-equal operation :def )
     ( setf xml-answer ( hint-suggest texto #'make-def ) ) )
   ( when ( string-equal operation :hyperspec )
     ( setf xml-answer ( hint-hyperspec texto ) ) )
   ( wrap
    ( list xml-status
          ( if ( string-equal operation :hyperspec )
              xml-answer
              ( jquery-1 "#hintslist" "html" xml-answer ) ) ) ) ) )