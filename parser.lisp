

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

;;; 

;;; - This is a recursive-descent parser 

;;; - The parser accepts a superset of what CL reader accepts 

;;; - Does not signal errors for ilegal characters 

;;; - Does not make any convertion to LISP objects 

;;; - It produces just an alternative representation of the input text code 

;;; - The reconversion to text code should produce fairly the same output as the input 

;;; With the exceptions added for some coherence,  avoid errors, or ease of parsing: 

;;; - Unbalanced parenthesis are compensated. 

;;; - At end of file, strings, comments and symbols will be necessarily "closed". 

;;; - Multiple spaces which do not belong to symbols or comments are discarded. 

;;; - Non graphical characters are replaced by spaces, except newlines 

( in-package :pretty-lisp )

( defparameter +ATOM+ "text" )

( defparameter +LIST+ "rect" )

( defparameter +TYPE+ "type" )

( defparameter +LIST-TYPE+ "listType" )

( defparameter +CLASS-LIST+ "list" )

( defparameter +CLASS-ATOM+ "atom" )

( defparameter +COMM-TRIM+
  ( coerce '( #\; #\Newline #\Space ) 'string )
  "Comments are trimmed/normalized with this string." )

( defclass pretty-list ( xml-node ) ( ( tag :accessor xml-tag :initform +LIST+ ) ) )

( ;; FIXME
   progn ( defgeneric pretty-list-p ( obj ) ) ( defmethod pretty-list-p ( obj ) nil )
 ( defmethod pretty-list-p ( ( obj pretty-list ) ) t ) )

( defclass pretty-atom ( xml-node ) ( ( tag :accessor xml-tag :initform +ATOM+ ) ) )

( ;; FIXME
   progn ( defgeneric pretty-atom-p ( obj ) ) ( defmethod pretty-atom-p ( obj ) nil )
 ( defmethod pretty-atom-p ( ( obj pretty-atom ) ) t ) )

( defclass pretty-descriptor ( xml-node )
          ( ( tag :accessor xml-tag :initform +ATOM+ ) ) )

( defun pretty-descriptor-p ( obj )
  ( string-equal ( class-name ( class-of obj ) ) "PRETTY-DESCRIPTOR" ) )

( defgeneric xml-to-code-string ( xml )
            ( :documentation
             "transforms the code in pretty format to textual code." ) )

( defun normalize-token ( token )
  "Compensates for unbalanced big comments, strings
and specials"
  ( let ( ( len ( length token ) ) )
    ( cond  ;; trim inline comments
            ( ( and ( > len 0 ) ( eq #\; ( char token 0 ) ) ) ( string-trim " " token ) ) 
          ;;strings (FIXME: dashes)
           ( ( and ( > len 0 ) ( eq #\" ( char token 0 ) )
                ( not ( eq #\" ( char token ( - len 1 ) ) ) ) )
           ( concatenate 'string token ( list #\Space #\" ) ) )
          
          ;; specials (FIXME: DASHES)
           ( ( and ( > len 1 ) ( eq #\| ( char token 1 ) ) ( eq #\: ( char token 0 ) )
                ( not ( eq #\| ( char token ( - len 1 ) ) ) ) )
           ( concatenate 'string token ( list #\Space #\| ) ) )
          
          ;; big comments
           ( ( and ( > len 1 )
                ( or ( and ( eq #\# ( char token 0 ) ) ( eq #\| ( char token 1 ) ) )
                    ( and ( eq #\| ( char token ( - len 2 ) ) )
                         ( eq #\# ( char token ( - len 1 ) ) ) ) ) )
           ( do ( ( left ( count-substring "#|" token ) )
                ( right ( count-substring "|#" token ) )
                ( prefix )
                ( sufix ) )
               ( ( = left right ) ( concatenate 'string prefix token sufix ) )
             ( if ( > left right )
                 ( progn
                  ( setf sufix ( concatenate 'string sufix " _by_parser_|#" ) )
                  ( incf right ) )
                 ( progn
                  ( setf prefix ( concatenate 'string prefix "#|_by_parser_ " ) )
                  ( incf left ) ) ) ) )
          ( t token ) ) ) )

( defun trim-comment-marks ( comment )
  ( let* ( ( str ( string-trim +COMM-TRIM+ comment ) ) ( len ( length str ) ) )
   ( if
    ( and ( > len 3 ) ( eq #\# ( char str 0 ) ) ( eq #\| ( char str 1 ) )
         ( eq #\| ( char str ( - len 2 ) ) ) ( eq #\# ( char str ( - len 1 ) ) ) )
    ( subseq str 2 ( - len 2 ) ) str ) ) )

( defun possible-list-type ( token )
  "Decides wheter token can be used as a prefix for a list, i.e. some 'list type'
Usual non nil returns exist for tokens like #' , ,@  `  ' #S `, 
And nil for tokens like #B #. #O The non nil return is the input token, trimmed."
  
  ;; TODO give another look into this function
   ( when token
    ( cond 
          ;; these are necessarily not
           ( ( or ( = 0 ( length token ) ) 
               ;; vectors, non radix 10 numbers, non readable objs
                ( member token '( "#\\" "#*" "#:" "#." "#P" "#O" "#X" "#<" ) :test
                       #'equalp )
               
               ;; radix n, object number
                ( member token '( "#R" "##" ) :test
                       #'( lambda ( tk str )
                         ( and ( char-equal ( char tk 0 ) ( char str 0 ) )
                              ( > ( length tk ) 2 )
                              ( char-equal ( char tk ( 1- ( length tk ) ) )
                               ( char str 1 ) )
                              ( equalp ( remove-if #'digit-char-p tk ) str ) ) ) ) )
           nil )
          
          ;; these are or may be
           ( ( or  ;; among ' ` , and the list whitespaces are allowed
                 ( equal ( string-trim " '`," token ) "" )
               ( member token '( "#'" "#" "#+" "#-" "#." "#S" ) :test #'equalp )
               ( equal ( string-trim " '`" token ) ",@" ) 
               ;; arrays and printed representation of objects
                ( member token '( "#A" "#=" ) :test
                       #'( lambda ( tk str )
                         ( and ( char-equal ( char tk 0 ) ( char str 0 ) )
                              ( > ( length tk ) 2 )
                              ( char-equal ( char tk ( 1- ( length tk ) ) )
                               ( char str 1 ) )
                              ( equalp ( remove-if #'digit-char-p tk ) str ) ) ) ) )
           ( remove #\Space token :test #'char= ) )
          ( t nil ) ) ) )

( defun parser-step ( char-current state-previous )
  "Returns a plist, describing the updated state with the current char,
and a previous state.
This function is critical!"
  
  
  
  
  ;; Though input sequences like ('word1'word2) are interpreted as a list of one symbol
  ;; while by reader it is parsed to a list of two symbols ('word1 'word2)
  ;; this will not cause problems when the code is reconverted to text
  ;; because it will have the same representation.
   ( declare ( optimize ( speed 3 ) ) )
  ( when
      ( and  ;; not end of file
             char-current
           ( or ( graphic-char-p char-current ) ( char= #\Newline char-current ) 
               ;; other chars are replaced by spaces
                ( setf char-current #\Space )  ;; you can do specific implementation changes here
                                            ;; like #\Linefeed or #\Return to be synonymous with #\Newline
                                             ) )
    
    ;; update new state
     ( let ( ( state ( copy-list state-previous ) )
          ( char-last ( getf state-previous :char ) ) )
      
      
      ;; current state is derived from the previous
      ;; these properties must be reset
       ( setf ( getf state :char ) char-current )
      ( setf ( getf state :list-start ) nil )
      ( setf ( getf state :list-end ) nil )
      ( setf ( getf state :separator ) nil )
      ( setf ( getf state :node ) nil )
      
      
      ;; this is used for the browser extension
      ;; allows keeping the text, to pretty only top level lists
       ( setf ( getf state :past )
              ( concatenate 'string ( getf state :past ) ( list char-current ) ) )
      
      ;; reset number os recent dashes
       ( cond 
            ;; separator: space
             ( ( and ( char= char-current #\Space ) ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :escaped ) ) ( not ( getf state :special ) ) )
             ( progn
              ( setf ( getf state :separator ) t )
              
              ;; set the final token, if any was concatenated
               ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              ( setf ( getf state :concatenation ) nil )
              
              ;; nothing is done to the current concatenation (!)
              ;; but the space is not added to it
               ) )
             
            ;; separator : newline, or
            ;; end of an inline comment
             ( ( and ( char= char-current #\Newline )
                  ( not ( getf state :comment-big ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) )
             ( progn
              ( setf ( getf state :separator ) t )
              ( setf ( getf state :comment-inline ) nil )
              ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              ( setf ( getf state :concatenation ) nil )
              ;; the space is not added to :token entry
               ) )
            
            ;; start of a big comment # |
             ( ( and ( char= char-current #\| ) ( char= char-last #\# )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) )
             ( if ( getf state :comment-big )
                 ( progn
                  ( incf ( getf state :comment-big ) )
                  ( setf ( getf state :concatenation )
                          ( concatenate 'string ( getf state :concatenation )
                                       ( list char-current ) ) ) )
                 ( progn
                  ( setf ( getf state :comment-big ) 1 )
                  ( if ( > ( length ( getf state :concatenation ) ) 1 ) 
                      ;; generates a token only if concatenation is not just "#"
                       ( setf ( getf state :token )
                              ( subseq ( getf state :concatenation ) 0
                                      ( - ( length ( getf state :concatenation ) )
                                       2 ) ) ) )
                  ( setf ( getf state :concatenation ) "#|" ) ) ) )
            
            ;; end of a big comment  | #
             ( ( and ( char= char-last #\| ) ( char= char-current #\# )
                  ( getf state :comment-big ) )
             ( progn
              ( decf ( getf state :comment-big ) )
              ( if ( eql 0 ( getf state :comment-big ) ) 
                  ;; comment end here
                   ( progn
                   ( setf ( getf state :comment-big ) nil )
                   ( setf ( getf state :token )
                           ( concatenate 'string ( getf state :concatenation )
                                        ( list char-current ) ) )
                   ( setf ( getf state :concatenation ) nil ) )
                   
                  ;; comment is inserted inside another one
                  ;; continue concatenation of comment
                   ( setf ( getf state :concatenation )
                          ( concatenate 'string ( getf state :concatenation )
                                       ( list char-current ) ) ) ) ) )
            
            ;; start comment inline
             ( ( and ( char= char-current #\; ) ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) ( not ( getf state :escaped ) ) )
             ( progn
              ( setf ( getf state :comment-inline ) t )
              
              ;; any token finish
               ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              
              ;; new token is initialized whith ";"
               ( setf ( getf state :concatenation )
                      ( coerce ( list char-current ) 'string ) ) ) )
            
            ;; beginning of a string
             ( ( and ( eq char-current #\" ) ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) ( not ( getf state :escaped ) ) )
             ( progn
              ( setf ( getf state :string ) t )
              ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              ( setf ( getf state :concatenation )
                      ( coerce ( list char-current ) 'string ) ) ) )
            
            ;; end of a string
             ( ( and ( eq char-current #\" ) ( getf state :string )
                  ( not ( getf state :escaped ) ) )
             ( progn
              ( setf ( getf state :string ) nil )
              ( setf ( getf state :token )
                      ( concatenate 'string ( getf state :concatenation )
                                   ( list char-current ) ) )
              ( setf ( getf state :concatenation ) nil ) ) )
            
            ;; start special
             ( ( and ( char= char-current #\| ) ( not ( getf state :escaped ) )
                  ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) )
             ( progn
              ( setf ( getf state :special ) t )
              
              ;; the multiple escape | can be used also inside symbols
               ( setf ( getf state :concatenation )
                      ( concatenate 'string ( getf state :concatenation )
                                   ( list char-current ) ) ) ) )
            
            ;; end special
             ( ( and ( char= char-current #\| ) ( getf state :special )
                  ( not ( getf state :escaped ) ) )
             ( progn
              ( setf ( getf state :special ) nil )
              ( setf ( getf state :concatenation )
                      ( concatenate 'string ( getf state :concatenation )
                                   ( list char-current ) ) ) ) )
            
            ;; start of lists
             ( ( and ( char= char-current #\( ) ( not ( getf state :escaped ) )
                  ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) )
             ( progn
              ( setf ( getf state :list-start ) t )
              ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              ( setf ( getf state :concatenation ) nil ) ) )
            
            ;; end of lists
             ( ( and ( char= char-current #\) ) ( not ( getf state :escaped ) )
                  ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) ( not ( getf state :string ) )
                  ( not ( getf state :special ) ) )
             ( progn
              ( setf ( getf state :list-end ) t )
              ( if ( getf state :concatenation )
                  ( setf ( getf state :token ) ( getf state :concatenation ) ) )
              ( setf ( getf state :concatenation ) nil ) ) )
            
            ;; update number os recent dashes
             ( ( and ( eq char-current #\\ ) ( not ( getf state :comment-big ) )
                  ( not ( getf state :comment-inline ) ) )
             ( progn
              ( setf ( getf state :escaped ) ( not ( getf state :escaped ) ) )
              ( setf ( getf state :concatenation )
                      ( concatenate 'string ( getf state :concatenation )
                                   ( list char-current ) ) ) ) )
            
            ;; otherwise cancatenate the char to the existent token
             ( t
             ( setf ( getf state :concatenation )
                     ( concatenate 'string ( getf state :concatenation )
                                  ( list char-current ) ) ) ) )
      
      
      ;; ...end of cond
      ;; update number of dashes
       ( if ( not ( eq char-current #\\ ) ) ( setf ( getf state :escaped ) nil ) )
      
      ;; return the new state
       state ) ) )

( defgeneric parse-with-pretty-node
            ( node state-previous char-stream &optional top-level ) )

( defmethod parse-with-pretty-node
           ( ( atom-node pretty-atom ) state-previous char-stream &optional
            top-level )
           ( declare ( ignore top-level ) )
           ( let ( ( state ) )
             
             ;; end of a list, return one level up
              ( when ( getf state-previous :list-end )
               ( return-from parse-with-pretty-node state-previous ) )
             
             ;; beginning of a list, continue one level deep
              ( when ( getf state-previous :list-start )
               ( return-from parse-with-pretty-node
                ( parse-with-pretty-node ( make-instance 'pretty-list )
                 state-previous char-stream ) ) )
             
             ;; update the new state
              ( setf state
                     ( parser-step ( read-char char-stream nil ) state-previous ) )
             
             ;; continue according to the new state
              ( cond 
                   ;;end of stream
                    ( ( not state )
                    ( progn
                     ( setf ( getf state :eof ) t )
                     ( when
                         ( or ( getf state-previous :token )
                             ( getf state-previous :concatenation ) )
                       ( setf ( xml-children atom-node )
                               ( list
                                ( normalize-token
                                 ( concatenate 'string
                                              ( getf state-previous :token )
                                              ( getf state-previous
                                                    :concatenation ) ) ) ) )
                       ( setf ( getf state :node ) atom-node ) ) ) )
                   
                   ;; separator, continue parsing
                    ( ( getf state :separator )
                    ( setf state
                            ( parse-with-pretty-node atom-node state
                             char-stream ) ) )
                   
                   ;; there is a token to retrieve, and parentheses ( not  being the current char
                    ( ( and ( not ( getf state :list-start ) ) ( getf state :token ) )
                    ( progn
                     ( setf ( xml-children atom-node )
                             ( list ( normalize-token ( getf state :token ) ) ) )
                     ( setf ( getf state :node ) atom-node )
                     ( setf ( getf state :token ) nil ) ) )
                   
                   ;; token to retrieve and parentheses ( found
                    ( ( and ( getf state :list-start ) ( getf state :token ) )
                    ( let ( ( list-type ( possible-list-type ( getf state :token ) ) ) )
                      ( when list-type
                        ( setf state
                                ( parse-with-pretty-node atom-node state
                                 char-stream ) ) )
                      ( unless list-type
                        ( progn
                         ( setf ( xml-children atom-node )
                                 ( list ( normalize-token ( getf state :token ) ) ) )
                         ( setf ( getf state :node ) atom-node )
                         ( setf ( getf state :token ) nil ) ) ) ) )
                   
                   ;; continue parsing with the same pretty-atom node
                    ( t
                    ( setf state
                            ( parse-with-pretty-node atom-node state
                             char-stream ) ) ) )
             state ) )

( defmethod parse-with-pretty-node
           ( ( list-node pretty-list ) state char-stream &optional
            ( top-level nil ) )
           "Call recursively parse-with-pretty-atom and 
concatenates the result as children of list-node 
Does not call parser directly."
           
           ;; set the list type
            ( let ( ( list-type ( or ( possible-list-type ( getf state :token ) ) "" ) ) )
             ( setf ( xml-attributes list-node )
                     ( list ( cons +LIST-TYPE+ list-type ) ) )
             
             ;; token was used to build list type, discard it
              ( setf ( getf state :token ) nil )
             
             
             ;; need to set :list-start to nil, because
             ;; parse-with-pretty-atom would loop forever
              ( setf ( getf state :list-start ) nil )
             ( loop while ( progn
                          ( setf state
                                  ( parse-with-pretty-node
                                   ( make-instance 'pretty-atom ) state
                                   char-stream ) )
                          ( and state ( getf state :node )
                               ( xml-append-child list-node ( getf state :node ) )
                               ( not ( getf state :list-end ) )
                               ( not ( getf state :eof ) ) ) ) )
             ( setf ( getf state :node ) list-node )
             
             
             ;; need to set :list-end to nil, because
             ;; parse-with-pretty-atom would loop forever
              ( and top-level ( getf state :list-end )
                  ( setf ( getf state :extra-end-parenthesis ) t ) )
             ( setf ( getf state :list-end ) nil )
             state ) )

( defun parse-to-pretty-lisp
       ( text-code &key ( input-type :string ) ( for-extension nil ) )
  "Converts text-code to a list of pretty-list and pretty-atoms.
The input-type of text-code can be :stream or :string"
  ( let (
        ;; accepts input as stream or text
         ( text-stream
         ( if ( eq input-type :stream ) text-code
             ( make-string-input-stream text-code ) ) )
        
        ;; the return list
         ( list-pretty-xml '( ) )
        ( state )
        ( past ) )
    ( loop while ( and
                 ( setf state
                         ( parse-with-pretty-node ( make-instance 'pretty-list )
                          ( list :char #\Space :separator t :past past )
                          text-stream t ) )
                 ( if for-extension
                     ( if
                      ( pretty-atom-p ( car ( xml-children ( getf state :node ) ) ) )
                      ( progn ( setf past ( getf state :past ) ) )
                      ( progn
                       ( if past
                           ( setf list-pretty-xml
                                   ( append list-pretty-xml
                                           ( list
                                            ( make-instance 'pretty-atom
                                             :children ( list past ) ) ) ) ) )
                       ( setf list-pretty-xml
                               ( append list-pretty-xml
                                       ( xml-children ( getf state :node ) ) ) )
                       ( setf past nil )
                       ( when ( getf state :extra-end-parenthesis )
                         
                         
                         
                         ;; this is a compensation procedure for unbalenced right parenthesis
                         ;; results in ( being added to the beginning of file to compensate this extra )
                         ;; the opposite, adding ) to compensate extra ( is done automatically, since each ( originates a list
                          ( setf list-pretty-xml
                                 ( list
                                  ( make-instance 'pretty-list :children
                                   list-pretty-xml :attributes
                                   ( list ( cons +LIST-TYPE+ "" ) ) ) ) ) )
                       t ) )
                     ( progn
                      ( setf list-pretty-xml
                              ( append list-pretty-xml
                                      ( xml-children ( getf state :node ) ) ) )
                      ( setf past nil )
                      ( when ( getf state :extra-end-parenthesis )
                        
                        
                        
                        ;; this is a compensation procedure for unbalenced right parenthesis
                        ;; results in ( being added to the beginning of file to compensate this extra )
                        ;; the opposite, adding ) to compensate extra ( is done automatically, since each ( originates a list
                         ( setf list-pretty-xml
                                ( list
                                 ( make-instance 'pretty-list :children
                                  list-pretty-xml :attributes
                                  ( list ( cons +LIST-TYPE+ "" ) ) ) ) ) )
                      t ) )
                 ( not ( getf state :eof ) ) ) )
    ( if past
        ( setf list-pretty-xml
                ( append list-pretty-xml
                        ( list
                         ( make-instance 'pretty-atom :children ( list past ) ) ) ) ) )
    list-pretty-xml ) )

( defun pretty-nodes-to-xml-nodes ( node )
  "Outputs a xml-node"
  ( cond
   ( ( pretty-atom-p node )
    ( make-instance 'xml-node :tag ( xml-tag node ) :attributes
     ( append ( xml-attributes node )
             ( list ( cons +TYPE+ +CLASS-ATOM+ ) ( cons "class" +CLASS-ATOM+ ) ) )
     :children ( xml-children node ) ) )
   ( ( pretty-list-p node )
    ( make-instance 'xml-node :tag ( xml-tag node ) :attributes
     ( append ( xml-attributes node )
             ( list ( cons +TYPE+ +CLASS-LIST+ ) ( cons "class" +CLASS-LIST+ ) ) )
     :children ( mapcar #'pretty-nodes-to-xml-nodes ( xml-children node ) ) ) )
   ( ( consp node ) ( mapcar #'pretty-nodes-to-xml-nodes node ) ) ) )

( defun code-string-to-xml ( code-string )
  ( pretty-nodes-to-xml-nodes ( parse-to-pretty-lisp code-string ) ) )