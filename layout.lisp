

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

( defparameter +MARGIN-X+ 12 "left margin between atom and containing list" )

( defparameter +MARGIN-Y+ 5 "top margin between atom and containing list" )

( defparameter +MARGIN-X2+ 12 "right margin between atom and containing list" )

( defparameter +MARGIN-Y2+ 5 "bottom margin between atom and containing list" )

( defparameter +TEXT-HEIGHT+ 9 )

( defparameter +FACTOR-LEN+ 8 "spacing + width of characters" )

( defparameter +FACTOR-H+ 3 )

( defparameter +margin-list-desc+ 2 )

( defparameter +RX+ "5" )

( defparameter +RY+ "8" )

( defparameter *EDIT-MEMORY* nil )

( defparameter +HYPERSPEC+ nil )

( defparameter +HYPERSPEC-LIST+ nil )

( defvar +HYPERSPEC-FILE+
  ( concatenate 'string +source-folder+ "public/X_AllSym.htm" )
  "location of hyperspecification links file" )

( defparameter +LISPWORKS-HYPERSPEC-URL+
  "http://www.lispworks.com/documentation/HyperSpec/"
  "hyperspecification website" )

( defparameter +EVENT-FUN+ "roxEvent" )

( defparameter +horizontal-helders+
  ( mapcar #'( lambda ( i ) ( format nil "~A" i ) )
          '( :defparameter :defvar :defconstant ) )
  "If one of these tokens is the first element of a list, 
the list will assume an horizontal layout by default." )

( defparameter +vertical-helders+
  ( mapcar #'( lambda ( i ) ( format nil "~A" i ) )
          '( :let :let* :progn :cond :dolist :dotimes :block :when :eval-when
           :unless :if :lambda :mapcar :maplist :map :reduce :loop :or :and :do
           :labels :flet :flet* ) )
  "If one of these tokens is the first element of a list, 
the list will assume a vertical layout by default." )

( defun make-svg-children ( xml )
  "With the list of pretty-nodes xml, constructs the 
<rect> and <text> elements to be inserted as children
of a <svg> element."
  ( if ( pretty-atom-p xml )
      ( let* ( ( str ( xml-value xml ) ) ( splitted ( string-split str #\Newline ) ) )
       ( if ( = 1 ( length splitted ) )
           ( list xml )
           ( let ( ( counter -1 )
                 ( y ( read-from-string ( xml-attribute-get xml :y ) ) )
                 ( x ( xml-attribute-get xml :x ) ) )
             ( list
              ( make-instance 'pretty-atom :attributes ( xml-attributes xml )
               :children
               ( mapcar
                #'( lambda ( span )
                  ( make-instance 'xml-node :tag "tspan" :attributes
                   ( list ( cons :x x )
                         ( cons :y
                               ( format nil "~A"
                                       ( + y
                                        ( * ( incf counter )
                                         ( + +TEXT-HEIGHT+ +MARGIN-Y+ ) ) ) ) ) )
                   :children ( list span ) ) )
                splitted ) ) ) ) ) )
      ( let ( ( newlist
             ( list
              ( make-instance 'pretty-list :attributes ( xml-attributes xml ) ) ) ) )
        ( dolist ( child ( xml-children xml ) newlist )
          ( setf newlist ( append ( make-svg-children child ) newlist ) ) ) ) ) )

( defmethod xml-node-to-pretty-node ( ( xml xml-node ) &optional parent )
  ( let*
   ( ( tag ( xml-tag xml ) )
    ( pretty-node
     ( make-instance
      ( if ( string-equal tag +LIST+ )
          'pretty-list
          'pretty-atom ) ) ) )
   ( setf ( xml-parent pretty-node ) parent
         ( xml-attributes pretty-node ) ( xml-attributes xml )
         ( xml-properties pretty-node ) ( xml-properties xml )
         ( xml-children pretty-node )
           ( mapcar #'( lambda ( child ) ( xml-node-to-pretty-node child pretty-node ) )
                   ( xml-children xml ) ) )
   pretty-node ) )

( defmethod xml-node-to-pretty-node ( string-node &optional parent )
  
  ;; TODO use declare ignore
   ( format nil "~A" ( or string-node parent ) ) )

( defgeneric size-pretty
    ( node ) )

( defgeneric position-pretty
    ( node ) )

( defgeneric pretty-layout
    ( node ) )

( defmethod size-pretty ( ( node xml-node ) )
  "Returns a list '(width height) in px for the pretty-list/atom"
  ( declare ( optimize ( speed 3 ) ) )
  ( let ( ( tam ( xml-property-get node :size ) ) )
    (  ;; if the size is already set as a property, return it
       ;; this avoids to compute it down to the last children.
        if tam tam
     ( let ( ( posdim ( list ( list 0 0 0 0 ) ) )
           ( margins
            ( if ( pretty-atom-p node )
                ( list 0 0 )
                ( list +MARGIN-X2+ +MARGIN-Y2+ ) ) ) )
       ( if ( and ( pretty-list-p node ) ( null ( xml-children node ) ) )
           ( list ( + +MARGIN-X+ +MARGIN-X2+ )
                 ( + +MARGIN-Y+ +TEXT-HEIGHT+ +MARGIN-Y2+ ) )
           ( dolist
               ( child ( xml-children node )
                      ( mapcar #'+ ( farthest posdim ) margins ) )
             ( setf posdim
                     ( cons ( append ( size-pretty child ) ( position-pretty child ) )
                           posdim ) ) ) ) ) ) ) )

( defmethod size-pretty ( string-node )
  ( declare ( optimize ( speed 3 ) ) )
  ( let ( ( newlines ( count #\Newline string-node ) ) )
    ( if ( = newlines 0 )
        ( list ( * +FACTOR-LEN+ ( length string-node ) ) +TEXT-HEIGHT+ )
        ( list
         ( * +FACTOR-LEN+
          ( apply #'max ( mapcar #'length ( string-split string-node #\Newline ) ) ) )
         ( + +TEXT-HEIGHT+ ( * ( + +TEXT-HEIGHT+ +MARGIN-Y+ ) newlines ) ) ) ) ) )

( defmethod position-pretty ( ( xml xml-node ) )
  ( declare ( optimize ( speed 3 ) ) )
  ( let ( ( pos ( xml-property-get xml :position ) ) )
    ( if pos
        pos
        ( let ( ( parent ( xml-parent xml ) )
              ( list-type-length
               ( * +FACTOR-LEN+ ( length ( xml-attribute-get xml :listtype ) ) ) ) )
          ( if ( null parent )
              ( list ( + list-type-length +MARGIN-X+ ) +FACTOR-H+ )
              ( let*
               ( ( children ( xml-children parent ) ) ( inib ( pretty-layout parent ) )
                ( idx ( position xml children ) ) )
               ( when ( > list-type-length 0 )
                 ( incf list-type-length +margin-list-desc+ ) )
               ( if ( = idx 0 )
                   ( list ( + list-type-length +MARGIN-X+ ) +MARGIN-Y+ )
                   ( mapcar #'+
                           ( mapcar #'* inib
                                   ( mapcar #'+
                                           ( position-pretty
                                            ( nth ( - idx 1 ) children ) )
                                           ( size-pretty
                                            ( nth ( - idx 1 ) children ) ) ) )
                           ( list +MARGIN-X+ +MARGIN-Y+ )
                           ( list list-type-length 0 ) ) ) ) ) ) ) ) )

( defmethod position-pretty ( string-node ) '( 0 0 ) )

( defun only-pretty-atoms ( node )
  ( let ( ( ret t ) ( list-of-xml ( xml-children node ) ) )
    ( dolist ( child list-of-xml ) ( setf ret ( and ret ( pretty-atom-p child ) ) ) )
    ret ) )

( defun only-pretty-lists ( node )
  ( let ( ( ret t ) ( list-of-xml ( xml-children node ) ) )
    ( dolist ( child list-of-xml ) ( setf ret ( and ret ( pretty-list-p child ) ) ) )
    ret ) )

( defun is-defclass-parameter-list ( node )
  ( when ( pretty-list-p node )
    ( let ( ( parent node ) )
      ( dotimes ( i 2 )
        ( if parent
            ( setf parent ( xml-parent parent ) ) ) )
      ( if ( and parent ( pretty-atom-p ( car ( xml-children parent ) ) ) )
          ( equalp "defclass" ( xml-value ( car ( xml-children parent ) ) ) ) ) ) ) )

( defun is-arguments-list ( node )
  ( when ( pretty-list-p node )
    ( let*
     ( ( parent ( xml-parent node ) )
      ( siblings
       ( if parent
           ( xml-children parent )
           ( return-from is-arguments-list nil ) ) )
      ( helder-text
       ( and ( pretty-atom-p ( car siblings ) ) ( xml-value ( car siblings ) ) ) ) )
     ( and ( = ( position node siblings ) 2 )
          ( find helder-text '( "defun" "defmacro" "defmethod" "defgeneric" ) :test
                #'equalp )
          t ) ) ) )

( defmethod pretty-layout ( node ) '( 1 0 ) )

( defmethod pretty-layout ( ( node pretty-list ) )
  ( declare ( optimize ( speed 3 ) ) )
  ( let ( ( v '( 0 1 ) )
        ( h '( 1 0 ) )
        ( child ( car ( xml-children node ) ) )
        ( child-text nil )
        ( cur-lay ( xml-property-get node :layout ) ) )
    ( if ( pretty-atom-p child )
        ( setf child-text ( xml-value child ) ) )
    ( cond  ;; if the layout is user defined
            ( cur-lay cur-lay ) ( ( equalp child-text "#|v|#" ) v )
          ( ( equalp child-text "#|h|#" ) h )  ;; if first child is a list
                                            ( ( pretty-list-p child ) v )
          ( ( find child-text +horizontal-helders+ :test #'string-equal ) h ) 
          ;; only atoms, strings and numbers of elements inside list is small: horizontal
           ( ( and ( only-pretty-atoms node ) ( < ( length ( xml-children node ) ) 5 ) ) h )
           ;; if the first element of the list is one of these: vertical
            ( ( find child-text +vertical-helders+ :test #'string-equal ) v ) 
          ;; for classes
           ( ( is-defclass-parameter-list node ) h )  ;; arguments list
                                                  ( ( is-arguments-list node ) h )
          
          ;; comments as the first element inside the list: vertical
           ( ( and ( > ( length child-text ) 0 )
                ( string= ";" ( subseq child-text 0 1 ) ) )
           v )
           ;; if there are only lists: vertical
            ( ( only-pretty-lists node ) v ) 
          ;; only atoms, strings and numbers of elements inside list is small: horizontal
           ( ( and ( only-pretty-atoms node ) ( < ( length ( xml-children node ) ) 10 ) )
           h )
           ;; with-output-to-string, with... etc
            ( ( > ( mismatch child-text "with'" :test #'string-equal ) 3 ) v ) 
          ;; if the number of elements is small, horizontal
           ( ( < ( length ( xml-children node ) ) 4 ) h )  ;; default: vertical
                                                   ( t v ) ) ) )

( defgeneric remove-all-positioning-properties
    ( xml ) )

( defmethod remove-all-positioning-properties ( a-string ) )

( defmethod remove-all-positioning-properties ( ( xml xml-node ) )
  ( dolist ( par '( :position :size :layout ) nil ) ( xml-property-set xml par nil ) )
  ( mapcar #'remove-all-positioning-properties ( xml-children xml ) )
  xml )

( defgeneric set-all-positioning-properties
    ( xml ) )

( defmethod set-all-positioning-properties ( xml ) )

( defmethod set-all-positioning-properties ( ( xml xml-node ) )
  ( mapcar #'set-all-positioning-properties ( xml-children xml ) )
  ( xml-property-set xml :layout ( pretty-layout xml ) )
  ( xml-property-set xml :size ( size-pretty xml ) )
  ( xml-property-set xml :position ( position-pretty xml ) ) )

( defgeneric set-predefined-layout
    ( xml ) )

( defmethod set-predefined-layout ( str ) )

( defmethod set-predefined-layout ( ( xml pretty-list ) )
  ( if ( xml-attribute-get xml :layout )
      ( xml-property-set xml :layout
       ( read-from-string ( xml-attribute-get xml :layout ) ) ) )
  ( mapcar #'set-predefined-layout ( xml-children xml ) ) )

( defun set-events-helper ( xml )
  ( declare ( optimize ( speed 3 ) ) )
  ( xml-attribute-set xml "onclick"
   ( format nil "~A('~A','~A')" +EVENT-FUN+ ( xml-attribute-get xml :roxid )
           "dblclick" ) ) )

( defgeneric set-events
    ( xml ) )

( defmethod set-events ( obj ) ( and ( consp obj ) ( mapcar #'set-events obj ) ) )

( defmethod set-events ( ( xml pretty-list ) )
  ( set-events-helper xml )
  ( mapcar #'set-events ( xml-children xml ) ) )

( defmethod set-events ( ( xml pretty-atom ) ) ( set-events-helper xml ) )

( defgeneric set-pretty-dimensions
    ( xml ) )

( defgeneric set-absolute-pretty-dimensions
    ( string-node ) )

( defun get-hyperspec ( )
  ( unless +HYPERSPEC+
    ( let ( ( hyper ( make-hash-table :test #'equalp ) )
          ( xml ( xml-parse-from-string ( read-file +HYPERSPEC-FILE+ ) ) )
          ( term ) )
      ( setf +HYPERSPEC-LIST+ nil )
      ( dolist ( item ( xml-children xml ) )
        ( setf term ( format-and-ucase ( car ( xml-children item ) ) ) )
        ( setf ( gethash term hyper ) ( xml-attribute-get item :href ) )
        ( push term +HYPERSPEC-LIST+ ) )
      ( setf +HYPERSPEC-LIST+ ( reverse +HYPERSPEC-LIST+ ) )
      ( setf +HYPERSPEC+ hyper ) ) )
  +HYPERSPEC+ )

( defgeneric set-atom-classes
    ( obj ) )

( defmethod set-atom-classes ( obj )
  ( and ( consp obj ) ( mapcar #'set-atom-classes obj ) ) )

( defmethod set-atom-classes ( ( xml pretty-list ) )
  ( mapcar #'set-atom-classes ( xml-children xml ) ) )

( defun append-new-to-string ( original new )
  ( if ( search new original :test #'equalp )
      original
      ( format nil "~A ~A" original new ) ) )

( defmethod set-atom-classes ( ( xml pretty-atom ) )
  ( declare ( optimize ( speed 3 ) ) )
  ( let*
   ( ( txt ( car ( xml-children xml ) ) ) ( txt2 ( string-trim "#'" txt ) )
    ( hyper ( get-hyperspec ) )
    ( cur-classes ( or ( xml-attribute-get xml :class ) "" ) ) )
   ( cond
    ( ( string-equal txt "nil" )
     ( setf cur-classes ( append-new-to-string cur-classes "nil" ) ) )
    ( ( gethash ( format-and-ucase txt2 ) hyper )
     ( setf cur-classes ( append-new-to-string cur-classes "hyperspec" ) ) )
    ( ( and ( > ( length txt ) 0 ) ( char= #\: ( char txt 0 ) ) )
     ( setf cur-classes ( append-new-to-string cur-classes "keyword" ) ) )
    ( ( and ( > ( length txt ) 0 ) ( char= #\" ( char txt 0 ) ) )
     ( setf cur-classes ( append-new-to-string cur-classes "string" ) ) )
    ( ( or ( and ( > ( length txt ) 0 ) ( char= ( char txt 0 ) #\; ) )
         ( and ( > ( length txt ) 1 ) ( char= ( char txt 0 ) #\# )
              ( char= ( char txt 1 ) #\| ) ) )
     ( setf cur-classes ( append-new-to-string cur-classes "comment" ) ) )
    ( ( ignore-errors ( numberp ( read-from-string txt ) ) )
     ( setf cur-classes ( append-new-to-string cur-classes "number" ) ) ) )
   ( xml-attribute-set xml :class ( string-trim " " cur-classes ) ) ) )

( defmethod process-dimensioning ( a-string ) )

( defmethod process-dimensioning ( ( xml xml-node ) )
  ( remove-all-positioning-properties xml )
  ( set-predefined-layout xml )
  ( set-all-positioning-properties xml )
  ( setf xml ( set-pretty-dimensions xml ) )
  ( setf xml ( set-absolute-pretty-dimensions xml ) )
  ( set-atom-classes xml )
  ( set-events xml )
  xml )

( defmethod set-id-all-family ( str &optional overwrite )
  ( declare ( ignore str overwrite ) ) )

( defmethod set-id-all-family ( ( xml xml-node ) &optional ( overwrite t ) )
  ( xml-id-set xml :roxid overwrite )
  ( mapcar #'( lambda ( c ) ( set-id-all-family c overwrite ) ) ( xml-children xml ) ) )

( defmethod set-pretty-dimensions ( string-node ) )

( defmethod set-pretty-dimensions ( ( xml xml-node ) )
  ( let ( ( pos ( position-pretty xml ) )
        ( tam ( size-pretty xml ) )
        ( is-atom ( pretty-atom-p xml ) )
        ( is-list ( pretty-list-p xml ) ) )
    ( xml-id-set xml :roxid nil )
    ( when ( xml-parent xml )
      ( xml-attribute-set xml :parent
       ( xml-attribute-get ( xml-parent xml ) :roxid ) ) )
    ( xml-attribute-set xml :x ( format nil "~A" ( first pos ) ) )
    ( xml-attribute-set xml :y ( format nil "~A" ( second pos ) ) )
    ( xml-attribute-set xml :width ( format nil "~A" ( first tam ) ) )
    ( xml-attribute-set xml :height ( format nil "~A" ( second tam ) ) )
    ( when is-list
      ( xml-attribute-set xml :rx +RX+ )
      ( xml-attribute-set xml :ry +RY+ ) )
    ( when is-atom
      ( xml-attribute-set xml :textlength ( format nil "~A" ( first tam ) ) ) )
    ( mapcar #'set-pretty-dimensions ( xml-children xml ) )
    xml ) )

( defmethod set-absolute-pretty-dimensions ( string-node ) )

( defmethod set-absolute-pretty-dimensions ( ( xml xml-node ) )
  ( let ( ( parent-x 0 )
        ( parent-y 0 )
        ( parent ( xml-parent xml ) )
        ( x ( read-from-string ( xml-attribute-get xml :x ) ) )
        ( y ( read-from-string ( xml-attribute-get xml :y ) ) ) )
    ( when parent
      ( setf parent-x ( read-from-string ( xml-attribute-get parent :x ) )
            parent-y ( read-from-string ( xml-attribute-get parent :y ) ) ) )
    ( setf x ( + x parent-x )
          y ( + y parent-y ) )
    ( when ( pretty-atom-p xml ) ( setf y ( + y +TEXT-HEIGHT+ ) ) )
    ( xml-attribute-set xml :x ( format nil "~A" x ) )
    ( xml-attribute-set xml :y ( format nil "~A" y ) )
    ( mapcar #'set-absolute-pretty-dimensions ( xml-children xml ) )
    xml ) )

( defun insert-list-type ( svg-list )
  ( declare ( optimize ( speed 3 ) ) )
  ( let ( ( ret-svg-list nil ) ( size 0 ) ( xml-type-desc nil ) ( type-text nil ) )
    ( dolist ( element svg-list ( reverse ret-svg-list ) )
      ( push element ret-svg-list )
      ( setf type-text ( xml-attribute-get element +LIST-TYPE+ ) )
      ( setf size ( * +FACTOR-LEN+ ( length type-text ) ) )
      ( when ( and ( > size 0 ) ( pretty-list-p element ) )
        ( setf xml-type-desc
                ( make-instance 'pretty-descriptor :children ( list type-text ) ) )
        ( xml-id-set xml-type-desc :roxid )
        ( xml-attribute-set xml-type-desc :textLength
         ( car ( size-pretty type-text ) ) )
        ( xml-attribute-set xml-type-desc :class "listdescriptor" )
        ( xml-attribute-set xml-type-desc :type "listdescriptor" )
        ( xml-attribute-set xml-type-desc :tgt
         ( xml-attribute-get element :roxid ) )
        ( xml-attribute-set xml-type-desc :y
         ( format nil "~A"
                 ( + +TEXT-HEIGHT+ +MARGIN-Y+
                  ( read-from-string ( xml-attribute-get element :y ) ) ) ) )
        ( xml-attribute-set xml-type-desc :x
         ( format nil "~A"
                 ( - ( read-from-string ( xml-attribute-get element :x ) )
                  ( + size +margin-list-desc+ ) ) ) )
        ( push xml-type-desc ret-svg-list ) ) ) ) )

( defmethod wrap-with-root-svg ( svg )
  "surround the element with a svg node and a basis rect"
  ( let ( ( width ( xml-attribute-get svg :width ) )
        ( height ( xml-attribute-get svg :height ) )
        ( x ( xml-attribute-get svg :x ) )
        ( y ( xml-attribute-get svg :y ) )
        ( id ( format nil "svg~A" ( xml-attribute-get svg :roxid ) ) )
        ( mainrect nil ) )
    ( if ( pretty-list-p svg )
        ( setf mainrect
                ( list
                 ( make-instance 'xml-node :tag "rect" :attributes
                  ( list ( cons :x x ) ( cons :y y ) ( cons :width width )
                        ( cons :height height ) ( cons :rx +RX+ ) ( cons :ry +RY+ )
                        ( cons :class "mainrect" ) ) ) ) ) )
    ( setf width ( + +MARGIN-X+ ( read-from-string width ) ( read-from-string x ) ) )
    ( setf height ( + +MARGIN-Y+ ( read-from-string height ) ( read-from-string y ) ) )
    ( setf svg ( reverse ( make-svg-children svg ) ) )
    ( setf svg ( insert-list-type svg ) )
    ( setf svg
            ( make-instance 'xml-node :tag "svg" :children
             ( append mainrect svg ) ) )
    ( xml-attribute-set svg :xmlns "http://www.w3.org/2000/svg" )
    ( xml-attribute-set svg :version "1.1" )
    ( xml-attribute-set svg :id id )
    ( xml-attribute-set svg :x "0" )
    ( xml-attribute-set svg :y "0" )
    ( xml-attribute-set svg :height ( format nil "~A" height ) )
    ( xml-attribute-set svg :width ( format nil "~A" width ) ) )
  svg )

( defun parse-pretty-atom-2 ( xml )
  
  ;; comments the node string and parse it
   ( let ( ( str
         ( concatenate 'string '( #\; )
                      ( substitute #\Space #\Newline ( xml-value xml ) :test
                                  #'char= ) ) ) )
    ( setf str ( ignore-errors ( code-string-to-xml str ) ) )
    ( cond ( ( consp str ) ( mapcar #'xml-node-to-pretty-node str ) )  
          ;; remove because the parsing should return
          ;; always a list
           ( ( and ( xml-node-p str ) ( setf str ( xml-node-to-pretty-node str ) )
                ( pretty-list-p str ) )
           ( xml-children str ) )
           ;; should not happen
            ( t nil ) ) ) )

( defmethod parse-pretty-atom ( xml ) ( list xml ) )

( defmethod parse-pretty-atom ( ( xml pretty-atom ) )
  ( let ( ( str ( xml-value xml ) ) )
    ( setf str ( ignore-errors ( code-string-to-xml str ) ) )
    ( cond ( ( consp str ) ( mapcar #'xml-node-to-pretty-node str ) )  
          ;; FIXME remove because the parsing should return
          ;; always a list
           ( ( and ( xml-node-p str ) ( setf str ( xml-node-to-pretty-node str ) )
                ( pretty-list-p str ) )
           ( xml-children str ) )
           ;; in case of error
            ( t ( parse-pretty-atom-2 xml ) ) ) ) )