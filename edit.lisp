

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

;;; These are the main functions to deal with code editing 

( in-package :pretty-lisp )

( defun set-as-focus-element ( xml )
  "sets a focus mark on the xml element."
  ( if ( xml-node-p xml ) ( xml-attribute-set xml :focus "focus" ) ) )

( defun remove-as-focus-element ( xml )
  "remove the focus mark from the xml element."
  ( if ( xml-node-p xml ) ( xml-attribute-set xml :focus nil ) ) )

( defun get-focus-element ( editing-nodes )
  "get the element which has the focus."
  ( find-by-attribute editing-nodes :focus "focus" ) )

( defun navigate ( elem parent editing-nodes operation )
  "navigating trough nodes with arrow keys, or other key combinations."
  ( let*
   ( ( siblings ( if parent ( xml-children parent ) editing-nodes ) )
    ( pos ( position elem siblings ) ) ( len ( length siblings ) )
    ( prev-sibling ( nth ( mod ( 1- pos ) len ) siblings ) )
    ( next-sibling ( nth ( mod ( 1+ pos ) len ) siblings ) )
    ( child ( and ( pretty-list-p elem ) ( car ( xml-children elem ) ) ) ) ( v '( 0 1 ) )
    ( h '( 1 0 ) ) ( layout ( or ( and parent ( xml-property-get parent :layout ) ) v ) ) )
   ( when ( eq prev-sibling elem )
     ( setf prev-sibling nil )
     ( setf next-sibling nil ) )
   ( cond
    ( ( string-equal operation :up )
     ( or ( and ( equal layout v ) prev-sibling )
         ( and ( or ( equal layout h ) ( = len 1 ) ) parent ) elem ) )
    ( ( string-equal operation :down )
     ( or ( and ( equal layout v ) next-sibling )
         ( and ( or ( equal layout h ) ( = len 1 ) ) ( or child parent ) ) elem ) )
    ( ( string-equal operation :left )
     ( or ( and ( equal layout h ) prev-sibling )
         ( and ( or ( equal layout v ) ( = len 1 ) ) parent ) elem ) )
    ( ( string-equal operation :right )
     ( or ( and ( equal layout h ) next-sibling )
         ( and ( or ( equal layout v ) ( = len 1 ) ) child ) elem ) )
    ( t elem ) ) ) )

( defun minimize-and-expand ( previous-response ancestor editing-nodes operation )
  "Changing the container height to have some kind of code-colapse and code-expand
To only one container or all of them"
  ( if
   ( find operation '( :collapse :collapseall :expand :expandall ) :test
         #'string-equal )
   ( let*
    ( ( only-one ( find operation '( :collapse :expand ) :test #'string-equal ) )
     ( to-process ( if only-one ( list ancestor ) editing-nodes ) ) )
    ( wrap
     ( cons previous-response
           ( mapcar
            #'( lambda ( node )
              ( jquery-2 ( format nil "#svg~A" ( xml-attribute-get node :roxid ) )
               "attr" "height"
               ( format nil "~A"
                       ( if
                        ( find operation '( :collapse :collapseall ) :test
                              #'string-equal )
                        ( min 70
                             ( + +FACTOR-H+
                              ( read-from-string
                               ( xml-attribute-get node :height ) ) ) )
                        ( + 1 +MARGIN-Y+
                         ( read-from-string ( xml-attribute-get node :height ) )
                         ( read-from-string ( xml-attribute-get node :y ) ) ) ) ) ) )
            to-process ) ) ) )
   previous-response ) )

( defgeneric make-triggered-event ( obj evtType ) )

( defmethod make-triggered-event ( obj evtType ) )

( defmethod make-triggered-event ( ( xml xml-node ) evtType )
           ( js-predefined "roxEvent" ( xml-attribute-get xml :roxid ) evtType ) )

( defun transpose-layout ( event-element )
  "Switches the layout of the list {horizontal, vertical}.
The internal representation is a list { (0 1), (1 0) }"
  ( let ( ;; if the layout is user defined, then get it
          ( layout ( xml-attribute-get event-element :layout ) ) )
    
    
    ;; The attribute is a string. Need to convert to a list
    ;; If the layout is not user-defined get the default
     ( if layout ( setf layout ( read-from-string layout ) )
        ( setf layout ( pretty-layout event-element ) ) )
    
    ;; the transposition is the simple reverse of the layout
     ( setf layout ( reverse layout ) )
    
    ;; set the new layout as an attribute of the element
     ( xml-attribute-set event-element :layout ( format nil "~A" layout ) ) ) )

( defun free-element
       ( event-element eltype parent siblings pos event-svg editing-nodes )
  "If the element is a list, this functions replaces the list with its children
If it is a comment, it removes the comment marks,
so that tha comment words are changed to code"
  ( declare ( ignore event-svg ) )
  ( let ( ( freed-elements ) ( insert-position ) )
    ( when ( string-equal :atom eltype )
      ( let ( ( text ( trim-comment-marks ( xml-value event-element ) ) ) )
        ( xml-value-set event-element ( if text text "" ) )
        ( setf freed-elements ( parse-pretty-atom event-element ) ) ) )
    ( when ( string-equal :list eltype )
      ( setf freed-elements ( xml-children event-element ) ) )
    ( set-as-focus-element ( car freed-elements ) )
    ( when parent
      ( setf ( xml-children parent ) nil )
      ( dolist
          ( child
           ( append ( subseq siblings 0 pos ) freed-elements
                   ( subseq siblings ( + pos 1 ) ) ) )
        ( xml-append-child parent child ) ) )
    ( unless parent
      ( dolist ( elem freed-elements ) ( setf ( xml-parent elem ) nil ) )
      ( setf insert-position ( position event-element editing-nodes ) )
      ( setf editing-nodes
              ( append ( subseq editing-nodes 0 insert-position ) freed-elements
                      ( subseq editing-nodes ( 1+ insert-position ) ) ) ) )
    editing-nodes ) )

( defun delete-element ( event-element parent siblings pos editing-nodes )
  ( if parent
      ( progn
       ( setf ( xml-children parent ) ( remove event-element siblings ) )
       ( if ( xml-children parent )
           ( set-as-focus-element
            ( nth ( min pos ( 1- ( length ( xml-children parent ) ) ) )
                 ( xml-children parent ) ) )
           ( set-as-focus-element parent ) ) )
      
      ;; else (no parent - top level)
       ( let ( ( insert-position ( position event-element editing-nodes ) ) )
        
        ;; avoid empty editing nodes by inserting a dummy node
         ( if ( = 1 ( length editing-nodes ) )
            ( setf editing-nodes
                    ( append editing-nodes
                            ( list
                             ( make-instance 'pretty-atom :children
                              ( list "#||#" ) :attributes
                              ( list ( cons :class +CLASS-ATOM+ )
                                    ( cons :type +CLASS-ATOM+ ) ) ) ) ) ) )
        ( setf editing-nodes ( remove event-element editing-nodes ) )
        ( set-as-focus-element
         ( nth ( min insert-position ( 1- ( length editing-nodes ) ) )
              editing-nodes ) ) ) )
  editing-nodes )

( defun update-element
       ( event-element eltype parent siblings pos event-svg editing-nodes )
  "Parses the event-element text and updates the code xml structure"
  ( let ( ;; the new text is the value received with the event-svg
          ( text ( or ( xml-value event-svg ) "" ) ) ( new-elements ) ( insert-position ) )
    
    ;; if the element is a list the text is the list type
     ( when ( string-equal :list eltype )
      ( xml-attribute-set event-element :listtype text )
      ( set-as-focus-element event-element ) )
    
    
    ;; if its an atom the new text must be parsed, and the resulting elements
    ;; inserted in the place of the element
     ( when ( string-equal :atom eltype )
      ( when ( = 0 ( length text ) )
        ( return-from update-element
         ( delete-element event-element parent siblings pos editing-nodes ) ) )
      ( xml-value-set event-element text )
      ( setf new-elements ( parse-pretty-atom event-element ) )
      ( set-as-focus-element ( car new-elements ) )
      ( when parent
        ( setf ( xml-children parent ) nil )
        ( dolist
            ( child
             ( append ( subseq siblings 0 pos ) new-elements
                     ( subseq siblings ( + pos 1 ) ) )
             nil )
          ( xml-append-child parent child ) ) )
      ( unless parent
        ( dolist ( elem new-elements ) ( setf ( xml-parent elem ) nil ) )
        ( setf insert-position ( position event-element editing-nodes ) )
        ( setf editing-nodes
                ( append ( subseq editing-nodes 0 insert-position ) new-elements
                        ( subseq editing-nodes ( 1+ insert-position ) ) ) ) ) )
    editing-nodes ) )

( defun replace-newlines
       ( text &key ( to-replace ( list #\Newline ) ) ( replace-with #\Space ) )
  ( reduce
   #'( lambda ( txt replc ) ( substitute replace-with replc txt :test #'char= ) )
   to-replace :initial-value text ) )

( defun comment-element ( event-element parent pos )
  "Returns a pretty-atom with the textual code represented by event-element preceeded by a comment mark"
  ( let*
   ( 
    ;; the comment text is the textual representation of the code
    ;; preceeded with a comment mark
     ( text
     ( replace-newlines
      ( concatenate 'string ";" ( xml-to-code-string event-element ) ) ) )
     
    ;; the internal representation of the comment is an pretty-atom
    ;; with the comment text as unique children
     ( commented-element
     ( make-instance 'pretty-atom :children ( list text ) :attributes
      ( list ( cons :class +CLASS-ATOM+ ) ( cons :type +CLASS-ATOM+ ) ) :parent
      parent ) ) )
    ;; if the event-element has a parent, replace the event-element with the generated comment
     ( if parent ( setf ( nth pos ( xml-children parent ) ) commented-element ) )
   commented-element ) )

( defun surround-element ( event-element parent siblings )
  "Sorround the element with closed parentheses"
  ( let ( ( newlist
         ( make-instance 'pretty-list :attributes
          ( list ( cons :class +CLASS-LIST+ ) ( cons :type +CLASS-LIST+ )
                ( cons :listtype "" ) )
          :parent parent ) ) )
    ( xml-append-child newlist ( xml-copy event-element ) )
    ( when parent
      ( setf ( xml-children parent ) ( substitute newlist event-element siblings ) ) )
    newlist ) )

( defun insert-element
       ( event-element operation parent pos siblings editing-nodes )
  "inserts a new element, depending on the operation: 
- :inside event-element (if a list)
- :before event-element
- :after event-element"
  ( let ( ( newatom
         ( make-instance 'pretty-atom :children ( list "#||#" ) :attributes
          ( list ( cons :class +CLASS-ATOM+ ) ( cons :type +CLASS-ATOM+ ) ) :parent
          parent ) )
        ( insert-position ) )
    ( when ( and ( string-equal operation :inside ) ( pretty-list-p event-element ) )
      ( setf ( xml-children event-element )
              ( cons newatom ( xml-children event-element ) ) )
      ( setf ( xml-parent newatom ) event-element )
      ( set-as-focus-element newatom ) )
    ( when ( and ( string-equal operation :inside ) ( pretty-atom-p event-element ) )
      ( setf ( xml-children event-element ) ( list "#||#" ) )
      ( set-as-focus-element event-element ) )
    ( when ( and parent ( find operation '( :before :after ) :test #'string-equal ) )
      ( if ( string-equal :before operation ) ( setf insert-position pos ) 
          ;; else :after
           ( setf insert-position ( + 1 pos ) ) )
      ( setf ( xml-children parent )
              ( append ( subseq siblings 0 insert-position ) ( list newatom )
                      ( subseq siblings insert-position ) ) )
      ( set-as-focus-element newatom ) )
    ( when
        ( and ( not parent )
             ( find operation '( :after :before ) :test #'string-equal ) )
      ( set-as-focus-element newatom )
      ( setf insert-position ( position event-element editing-nodes ) )
      ( if ( string-equal operation :after ) ( incf insert-position ) )
      ( setf editing-nodes
              ( append ( subseq editing-nodes 0 insert-position ) ( list newatom )
                      ( subseq editing-nodes insert-position ) ) ) )
    editing-nodes ) )

( defun editelement ( request-xml )
  "This process the xml request for purposes of editing the elements."
  
  
  
  #|
The xml has the example form:
<editelement>
  <text roxid='roxid5455' type='atom' operation='Update'>my-fun</text>
</editelement>
|#
  ;; identify the file which is being edited
  ;; and make a backup of the actual state
   ( copy-state request-xml )
  ( let*
   (  ;; keeps track of the element to be signaled as the
      ;; changed element
       ( track nil )   ;; the svg element which co-activated the editing event
                    ;; is the first (and unique!) children of the xml request
                     ( event-svg ( car ( xml-children request-xml ) ) ) 
    ;; the type of svg element {list, atom, list descriptor}
     ( eltype ( xml-attribute-get event-svg :type ) ) 
    ;; the event operation {:execute, :delete, ...}
     ( operation ( xml-attribute-get event-svg :operation ) ) 
    ;; the unique id of the element
     ( roxid
     ( if ( string-equal eltype :listdescriptor )
         ( xml-attribute-get event-svg :tgt )
         ( xml-attribute-get event-svg :roxid ) ) )
       ;; The opened file which contains the element.
       ;; Several files may be opened. An alternative is to identify
       ;; the file as an attribute in the svg element
        ( pfile ( roxid-pfile roxid ) )
    ( dummy
     ( if ( null pfile )
         ( return-from editelement
          ( jquery-1 "#bottomstatus" "html" operation
           ": Impossible to process command. Was file closed? Multiple tabs for same file?" ) ) ) )
      ;; list of xml nodes, representing the code in the file
      ;; each element of the xml list represents a code list or an atom
       ( editing-nodes ( copy-list ( pfile-xml pfile ) ) ) 
    ;; find the xml node corresponding to the event element
     ( event-element ( find-by-attribute editing-nodes :roxid roxid ) )
    ( event-element-copy ( xml-copy event-element ) ) 
    ;; the parent of the element (a list, if any)
     ( parent ( xml-parent event-element ) )  
    ;; if the parent (list) exists,
    ;;the siblings are other elements inside the parent (list)
     ( siblings ( if parent ( xml-children parent ) ) )  
    ;; the toplevel element (list) for which
    ;; the event element is one of the descendant
     ( ancestor ( find-ancestor event-element ) ) 
    ;; the position of the event element among siblings
     ( pos ( if parent ( position event-element siblings ) 0 ) ) 
    ;; the xml to be returned as the answer to the editing event
     ( ret ( jquery-1 "#bottomstatus" "html" operation ) ) )
   ( declare ( ignore dummy ) ) 
   ;; if the operation is not allowed return
    ( unless ( find operation +allowed-edit-operation+ :test #'string-equal )
     ( setf ( xml-children ret )
             ( list ( format nil "~A: disabled for this session!" operation ) ) )
     ( return-from editelement ret ) )
      
   ;; remove the classes that are used in the interface
   ;; to colour newly edited elements
   ;(remove-editing-class ancestor )
   ;; if :cancel, set the focus in the same element
    ( if ( string-equal operation :cancel ) ( set-as-focus-element event-element ) ) 
   ;; if collapsing or expanding, set next focus on the top level element
    ( if
    ( find operation '( :collapse :collapseall :expand :expandall ) :test
          #'string-equal )
    ( set-as-focus-element ancestor ) )
   
   ;; transposition of the layout {vertical, horizontal}
    ( when ( string-equal operation :transpose )
     ( transpose-layout event-element )
     ( set-as-focus-element event-element ) )
   
   ;; transforms the pretty element into a comment
    ( when ( string-equal operation :comment )
     ( let ( ( comment-atom ( comment-element event-element parent pos ) ) )
       ( set-as-focus-element comment-atom )
       ( if ( eq ancestor event-element ) 
           ;; sets the comment-atom as the top level replacement for event-element
            ( setf editing-nodes
                   ( substitute comment-atom event-element editing-nodes ) ) ) ) )
   
   ;; code execution with eval
    ( when ( string-equal operation :execute )
     ( setf ( xml-children ret )
             ( list
              ( format nil "~A> ~S" ( string-upcase ( package-name *package* ) )
                      ( eval
                       ( read-from-string
                        ( xml-to-code-string event-element ) ) ) ) ) )
     ( set-as-focus-element event-element ) )
   
   ;; update the element text, or parse it to new elements
    ( if ( string-equal :update operation )
       ( setf editing-nodes
               ( update-element event-element eltype parent siblings pos
                event-svg editing-nodes ) ) )
   
   ;; remove comment marks or replace the list with its children
    ( if ( string-equal :free operation )
       ( setf editing-nodes
               ( free-element event-element eltype parent siblings pos event-svg
                editing-nodes ) ) )
   
   ;; surround the element with a closed parentheses
    ( when ( string-equal :surround operation )
     ( let ( ( newlist ( surround-element event-element parent siblings ) ) )
       ( set-as-focus-element newlist )
       ( if ( eq ancestor event-element )
           ( setf editing-nodes
                   ( substitute newlist event-element editing-nodes ) ) ) ) )
   
   ;; when cut or copy place the element in memory
    ( when ( find operation '( :copy :cut ) :test #'string-equal )
     ( setf *EDIT-MEMORY* ( xml-copy event-element ) )
     ( if ( string-equal :copy operation ) ( set-as-focus-element event-element ) ) )
   
   ;; delete or cut removes the element
    ( when ( find operation '( :delete :cut ) :test #'string-equal )
     ( setf editing-nodes
             ( delete-element event-element parent siblings pos editing-nodes ) ) )
   
   ;; the paste operation
    ( when ( and ( string-equal :paste operation ) *EDIT-MEMORY* )
     ( let ( ( memory ( xml-copy *EDIT-MEMORY* ) ) )
       ( set-id-all-family memory )
       ( setf ( xml-parent memory ) parent )
       ( set-as-focus-element memory )
       ( if parent ( setf ( nth pos ( xml-children parent ) ) memory ) 
           ;;else
            ( progn
            ( setf ancestor memory )
            ( setf editing-nodes
                    ( substitute memory event-element editing-nodes ) ) ) ) ) )
   
   ;; inserting inside, before and after
    ( if ( find operation '( :after :before :inside ) :test #'string-equal )
       ( setf editing-nodes
               ( insert-element event-element operation parent pos siblings
                editing-nodes ) ) )
   
   ;; undo
    ( when ( string-equal operation :undo )
     ( setf track ( getf ( pfile-current-focus pfile ) :undo ) )
     ( setf editing-nodes ( pfile-undo-redo pfile operation nil ) )
     ( unless track
       ( setf track
               ( or ( and ( find ancestor editing-nodes ) event-element )
                   ( car editing-nodes ) ) ) ) )
   
   ;; redo
    ( when ( string-equal operation :redo )
     ( setf editing-nodes ( pfile-undo-redo pfile operation nil ) )
     ( setf track ( getf ( pfile-current-focus pfile ) :redo ) ) )
   
   ;; update nodes in browser (1)
    ( when
       ( find operation
             '( :update :transpose :comment :free :surround :cut :delete :paste
              :after :before :inside )
             :test #'string-equal )
     
     ;; ensure that the ancestor will be replaced
      ( setf editing-nodes
             ( substitute ( xml-copy ancestor ) ancestor editing-nodes ) )
     
     ;; control the focus element
      ( let ( ( focus ( get-focus-element editing-nodes ) ) )
       ( when focus ( setf track focus ) ( remove-as-focus-element focus ) ) )
     
     ;; update nodes in browser
      ( setf ret ( update-editing-nodes-in-browser ret pfile editing-nodes ) ) )
   
   ;; for undo e redo
    ( when ( find operation '( :undo :redo ) :test #'string-equal )
     
     ;; update nodes in browser
      ( setf ret ( update-editing-nodes-in-browser ret pfile editing-nodes ) )
     ( setf ret
             ( wrap
              ( list ret
                    ( js-predefined "goToElement"
                     ( xml-attribute-get track :roxid ) ) ) ) ) )
   
   ;; mark focus
    ( when
       ( find operation
             '( :copy :cancel :execute :collapse :collapseall :expand :expandall )
             :test #'string-equal )
     
     ;; control the focus element
      ( let ( ( focus ( get-focus-element editing-nodes ) ) )
       ( when focus ( setf track focus ) ( remove-as-focus-element focus ) ) ) )
   ( when ( find operation '( :up :down :left :right ) :test #'string-equal )
     ( setf track ( navigate event-element parent editing-nodes operation ) ) )
   
   ;; update pfile-xml with the new nodes
    ( if ( find operation '( :undo :redo ) :test #'string-equal )   
       ;; undo redo dont need pointer update
       ;; only the update of current file
       ;; because the pointer was already changed
        ( progn ( setf ( pfile-xml pfile ) editing-nodes ) ) 
       ;; other than undo redo
        ( pfile-update-current-nodes pfile editing-nodes
        ( list :undo event-element-copy :redo track ) operation ) )
   ( setf ret ( wrap ( list ret ( make-triggered-event track "lastclick" ) ) ) )
   ( minimize-and-expand ret ancestor editing-nodes operation ) ) )
