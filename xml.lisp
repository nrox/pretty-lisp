

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


( in-package :pretty-lisp )


( defvar *ID-COUNTER*
  0
  "This will be incremented to assign single id values to elements." )


( defgeneric xml-to-string ( xml-structure ) )


( defun string-to-xml ( xml-string )
  ( parse-xml-string xml-string :output-type :xml-struct ) )


( defun xml-children-list ( xml ) ( xml-element-children xml ) )


( defgeneric xml-value ( xml ) )


( defmethod xml-value ( xml ) ( car ( xml-children-list xml ) ) )


( defclass xml-node ( )
          ( ( tag :accessor xml-tag :initarg :tag :initform nil )
           ( parent :accessor xml-parent :initarg :parent :initform nil )
           ( attributes :accessor xml-attributes :initarg :attributes :initform
            nil )
           ( properties :accessor xml-properties :initarg :properties :initform
            nil )
           ( children :accessor xml-children :initarg :children :initform nil ) ) )


( defun format-and-ucase ( att ) ( string-upcase ( format nil "~A" att ) ) )


( ;; FIXME
   progn ( defgeneric xml-node-p ( object ) )
 ( defmethod xml-node-p ( ( object xml-node ) ) t )
 ( defmethod xml-node-p ( object ) nil ) )


( defun xml-property-get ( node ppt )
  ( rest ( assoc ppt ( xml-properties node ) :test #'string-equal ) ) )


( defun xml-property-set ( node ppt value )
  ( let*
   ( ( ppts-list ( xml-properties node ) )
    ( pair ( assoc ppt ppts-list :test #'string-equal ) ) )
   ( cond  ;; already exists, and the new value is not nil, replace it
           ( ( and pair value ) ( setf ( rest pair ) value ) ) 
         ;; exists and value is nil, remove it
          ( pair
          ( setf ( xml-properties node )
                  ( remove ppt ppts-list :test #'string-equal :key #'car ) ) )
         
         ;; does not exist and value is not null, set it
          ( value
          ( setf ( xml-properties node ) ( cons ( cons ppt value ) ppts-list ) ) ) ) ) )


( defun xml-attribute-set ( node att value )
  ( let*
   ( ( atts-list ( xml-attributes node ) )
    ( pair ( assoc att atts-list :test #'string-equal ) ) )
   ( cond 
         ;; the attribute already exists, and the new value is not nil, replace it
          ( ( and pair value )
          ( setf ( rest pair )
                  ( if ( stringp value ) value ( format nil "~A" value ) ) ) )
         
         ;; exists and value is nil, remove it
          ( pair
          ( setf ( xml-attributes node )
                  ( remove att ( xml-attributes node ) :test #'string-equal :key
                          #'car ) ) )
         
         ;; does not exist and value is not null, set it
          ( value
          ( setf ( xml-attributes node )
                  ( cons
                   ( cons att
                         ( if ( stringp value ) value ( format nil "~A" value ) ) )
                   atts-list ) ) ) ) ) )


( defun xml-attribute-get ( node att )
  ( rest ( assoc att ( xml-attributes node ) :test #'string-equal ) ) )


( defun xml-attribute-concatenate ( node att value &optional ( separator " " ) )
  ( let ( ( cur-value ( xml-attribute-get node att ) ) )
    ( unless ( search value cur-value )
      ( xml-attribute-set node att
       ( concatenate 'string ( format nil "~A" cur-value ) separator
                    ( format nil "~A" value ) ) ) ) ) )


( defun xml-attribute-remove ( node att value )
  ( let ( ( cur-value ( xml-attribute-get node att ) ) )
    ( setf cur-value ( string-trim " " ( string-replace cur-value value "" ) ) )
    ( xml-attribute-set node att cur-value ) ) )


( defun xml-attribute-append ( node att value )
  ( let ( ( cur-value ( xml-attribute-get node att ) ) )
    ( if ( listp cur-value )
        ( xml-attribute-set node att ( append cur-value ( list value ) ) )
        ( xml-attribute-set node att ( list cur-value value ) ) ) ) )


( defgeneric xml-append-child ( parent child ) )


( defmethod xml-append-child ( ( parent xml-node ) ( child string ) )
           ( setf ( xml-children parent )
                   ( append ( xml-children parent ) ( list child ) ) ) )


( defmethod xml-append-child ( ( parent xml-node ) ( child xml-node ) )
           ( when ( listp ( xml-children parent ) )
             ( setf ( xml-children parent )
                     ( append ( xml-children parent ) ( list child ) ) )
             ( setf ( xml-parent child ) parent ) ) )


( defun read-xml ( in )
  ( let ( ( parents-stack ) ( root ) )
    ( start-parse-xml in
     ( make-instance 'xml-parser-state :new-element-hook
      #'( lambda ( name attributes seed )
        ( declare ( ignore seed ) )
        
        #| create a new xml element and puch it to the parents stack |#
         ( push
         ( make-instance 'xml-node :tag name :attributes
          ( mapcar #'( lambda ( p ) ( cons ( car p ) ( cdr p ) ) ) ( reverse attributes ) ) )
         parents-stack )
        
        #| set parent and children |#
         ( when ( second parents-stack )
          ( xml-append-child ( second parents-stack ) ( first parents-stack ) ) ) )
      :finish-element-hook
      #'( lambda ( name attributes parent-seed seed )
        ( declare ( ignore name attributes parent-seed seed ) )
        ( setf root ( pop parents-stack ) ) )
      :text-hook
      #'( lambda ( string seed )
        ( declare ( ignore seed ) )
        ( xml-append-child ( first parents-stack ) string ) ) ) )
    root ) )


( defun xml-parse-from-string ( str )
  ( with-input-from-string ( in str ) ( read-xml in ) ) )


( defun auto-id ( prefix ) ( format nil "~A~A" prefix ( incf *ID-COUNTER* ) ) )


( defgeneric xml-id-set ( str &optional prefix overwrite ) )


( defmethod xml-id-set
           ( ( node xml-node ) &optional ( prefix :roxid ) ( overwrite nil ) )
           ( when ( or overwrite ( not ( xml-attribute-get node prefix ) ) )
             ( xml-attribute-set node prefix ( auto-id prefix ) ) ) )


( defmethod xml-id-set ( str &optional prefix overwrite )
           ( declare ( ignore str prefix overwrite ) ) )


( defgeneric find-by-attribute ( xml attr val ) )


( defmethod find-by-attribute ( xml attr val )
           ( if ( consp xml )
               ( reduce #'( lambda ( x y ) ( or x ( find-by-attribute y attr val ) ) ) xml
                       :initial-value nil )
               nil ) )


( defmethod find-by-attribute ( ( xml xml-node ) attr val )
           "Finds a descendant of xml by a value of its attribute."
           ( if ( string-equal ( xml-attribute-get xml attr ) val ) xml
               ( reduce #'( lambda ( x y ) ( or x ( find-by-attribute y attr val ) ) )
                       ( xml-children xml ) :initial-value nil ) ) )


( defgeneric find-ancestor ( xml ) )


( defmethod find-ancestor ( xml ) )


( defmethod find-ancestor ( ( xml xml-node ) )
           "Finds the top level parent of parent,
or the same xml, if it has no parent."
           ( if ( xml-parent xml ) ( find-ancestor ( xml-parent xml ) ) xml ) )


( defgeneric xml-copy ( obj ) )


( defmethod xml-copy ( str )
           ( if ( consp str ) ( mapcar #'xml-copy str ) ( format nil "~A" str ) ) )


( defmethod xml-copy ( ( xml xml-node ) )
           ( let ( copy ( atts ( xml-attributes xml ) ) ( ppts ( xml-properties xml ) ) )
             ( setf copy ( make-instance ( class-of xml ) :tag ( xml-tag xml ) ) )
             ( dolist ( attpair atts )
               ( xml-attribute-set copy ( car attpair ) ( rest attpair ) ) )
             ( dolist ( pptpair ppts )
               ( xml-property-set copy ( car pptpair ) ( rest pptpair ) ) )
             ( dolist ( child ( mapcar #'xml-copy ( xml-children xml ) ) copy )
               ( xml-append-child copy child ) ) ) )


( defun s-xml-to-rox-xml ( xml-struct )
  "Converts s-xml struct to xml-node."
  ( if ( s-xml:xml-element-p xml-struct )
      ( let ( ( name ( s-xml:xml-element-name xml-struct ) )
            ( children ( s-xml:xml-element-children xml-struct ) )
            ( attributes ( s-xml:xml-element-attributes xml-struct ) )
            ( node ( make-instance 'xml-node ) ) )
        ( setf ( xml-tag node ) ( format-and-ucase name ) )
        ( dolist ( child ( mapcar #'s-xml-to-rox-xml children ) nil )
          ( xml-append-child node child ) )
        ( dolist ( att attributes nil )
          ( xml-attribute-set node ( car att ) ( rest att ) ) )
        node )
      ( format nil "~A" xml-struct ) ) )


( defun rox-xml-to-s-xml ( node )
  "Converts xml-node to s-xml element."
  ( if ( xml-node-p node )
      ( let ( ( tag ( xml-tag node ) )
            ( children ( xml-children node ) )
            ( attributes ( xml-attributes node ) )
            ( xml-struct ( s-xml:make-xml-element ) ) )
        ( setf ( s-xml:xml-element-name xml-struct ) tag )
        ( setf ( s-xml:xml-element-attributes xml-struct )
                ( mapcar
                 #'( lambda ( att-pair )
                   ( cons ( first att-pair ) ( format nil "~A" ( rest att-pair ) ) ) )
                 attributes ) )
        ( setf ( s-xml:xml-element-children xml-struct )
                ( mapcar #'rox-xml-to-s-xml children ) )
        xml-struct )
      ( format nil "~A" node ) ) )


( defun print-rox-xml ( node )
  ( s-xml:print-xml-string ( rox-xml-to-s-xml node ) :pretty t :input-type
   :xml-struct ) )


( defmethod xml-to-string ( xml-structure )
           ( print-xml-string xml-structure :pretty t :input-type :xml-struct ) )


( defmethod xml-to-string ( ( node xml-node ) )  
           ;; the convertion to string of xml-node use an
           ;; intermediate convertion to s-xml
            ( xml-to-string ( rox-xml-to-s-xml node ) ) )


( defmethod xml-value ( ( xml xml-node ) )
           ( let ( ( val ( car ( xml-children xml ) ) ) )
             ( when ( null val ) ( setf val "" ) )
             ( format nil "~A" val ) ) )


( defun xml-value-set ( xml val )
  ( setf ( xml-children xml ) ( list ( format nil "~A" val ) ) ) )
