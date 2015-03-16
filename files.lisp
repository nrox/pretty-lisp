

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

( defparameter *use-pprint*
  t
  "If the customized pprint should be used when saving files." )

( defparameter +MAX-UNDO+ 50 "Maximum of undo operations." )

( defvar *DEFAULT-FOLDER*
  ( concatenate 'string +source-folder+ "temp/" )
  "The initial browsing path." )

( defparameter +AUTO-SAVE-COUNTER+
  0
  "Counts the number of editing operations since last backup copy." )

( defparameter +AUTO-SAVE-LIMIT+
  15
  "After this number of editing operations there is an automatic backup copy." )

( defparameter +TAG-CONT+ "div" )

( defvar +BK-DIR+
  ( concatenate 'string +source-folder+ "backups/" )
  "Editing files are periodically saved to this folder." )

( defun default-folder ( &optional path )
  ( let ( ( name ( format nil "~A~A" +BK-DIR+ "deffolder.txt" ) ) )
    ( cond ( path ( write-to-file name path ) ) ( ( probe-file name ) ( read-file name ) )
          ( t ( write-to-file name *DEFAULT-FOLDER* ) *DEFAULT-FOLDER* ) ) ) )

( setf *DEFAULT-FOLDER* ( default-folder ) )

( defmethod xml-to-code-string ( ( xml pretty-list ) )
  ( let ( ( children-code ( mapcar #'xml-to-code-string ( xml-children xml ) ) ) )
    ( concatenate 'string ( xml-attribute-get xml +LIST-TYPE+ ) "("
                 ( apply #'concatenate ( push 'string children-code ) ) ")" ) ) )

( defmethod xml-to-code-string ( ( xml pretty-atom ) )
  ( let*
   ( ( val ( xml-value xml ) )
    ( newline
     ( or
      ( and ( > ( length val ) 0 ) ( eq #\; ( char val 0 ) )
           ( coerce '( #\Newline ) 'string ) )
      " " ) ) )
   ( concatenate 'string val newline ) ) )

( ;; DELETEME
   defun make-top-menu ( )
 ( string-to-xml "<appendchildren tgt='topmenu'><b>ola</b></appendchildren>" ) )

( defclass pfile-manager ( )
          ( ( name :accessor pfile-name :initarg :name )
           ( path :accessor pfile-path :initarg :path )
           ( id :accessor pfile-id :initform ( auto-id "file" ) )
           ( xml :accessor pfile-xml :initarg :xml )
           ( history :accessor pfile-history :initarg :history )
           ( focus-history :accessor pfile-focus-history :initform ( list nil ) )
           ( focus :accessor pfile-focus :initform nil )
           ( pointer :accessor pfile-pointer :initform 0 ) ) )

( ;; FIXME
   progn
 ( defgeneric pfile-manager-p
     ( obj ) )
 ( defmethod pfile-manager-p ( ( obj pfile-manager ) ) t )
 ( defmethod pfile-manager-p ( obj ) ) )

( defun roxid-pfile ( roxid )
  
  ;; FIXME use loop instead
   ( block nil
    ( dolist ( file *EDITING-FILES* nil )
      ( when ( find-by-attribute ( pfile-xml file ) :roxid roxid ) ( return file ) ) ) ) )

( defun pfile-current-focus ( pfile )
  ( nth ( pfile-pointer pfile ) ( pfile-focus-history pfile ) ) )

( defun pfile-undo-redo ( pfile direction &optional update )
  "Replaces the current list of nodes with a
 previous (direction :undo) or the next (direction :redo) in history."
  ( let ( ( pointer ( pfile-pointer pfile ) ) ( history ( pfile-history pfile ) ) )
    
    ;; undo or redo changes the pointer to the history item
     ( if ( string-equal direction :undo )
        ( incf pointer )
        ( decf pointer ) )
    
    ;; check pointer upper boundary
     ( when ( > pointer ( - ( length history ) 1 ) )
      ( setf pointer ( - ( length history ) 1 ) ) )
    
    ;; check pointer lower boundary
     ( when ( < pointer 0 ) ( setf pointer 0 ) )
    
    ;; update pointer after checking boundaries
     ( setf ( pfile-pointer pfile ) pointer )
    
    ;; if update the current nodes, update it, else just return the new nodes
     ( if update
        ( setf ( pfile-xml pfile ) ( copy-list ( nth pointer history ) ) )
        ( copy-list ( nth pointer history ) ) ) ) )

( defgeneric pfile-pprint
    ( pfile &optional to-other-path ) )

( defun pfile-save ( pfile )
  "Save the pretty nodes to a file, after converting them to text code."
  ( if *use-pprint*
      ( pfile-pprint pfile )
      ( let ( ( nodes ( pfile-xml pfile ) ) ( path ( pfile-path pfile ) ) ( str nil ) )
        
        ;; FIXME use reduce
         ( dolist ( node nodes nil )
         
         
          ;; for each node tranform if to text code
          ;; and concatenate it to the string to be saved
           ( setf str
                  ( concatenate 'string str
                               ( coerce '( #\Newline #\Newline ) 'string )
                               ( xml-to-code-string node ) ) ) )
        ( write-to-file path str ) ) ) )

( defun pfile-backup ( pfile )
  "Backup of a text code file. Used before saving, as precaution."
  ( if *backup-files*
      ( let*
       ( ( path ( pfile-path pfile ) ) ( name ( pfile-name pfile ) )
        ( bk-path ( format nil "~Abackup_~A" +BK-DIR+ name ) ) )
       ( when ( probe-file path ) ( write-to-file bk-path ( read-file path ) ) ) ) ) )

( defun pfile-auto-save ( pfile )
  "After some number of editing operations this function is calles to make
an automatic backup copy of the pretty code, saved in text format."
  ( if *autosave-files*
      ( when ( > ( incf +AUTO-SAVE-COUNTER+ ) +AUTO-SAVE-LIMIT+ )
        ( setf +AUTO-SAVE-COUNTER+ 0 )
        ( let*
         ( ( nodes ( pfile-xml pfile ) ) ( name ( pfile-name pfile ) ) ( str nil )
          ( bk-path ( format nil "~Aautosave_~A" +BK-DIR+ name ) ) )
         ( dolist ( node nodes nil )
           ( setf str
                   ( concatenate 'string str
                                ( coerce '( #\Newline #\Newline ) 'string )
                                ( xml-to-code-string node ) ) ) )
         ( write-to-file bk-path str ) ) ) ) )

( defun copy-state ( xml )
  "Make a copy of the list of actual nodes being edited
so that the changes will not afect history, making undo redo coherent.
This function is called before any editing operations."
  ( let*
   ( ( svg ( car ( xml-children xml ) ) )
    ( operation ( xml-attribute-get svg :operation ) )
    ( roxid ( xml-attribute-get svg :roxid ) ) ( pfile ( roxid-pfile roxid ) )
    ( dummy
     ( if ( null pfile )
         ( return-from copy-state nil ) ) )
    ( current ( copy-list ( pfile-xml pfile ) ) )
    ( element-that-will-change
     ( find-ancestor ( find-by-attribute current :roxid roxid ) ) )
    ( copy nil ) )
   ( declare ( ignore dummy ) )
   ( when 
     ;; only these operations are relevant
      ( find operation
           '( :update :paste :delete :cut :before :inside :after :surround :free
            :transpose :comment )
           :test #'string-equal )
     ( setf copy
             ( mapcar
              #'( lambda ( node )
                
                
                ;; only the top level node that will be changed
                ;; needs to be copied
                 ( if ( eq element-that-will-change node )
                    ( xml-copy node )
                    node ) )
              current ) )
     
     
     ;; what will be edited later is the copy
     ;; and the node that will be edited is kept intact in history
      ( setf ( pfile-xml pfile ) copy ) ) ) )

( defun pfile-update-current-nodes
       ( pfile current focus &optional ( operation :update ) )
  "Save the current editing nodes to history"
  ( let ( ( history ( pfile-history pfile ) )
        ( focus-history ( pfile-focus-history pfile ) ) )
    
    ;; this is necessary only for some editing operations
     ( when
        ( find operation
              '( :update :paste :delete :cut :before :inside :after :surround
               :free :transpose :comment )
              :test #'string-equal )
      
      
      ;; if this function is called after an undo redo operation the
      ;; most recent states are lost
       ( setf history ( subseq history ( pfile-pointer pfile ) ) )
      ( setf focus-history ( subseq focus-history ( pfile-pointer pfile ) ) )
      ( setf ( pfile-pointer pfile ) 0 )
      
      ;; save to history
       ( push ( copy-list current ) history )
      ( push focus focus-history )
      
      ;; limit history size
       ( when ( > ( length history ) +MAX-UNDO+ )
        ( setf history ( subseq history 0 +MAX-UNDO+ ) )
        ( setf focus-history ( subseq focus-history 0 +MAX-UNDO+ ) ) )
      
      ;; replace actual history
       ( setf ( pfile-history pfile ) history )
      ( setf ( pfile-focus-history pfile ) focus-history )
      
      ;; set current editing nodes
       ( setf ( pfile-xml pfile ) ( copy-list current ) )
      ( setf ( pfile-focus pfile ) focus )
      
      ;; process auto-saving (if needed)
       ( pfile-auto-save pfile ) ) ) )

( defgeneric process-dimensioning
    ( xml ) )

( defgeneric xml-node-to-pretty-node
    ( xml &optional parent ) )

( defgeneric wrap-with-root-svg
    ( xml ) )

( defgeneric set-id-all-family
    ( str &optional overwrite ) )

( defun update-editing-nodes-in-browser ( xml-ret pfile new-editing-nodes )
  "makes a list of instructions to update the editing file in browser"
  ( let ( ( old-editing-nodes ( copy-list ( pfile-xml pfile ) ) )
        ( actions
         ( wrap
          ( if xml-ret
              ( list xml-ret ) ) ) ) )
    
    ;; remove the nodes that were deleted or updated
     ( dolist ( root-node ( copy-list old-editing-nodes ) )
      ( unless ( member root-node new-editing-nodes )
        ( xml-append-child actions
         ( jquery-0 ( format nil "#p~A" ( xml-attribute-get root-node :roxid ) )
          "remove" ) )
        ( setf old-editing-nodes ( remove root-node old-editing-nodes ) ) ) )
    
    ;; insert containers and svg for the new and updated elements
     ( let ( ( reference "#nodescontainer" )  ;; to insert the first node to the html div, use prepend
                                          ( jmethod "prepend" ) ( node-svg nil ) )
      ( dolist ( root-node new-editing-nodes )
       
       
        ;; to insert the rest use after
        ;; and when a node already exists, use it as reference
         ( when ( member root-node old-editing-nodes )
          ( setf reference
                  ( format nil "#p~A" ( xml-attribute-get root-node :roxid ) )
                jmethod "after" ) )
       
        ;; when its is a new root member, create containers and svg
         ( unless ( member root-node old-editing-nodes )
          
          ;; make the svg for the root-node
           ( progn
           ( set-id-all-family root-node nil )
           ( xml-attribute-set root-node :parent nil )
           ( setf node-svg ( process-dimensioning root-node ) )
           ( setf node-svg ( wrap-with-root-svg node-svg ) ) )
          
          ;; create jquery methods to insert the svg into the right place
           ( xml-append-child actions
           ( jquery-1 reference jmethod
            ( make-instance 'xml-node :tag "div" :attributes
             ( list
              ( cons :id
                    ( concatenate 'string "p"
                                 ( xml-attribute-get root-node :roxid ) ) )
              ( cons :class "pcodecontainer" ) )
             :children ( list node-svg ) ) ) )
          
          
          ;; having inserted the node, use it further as reference
          ;; to apply later the method after
           ( setf reference
                  ( concatenate 'string "#p"
                               ( xml-attribute-get root-node :roxid ) )
                jmethod "after" ) ) ) )
    actions ) )

( defun pfile-open ( filepath )
  "Takes the file and returns a pretty version of it: a list of pretty-lists and pretty-nodes."
  ( block nil
    ( let*
     ( ( edit-nodes ) ( file-text ( read-file filepath ) )
      ( rox-xml
       ( ignore-errors
        ( code-string-to-xml
         ( or
          ( and
           ( >
            ( length
             ( string-trim ( coerce '( #\Newline #\Space ) 'string ) file-text ) )
            0 )
           file-text )
          "#| file was empty |#" ) ) ) ) )
     ( when ( null rox-xml ) ( return nil ) )
     ( dolist
         ( svg
          ( if ( consp rox-xml )
              rox-xml
              ( xml-children rox-xml ) )
          nil )
       ( setf ( xml-parent svg ) nil )
       ( setf svg ( xml-node-to-pretty-node svg ) )
       ( setf svg ( process-dimensioning svg ) )
       ( push svg edit-nodes ) )
     ( reverse edit-nodes ) ) ) )

( defun cur-paths-options ( path )
  ( if *multi-user-demo*
      ( setf path *DEFAULT-FOLDER* ) )
  ( let*
   ( ( cur-files ( sort ( mapcar #'pfile-path *EDITING-FILES* ) #'string< ) )
    ( cur-pathname ( pathname path ) )
    ( cur-folder ( directory-namestring cur-pathname ) )
    ( hierarchy ( folder-hierarchy cur-folder '( "lisp" "cl" ) ) ) )
   ( setf cur-files
           ( append '( "------ Opened Files" ) cur-files '( "" )
                   '( "------ Folder Structure" ) hierarchy ) )
   ( make-instance 'xml-node :tag "select" :attributes
    ( list ( cons "id" "fileslist" ) ( cons "onclick" "markSelectedFilename()" ) )
    :children
    ( mapcar
     #'( lambda ( f )
       ( make-instance 'xml-node :tag "option" :attributes
        ( if ( string= f path )
            ( list ( cons "class" "openfile" ) ( cons "selected" "selected" ) )
            ( list ( cons "class" "openfile" ) ) )
        :children ( list f ) ) )
     cur-files ) ) ) )

( defun topsubmenu ( xml )
  ( declare ( ignore xml ) )
  ( let ( ( file-operations '( ) ) )
    ( dolist
        ( operation
         '( "browse" "open" "load" "new" "save" "saveAs" "close" "closeAll"
          "saveAll" "openAll" )
         nil )
      ( push
       ( make-instance 'xml-node :tag "button" :attributes
        ( list ( cons :operation operation ) ( cons :class "fileoperation" )
              ( cons :onclick ( format nil "fileOp('~A')" operation ) ) )
        :children ( list operation ) )
       file-operations ) )
    ( setf file-operations ( reverse file-operations ) )
    ( jquery-1 "#topsubmenu" "html" ( cur-paths-options *DEFAULT-FOLDER* )
     file-operations ) ) )

( defun prettyfile ( xmlreq )
  "This functions is used to help processing file operations (open, new)
Takes the xml request to get the filename from it"
  ( let*
   ( ( button ( car ( xml-children xmlreq ) ) )
    ( filepath ( xml-attribute-get button :newvalue ) )
    ( filename ( file-namestring ( pathname filepath ) ) )
    ( pfile ( find filepath *EDITING-FILES* :key #'pfile-path :test #'string= ) )
    ( nodes-list ) )
   
   ;; if the file is not opened, open it
    ( unless pfile
     ( setf nodes-list ( pfile-open filepath ) )
     
     ;; FIXME attention when there is no data in file
      ( when nodes-list
       ( setf pfile
               ( make-instance 'pfile-manager :name filename :path filepath :xml
                nodes-list :history ( list ( xml-copy nodes-list ) ) ) )
       ( push pfile *EDITING-FILES* ) ) )
   ( when pfile ( setf nodes-list ( pfile-xml pfile ) ) )
   ( update-editing-nodes-in-browser
    ( wrap
     ( list
      ( jquery-1 "#idlispide" "html"
       ( make-instance 'xml-node :tag "div" :attributes
        ( list ( cons "id" "nodescontainer" ) ) ) ) ) )
    pfile ( xml-copy nodes-list ) ) ) )

( defun open-all ( folder-path &key ( extensions '( "lisp" "cl" ) ) )
  ( let ( ( paths
         ( mapcar #'namestring
                 ( remove-if-not
                  #'( lambda ( f )
                    ( and ( find ( pathname-type f ) extensions :test #'equalp )
                         ( string/= ( namestring f ) ( directory-namestring f ) ) ) )
                  ( directory
                   ( make-pathname :name :wild :type :wild :defaults
                    folder-path ) ) ) ) ) )
    ( mapcar
     #'( lambda ( pretty path )
       ( wrap
        ( list pretty ( jquery-1 "#curfileid" "html" path )
              ( jquery-1 "title" "html" ( file-namestring ( pathname path ) ) ) 
              ;; this might be removed
               ( js-predefined "roxEvent"
               ( xml-attribute-get
                ( first
                 ( pfile-xml
                  ( find path *EDITING-FILES* :test #'string= :key
                        #'pfile-path ) ) )
                :roxid )
               "lastclick" )
              ;;
               ) ) )
     ( mapcar
      #'( lambda ( file )
        ( prettyfile
         ( make-instance 'xml-node :tag "fileoperation" :children
          ( list
           ( make-instance 'xml-node :tag "button" :attributes
            ( list ( cons :newvalue file ) ) ) ) ) ) )
      paths )
     paths ) ) )

( defun fileoperation ( xml )
  "This function is always called for editing operations.
The first child of the xml is the associated html  menu button"
  ( let*
   ( ( button ( car ( xml-children xml ) ) )
    ( operation ( xml-attribute-get button :operation ) )
    ( id ( xml-attribute-get button :value ) )
    ( newfile ( xml-attribute-get button :newvalue ) ) ( path nil )
    ( pfile ( find id *EDITING-FILES* :key #'pfile-path :test #'string= ) ) 
    ;; the recognized operation is displayed in the status div
     ( status ( jquery-1 "#bottomstatus" "html" operation id ) ) ( ret ) )
   
   ;; if the operation is not allowed return
    ( unless ( find operation +allowed-file-operations+ :test #'string-equal )
     ( return-from fileoperation
      ( jquery-1 "#bottomstatus" "html" operation
       ": disabled for this session!" ) ) )
   ( when
       ( and ( not pfile )
            ( find operation '( :save :close :saveAs ) :test #'string-equal ) )
     ( return-from fileoperation
      ( jquery-1 "#bottomstatus" "html" operation
       ": Impossible to process file command. Was file closed? Multiple tabs for same file?" ) ) )
   ( when ( string-equal operation :openall )
     ( setf ret ( first ( open-all newfile ) ) )
     ( if ret
         ( setf newfile ( pfile-path ( car ( last *EDITING-FILES* ) ) ) ) ) )
   ( when ( string-equal operation :open )
     ( setf path newfile )
     ( if ( and ( probe-file path ) ( not ( directory-exists-p path ) ) )
         ( setf ret
                 ( wrap
                  ( list ( prettyfile xml ) ( jquery-1 "#curfileid" "html" path ) 
                        ;; this might be removed
                         ( js-predefined "roxEvent"
                         ( xml-attribute-get
                          ( first
                           ( pfile-xml
                            ( find path *EDITING-FILES* :test #'string= :key
                                  #'pfile-path ) ) )
                          :roxid )
                         "lastclick" )
                        ;;
                         ) ) )
         ( setf status
                 ( jquery-1 "#bottomstatus" "html" operation id
                  "CANCELED: FILE DOES NOT EXIST" ) ) ) )
   ( when ( string-equal operation :load )
     ( load newfile )
     ( setf status ( jquery-1 "#bottomstatus" "html" operation newfile ) ) )
   ( when ( string-equal operation :new )
     ( setf path ( concatenate 'string "" newfile ) )
     ( if ( probe-file path )
         ( setf status
                 ( jquery-1 "#bottomstatus" "html" operation id
                  "CANCELED: FILE ALREADY EXISTS" ) )
         ( progn
          ( write-to-file path ";;;;" )
          ( setf ret
                  ( wrap
                   ( list ( prettyfile xml )  ;;
                                            ( jquery-1 "#curfileid" "html" path )
                         ( js-predefined "roxEvent"
                          ( xml-attribute-get
                           ( first
                            ( pfile-xml
                             ( find path *EDITING-FILES* :test #'string= :key
                                   #'pfile-path ) ) )
                           :roxid )
                          "lastclick" )
                         ;;
                          ) ) ) ) ) )
   ( when ( string-equal operation :close )
     ( setf +AUTO-SAVE-COUNTER+ +AUTO-SAVE-LIMIT+ )
     ( pfile-auto-save pfile )
     ( setf *EDITING-FILES* ( remove pfile *EDITING-FILES* ) )
     ( setf ret
             ( wrap
              ( list ( jquery-0 "#idlispide" "empty" )
                    ( jquery-0 "#curfileid" "empty" )
                    ( jquery-1 "title" "html" "pretty-LISP Editor" ) ) ) ) )
   ( when ( string-equal operation :closeall )
     ( setf +AUTO-SAVE-COUNTER+ +AUTO-SAVE-LIMIT+ )
     ( mapcar #'pfile-auto-save *EDITING-FILES* )
     ( setf *EDITING-FILES* nil )
     ( setf ret
             ( wrap
              ( list ( jquery-0 "#idlispide" "empty" )
                    ( jquery-0 "#curfileid" "empty" )
                    ( jquery-1 "title" "html" "pretty-LISP Editor" ) ) ) ) )
   ( when ( string-equal operation :saveas )
     ( setf status ( jquery-1 "#bottomstatus" "html" operation id "--}" newfile ) )
     ( setf ( pfile-path pfile ) newfile )
     ( setf ( pfile-name pfile ) ( file-namestring ( pathname newfile ) ) )
     ( setf ret
             ( wrap
              ( list ( jquery-1 "#curfileid" "html" newfile )
                    ( jquery-1 "#fileslist" "val" newfile )
                    ( jquery-1 "title" "html" "pretty-LISP Editor" ) ) ) ) )
   
   ;; the save family
    ( when ( find operation '( :save :saveas :saveall ) :test #'string-equal )
     ( mapcar #'( lambda ( pf ) ( pfile-backup pf ) ( pfile-save pf ) )
             ( if ( string-equal operation :saveall )
                 *EDITING-FILES*
                 ( list pfile ) ) ) )
   
   ;; show files in folder and folder structure
    ( when ( string-equal operation :browse )
     ( setf status ( jquery-1 "#bottomstatus" "html" operation newfile ) ) )
   
   ;; on all file operations update paths list
    ( progn
    ( unless *multi-user-demo*
      ( setf *DEFAULT-FOLDER* ( directory-namestring ( pathname newfile ) ) )
      
      ;; save to disk
       ( default-folder *DEFAULT-FOLDER* ) )
    ( setf ret
            ( wrap
             ( list ret
                   ( jquery-1 "#fileslist" "replaceWith"
                    ( cur-paths-options newfile ) ) ) ) ) )
   ( wrap ( list ret status ( js-predefined "updateTitle" ) ) ) ) )