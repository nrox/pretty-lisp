

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


( in-package :pretty-lisp )


;;; ---- file and folder operations ------ 


( defun read-file ( path )
  "Read file and returns a string with its contents"
  ( with-open-file ( s path )
   ( let* ( ( len ( file-length s ) ) ( data ( make-string len ) ) )
    ( read-sequence data s ) ( values data ) ) ) )


( defun write-to-file ( filepath content )
  "Writes content to filepath, overwriting if exists."
  ( with-open-file
   ( stream filepath :direction :output :if-exists :supersede :if-does-not-exist
    :create )
   ( format stream "~A" content ) )
  filepath )


( defun list-folder-pathnames ( folderpath )
  "Returns a list of folder contents"
  ( directory ( make-pathname :name :wild :type :wild :defaults folderpath ) ) )


( defun pathname-filename ( pathname )
  ( concatenate 'string ( pathname-name pathname ) "." ( pathname-type pathname ) ) )


#| use iolib here, to portability |# 


( defun list-folder-filenames ( folderpath )
  ( mapcar #'pathname-filename ( list-folder-pathnames folderpath ) ) )


( defun folder-hierarchy ( folder-path &optional extensions )
  ( let*
   ( ( folder-path-name ( pathname folder-path ) )
    ( directory-list ( reverse ( pathname-directory folder-path-name ) ) )
    ( hierarchy ) ( auxiliar ) )
   ( setf hierarchy
           ( maplist #'( lambda ( dir ) ( make-pathname :directory ( reverse dir ) ) )
                    directory-list ) )
   ( setf hierarchy ( reverse hierarchy ) )
   ( setf auxiliar ( list-folder-pathnames folder-path ) )
   ( setf auxiliar
           ( remove-if #'null auxiliar :key
            #'( lambda ( f )
              ( or ( null extensions )
                  ( find ( pathname-type f ) extensions :test #'string-equal )
                  ( string= ( namestring f ) ( directory-namestring f ) ) ) ) ) )
   ( setf hierarchy ( append hierarchy auxiliar ) )
   ( mapcar #'namestring hierarchy ) ) )


( defun is-directory ( folder-path )
  ( let ( ( pname ( pathname folder-path ) ) )
    ( string= ( namestring pname ) ( directory-namestring pname ) ) ) )


( defun string-replace ( str1 sub1 sub2 )
  "Replace in str1 each occurrence of sub1 with sub2"
  
  ;;; http://www.tek-tips.com/viewthread.cfm?qid=1184743&page=2
   ( let ( ( str1 ( string str1 ) )
        ( str2 "" )
        ( sub1 ( string sub1 ) )
        ( sub2 ( string sub2 ) )
        ( index1 0 ) )
    ( loop ( if
           ( string-equal str1 sub1 :start1 index1 :end1
            ( min ( length str1 ) ( + index1 ( length sub1 ) ) ) )
           ( progn
            ( setq str2 ( concatenate 'string str2 sub2 ) )
            ( incf index1 ( length sub1 ) ) )
           ( progn
            ( setq str2
                    ( concatenate 'string str2
                                 ( subseq str1 index1 ( 1+ index1 ) ) ) )
            ( incf index1 ) ) ) ( unless ( < index1 ( length str1 ) ) ( return str2 ) ) ) ) )


( defun count-substring ( substr str )
  "Count the number of occurrences of substr in str"
  ( do ( ( sum -1 ( 1+ sum ) )
       ( pos -1 ( search substr str :start2 ( 1+ pos ) ) ) )
      ( ( null pos ) sum ) ) )


( defun string-split ( string &optional ( div-char #\Space ) )
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  
  ;; http://cl-cookbook.sourceforge.net/strings.html
   ( loop for i = 0 then ( 1+ j ) as j = ( position div-char string :start i )
        collect ( subseq string i j )
        while j ) )


;;; ----------- math --------------- 


( defun levenshtein ( s01 s02 &key ( ignore-case nil ) )
  "The Levenshtein distance between strings."
  
  ;; http://www.cliki.net/Levenshtein
   ( let*
   ( ( s1 ( if ignore-case ( string-downcase s01 ) s01 ) )
    ( s2 ( if ignore-case ( string-downcase s02 ) s02 ) ) ( width ( 1+ ( length s1 ) ) )
    ( height ( 1+ ( length s2 ) ) ) ( d ( make-array ( list height width ) ) ) )
   ( dotimes ( x width ) ( setf ( aref d 0 x ) x ) )
   ( dotimes ( y height ) ( setf ( aref d y 0 ) y ) )
   ( dotimes ( x ( length s1 ) )
     ( dotimes ( y ( length s2 ) )
       ( setf ( aref d ( 1+ y ) ( 1+ x ) )
               ( min ( 1+ ( aref d y ( 1+ x ) ) ) ( 1+ ( aref d ( 1+ y ) x ) )
                    ( + ( aref d y x )
                     ( if ( char= ( aref s1 x ) ( aref s2 y ) ) 0 1 ) ) ) ) ) )
   ( aref d ( 1- height ) ( 1- width ) ) ) )


( defun levenshtein-ratio ( to-compare reference )
  "The Levenshtein distance, normalized with the reference/target length"
  ( let ( ( len ( length reference ) ) )
    ( if ( = len 0 ) ( length to-compare )
        ( / ( levenshtein to-compare reference ) len ) ) ) )


( defun farthest ( desc )
  "return the farthest point (x+w y+h) from a list of (x y w h)"
  ( let ( ( x-far 0 ) ( y-far 0 ) )
    ( dolist ( item desc ( list x-far y-far ) )
      ( setf x-far ( max x-far ( + ( first item ) ( third item ) ) ) )
      ( setf y-far ( max y-far ( + ( second item ) ( fourth item ) ) ) ) ) ) )


;;;- ---------- other -------------- 


( defun truncate-list ( lst n ) ( if ( <= ( length lst ) n ) lst ( subseq lst 0 n ) ) )


( defun unfold ( tree )
  "Takes the tree and transform to a single list."
  ( if ( consp tree ) ( apply #'append ( mapcar #'unfold tree ) ) ( list tree ) ) )
