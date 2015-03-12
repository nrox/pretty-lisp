

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


;;; The core parsing functions and auxiliars 


;;; TODO consider also the cases :|v673 f7 %6 | for special atoms 


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


( defun string-to-char-list ( string-code )
  "Receives a string and returns a list of its 
characters. Non graphic chars are replaced by the 
newline char."
  ( let ( ( stream-code ( make-string-input-stream string-code ) ) ( char-list nil ) )
    ( do ( ( ch ( read-char stream-code nil ) ( read-char stream-code nil ) ) )
        ( ( null ch ) ( reverse char-list ) )
     
      ;ATTENTION: non graphic char are replaced by newlines
       ( if ( graphic-char-p ch ) ( push ch char-list ) ( push #\Newline char-list ) ) ) ) )


( defun comments-in-char-list ( char-list )
  "Char-list is a sequence of characters, commonly 
taken from parsing files. This functions signal 
the begining, end and inner characters of 
comments. Elements with t and nil if they do not 
belong to a comment"
  ( let ( ( comps-list nil )
        ( last-char #\Space )
        ( dashes 0 )
        ( inside-big 0 )
        ( inside-small nil )
        ( inside-string nil ) )
    ( dolist ( cur-char char-list ( reverse comps-list ) )
      ( cond
       ( ( and ( not inside-small ) ( not inside-string ) ( eq last-char #\# )
             ( eq cur-char #\| ) )
        ( progn
         ( incf inside-big )
         
         #| incrementar pois pode haver comments dentro de comments |#
          ( setf ( car comps-list ) t )
         
         #| the # in last-char is also part |#
          ( push t comps-list ) ) )
       ( ( and ( > inside-big 0 ) ( not inside-small ) ( not inside-string )
             ( eq last-char #\| ) ( eq cur-char #\# ) )
        ( progn ( decf inside-big ) ( push t comps-list ) ) )
       ( ( and ( = inside-big 0 ) ( not inside-string ) ( eq cur-char #\; )
             ( not ( eq last-char #\\ ) ) )
        ( progn ( setf inside-small t ) ( push t comps-list ) ) )
       ( ( and ( = inside-big 0 ) ( not inside-string ) inside-small
             ( eq cur-char #\Newline ) )
        ( progn ( setf inside-small nil ) ( push nil comps-list ) ) )
       ( ( and ( = inside-big 0 ) ( not inside-small ) ( not inside-string )
             ( eq cur-char #\" ) ( = 0 ( mod dashes 2 ) ) )
        ( progn ( setf inside-string t ) ( push nil comps-list ) ) )
       ( ( and ( = inside-big 0 ) ( not inside-small ) inside-string
             ( eq cur-char #\" ) ( = 0 ( mod dashes 2 ) ) )
        ( progn ( setf inside-string nil ) ( push nil comps-list ) ) )
       ( ( or inside-small ( > inside-big 0 ) ) ( push t comps-list ) )
       ( t ( push nil comps-list ) ) )
      ( if ( eq cur-char #\\ ) ( incf dashes ) ( setf dashes 0 ) )
      ( setf last-char cur-char ) ) ) )


( defun string-components-in-char-list ( char-list )
  "Char-list is a sequence of characters, commonly 
taken from parsing files of lisp code. This 
functions signal the begining and end elements 
of string in the code, and also inner elements 
with t and nil if they do not belong to a 
string"
  ( let ( ( comps-list nil )
        ( last-char nil )
        ( inside nil )
        ( end nil )
        ( dashes 0 )
        ( comments ( comments-in-char-list char-list ) ) )
    ( loop for cur-char in char-list
          for com-char in comments
          do ( cond ( com-char ( setf end nil inside nil ) )
                   ( ( and ( eq cur-char #\" ) ( = 0 ( mod dashes 2 ) ) )
                    ( setf inside ( not ( setf end inside ) ) ) )
                   ( t ( setf end nil ) ) ) ( push ( or inside end ) comps-list ) ( if
                                                                          ( eq
                                                                           cur-char
                                                                           #\\ )
                                                                          ( incf
                                                                           dashes )
                                                                          ( setf dashes
                                                                                  0 ) ) ( setf last-char
                                                                                              cur-char ) )
    ( reverse comps-list ) ) )


( defun sequences-in-char-list ( char-list )
  "Signals beginning, type and end of sequences - in
 a list of characters char-list."
  ( let ( ( last-char #\Space )
        
        ;;the character before last
         ( las2-char #\Space )
        
        ;;the return list
         ( seq-list nil )
        ( list-type nil )
        
        #| accumulate macro chars |#
         ( stri-list ( string-components-in-char-list char-list ) )
        ;; TODO consider also comments
         )
    ( loop for cur-char in char-list
          for str-part in stri-list
          do ( cond
              ( ( or str-part  #| if it is a string part or |#
                              ( eq cur-char #\Space )  #| if we find a space or current char or last char were \ then we ignore |#
                                                     ( eq cur-char #\Newline )
                   ( and ( eq last-char #\# ) ( eq cur-char #\\ ) )
                   ( and ( eq las2-char #\# )  #| if we are refering to characters |#
                                             ( eq last-char #\\ ) ) )
               ( progn ( setf list-type nil )  #| if what we had before was a possible list type, it is not anymore |#
                                             ( push nil seq-list ) ) )
              ( ( eq cur-char #\( ) 
               #| beginning of list |#
                ( progn
                ( push ( coerce ( reverse list-type ) 'string ) seq-list )
                ( setf list-type nil ) ) )
              ( ( eq cur-char #\) ) 
               #| end of list |#
                ( progn ( push ")" seq-list ) ( setf list-type nil ) ) )
              ( ( or ( eq cur-char #\# ) ( eq cur-char #\' ) ( eq cur-char #\` )
                   ( eq cur-char #\, ) )
               ( progn ( push cur-char list-type ) ( push nil seq-list ) ) )
              ( t
               ( progn
                ( when list-type ( push cur-char list-type ) )
                ( push nil seq-list ) ) ) ) ( setf las2-char
                                               last-char ) ( setf last-char
                                                                  cur-char ) )
    ( reverse seq-list ) ) )


( defun reverse-and-format ( token )
  ( setf token ( reverse token ) )
  ( setf token ( coerce token 'string ) )
  ( let ( ( len ( length token ) ) )
    ( if
     ( and ( > len 1 )
          ( or ( and ( eq #\# ( char token 0 ) ) ( eq #\| ( char token 1 ) ) )
              ( and ( eq #\| ( char token ( - len 2 ) ) )
                   ( eq #\# ( char token ( - len 1 ) ) ) ) ) )
     ( do ( ( left ( count-substring "#|" token ) )
          ( right ( count-substring "|#" token ) )
          ( prefix nil )
          ( sufix nil ) )
         ( ( = left right )
          ( concatenate 'string prefix ( string-trim +COMM-TRIM+ token ) sufix ) )
       ( if ( > left right )
           ( progn ( setf sufix ( concatenate 'string sufix " |#" ) ) ( incf right ) )
           ( progn
            ( setf prefix ( concatenate 'string prefix "#| " ) )
            ( incf left ) ) ) )
     token ) ) )


#| "Middle step before parsing to xml" |# 


( defun tokenize-code-string ( code-string )
  ( let*
   ( ( buffer ( list nil ) )  #| final result, a list of lists and strings |#
                          ( token nil ) 
    #| will have a list of chars to make a string token |#
     ( token-separators ( list #\Space #\Newline ) )
    ( char-list ( string-to-char-list code-string ) ) 
    #| all chars in a list |#
     ( comments ( comments-in-char-list char-list ) ) 
    #| list of t and nil, with t signaling that char belongs to a comment |#
     ( strings ( string-components-in-char-list char-list ) ) 
    #| signal if a char belongs to a string |#
     ( sequences ( sequences-in-char-list char-list ) ) )
   
   #| signals the beginning and end of sequences |#
    ( loop for cha in char-list
         for com in comments
         for str in strings
         for seq in sequences
         do ( cond  #| inside string or comment |#
                    ( ( or str com ) ( push cha token ) ) 
                  #| or is a space or newline |#
                   ( ( member cha token-separators )
                   ( when token
                     
                     #| if there was a token |#
                      ( setf token ( reverse-and-format token ) )
                     ( push token ( car buffer ) )
                     
                     #| push it to front of list |#
                      ( setf token nil ) ) )
                  ( ( string= seq ")" )
                   ( progn
                    ( when token
                      ( setf token ( reverse-and-format token ) )
                      ( push token ( car buffer ) )
                      ( setf token nil ) )
                    ( push ( reverse ( pop buffer ) ) ( car buffer ) ) ) )
                  
                  #| seq is not ), because it passed the last condition so, if not nil, it is the beginning of one sequence |#
                   ( seq
                   ( progn
                    ( when
                        ( and token
                             ( not
                              ( string= seq ( coerce ( reverse token ) 'string ) ) ) )
                      ( setf token ( reverse-and-format token ) )
                      ( push token ( car buffer ) )
                      ( setf token nil ) )
                    ( push ( list seq ) buffer )
                    ( setf token nil ) ) )
                  ( t ( push cha token ) ) ) )
   ( car ( reverse ( pop buffer ) ) ) ) )


#| IMPORTAN , REPLACE ALL INSIDE COMNINATIONS OF # and | !!!! |# 


#| token is reversed |# 


( defun trim-comment-marks ( comment )
  ( let* ( ( str ( string-trim +COMM-TRIM+ comment ) ) ( len ( length str ) ) )
   ( if
    ( and ( > len 3 ) ( eq #\# ( char str 0 ) ) ( eq #\| ( char str 1 ) )
         ( eq #\| ( char str ( - len 2 ) ) ) ( eq #\# ( char str ( - len 1 ) ) ) )
    ( string-trim +COMM-TRIM+ ( subseq str 2 ( - len 2 ) ) ) str ) ) )


( defun token-to-xml ( token )
  "Outputs a xml-node"
  ( if ( stringp token )
      ( make-instance 'xml-node :tag +ATOM+ :attributes
       ( list ( cons +TYPE+ +CLASS-ATOM+ ) ( cons "class" +CLASS-ATOM+ ) ) :children
       ( list token ) )
      ( make-instance 'xml-node :tag +LIST+ :attributes
       ( list ( cons +LIST-TYPE+ ( car token ) ) ( cons +TYPE+ +CLASS-LIST+ )
             ( cons "class" +CLASS-LIST+ ) )
       :children ( mapcar #'token-to-xml ( rest token ) ) ) ) )


( defun code-string-to-xml ( code-string )
  ( token-to-xml
   ( tokenize-code-string
    ( concatenate 'string ( list #\( #\Newline ) code-string
                 ( list #\Newline #\) ) ) ) ) )
