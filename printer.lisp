

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


;;; Functions for pretty printing, similar to the standard pprint. Need some fixes. 


( in-package :pretty-lisp )


( defparameter +pprint-replacement-char+
  #\X
  "Tokens that may cause pprint problems are
replaced by strings/sequences of this char." )


( defparameter +dummy-char+ #\9 "Character to be used to replace comments." )


( defparameter +dummy-string+
  ( format nil "~A" +dummy-char+ )
  "String to be used as replacement for comments." )


( defgeneric parse-pretty-atom ( xml ) )


( defun comment-p ( str )
  "Checks if str contains a comment."
  ( and ( stringp str )
       ( or ( and ( > ( length str ) 0 ) ( eq #\; ( char str 0 ) ) )
           ( and ( > ( length str ) 1 ) ( eq #\# ( char str 0 ) )
                ( eq #\| ( char str 1 ) ) ) ) ) )


( defgeneric format-for-pprint-template ( xml )
            ( :documentation
             "Replaces all lists for simple lists and all text 
for sequences of X. The result will be something
like (XXX (XXX XXX) (XX)). Comments are discarded. 
This will be used as input to return a prototype 
of pprint (!)" ) )


( defmethod format-for-pprint-template ( ( xml pretty-list ) )
           ( format nil " ( ~{ ~A ~} ) "
                   ( or ( mapcar #'format-for-pprint-template ( xml-children xml ) )
                       ( list +dummy-string+ ) ) ) )


( defmethod format-for-pprint-template ( ( xml pretty-atom ) )
           ( let ( ( str ( xml-value xml ) ) )
             
             
             ;; insertion of dummy char is usefull to avoid
             ;; pprint converting () to nil
              ( if ( comment-p str ) +dummy-string+
                 ( if
                  ( or
                   ( find-if
                    #'( lambda ( c )
                      ( or ( char= c +dummy-char+ )
                          ( not ( or ( alpha-char-p c ) ( digit-char-p c ) ) ) ) )
                    str )
                   ( string-equal str nil ) )
                   
                  ;; if some undesirable char was found
                  ;; replace the all text for a sequence like XXXXX
                   ( make-string ( length str ) :initial-element
                   +pprint-replacement-char+ )
                  str ) ) ) )


( defun unfold-pretty-xml-to-list-of-string ( xml )
  "Takes the pretty atoms and lists and retrieves a
list of their text contents, list types and
boundaries, i.e, a list of strings."
  ( cond
   ( ( pretty-list-p xml )
    ( append ( list ( format nil "~A~A" ( xml-attribute-get xml +LIST-TYPE+ ) "(" ) )
            ( apply #'append
                   ( mapcar #'unfold-pretty-xml-to-list-of-string
                           ( xml-children xml ) ) )
            '( ")" ) ) )
   ( ( pretty-atom-p xml ) ( list ( xml-value xml ) ) )
   ( ( consp xml )
    ( apply #'append ( mapcar #'unfold-pretty-xml-to-list-of-string xml ) ) ) ) )


( defmethod pfile-pprint ( pfile &optional to-other-path )
           "Converts the pretty code to text and then saves
it to the file path slot value. The standard
pprint is used as an intermediate step to the
convertion."
           ( let ( ( final-stream ( make-string-output-stream ) )
                 ( sep-chars ( list #\Newline #\Space ) ) )
             ( dolist ( node ( pfile-xml pfile ) )
               ( let*
                ( ( list-code-text ( unfold-pretty-xml-to-list-of-string node ) )
                 ( length-list-code-text ( length list-code-text ) )
                 ( pprint-template
                  ( format nil "~%~A"
                          ( remove +dummy-char+
                                  ( let ( ( s ( make-string-output-stream ) )
                                        ( code
                                         ( format-for-pprint-template node ) ) )
                                    ( if code
                                        ( pprint ( read-from-string code ) s ) )
                                    ( get-output-stream-string s ) )
                                  :test #'char= ) ) )
                 ( list-pprint-text
                  ( if
                   ( = 0
                    ( length
                     ( string-trim ( coerce '( #\Newline #\Space ) 'string )
                      pprint-template ) ) )
                   '( )
                   ( unfold-pretty-xml-to-list-of-string
                    ( parse-pretty-atom
                     ( make-instance 'pretty-atom :children
                      ( list pprint-template ) ) ) ) ) )
                 ( cur-pos 0 ) ( is-comment )
                 ( cur-char ( char pprint-template cur-pos ) )
                 ( total ( length pprint-template ) ) ( separator nil ) )
                ( ;; DEBUG
                   when nil ( format final-stream "~%~A~%" pprint-template ) ) 
                ;; insert nil in the position comments should be
                 ( let ( ( index -1 ) )
                  ( dolist ( text list-code-text )
                    ( incf index )
                    ( if ( comment-p text )
                        ( setf list-pprint-text
                                ( append ( subseq list-pprint-text 0 index )
                                        ( list text )
                                        ( subseq list-pprint-text index ) ) ) ) ) )
                ( loop for text in list-code-text
                      for proto-text in list-pprint-text
                      do ( progn
                          ( setf is-comment
                                  ( comment-p text )
                                separator
                                  is-comment )
                          
                          
                          ;; while spaces or newlines in the prototype
                          ;; print them also to the final stream
                           ( loop while ( member cur-char sep-chars :test #'char= )
                                do ( progn
                                    ( format final-stream "~A" cur-char )
                                    
                                    ;; the lists in pprint prototype have no type
                                     ( incf cur-pos )
                                    ( if ( < cur-pos total )
                                        ( setf cur-char
                                                ( char pprint-template cur-pos ) )
                                        ( setf cur-char +dummy-char+ ) )
                                    ( setf separator t ) ) )
                          
                          
                          ;; FIXME this is a faail safe measure
                          ;; but produces not so well formatted code
                           ( if ( not separator ) ( format final-stream " " ) )
                          
                          
                          ;; when a normal atom or list, print and increase
                          ;; the position on the prototype
                           ( unless is-comment
                            ( progn
                             ( format final-stream "~A" text )
                             ( incf cur-pos ( length proto-text ) ) ) )
                          
                          
                          
                          ;; for comments, need to compute the current
                          ;; position on the line, print the comment and
                          ;; continue in the same position next line
                           ( when is-comment
                            ( let ( ( cur-line-length-so-far
                                   ( - cur-pos
                                    ( or
                                     ( position #\Newline pprint-template
                                               :from-end t :end
                                               ( min ( 1- cur-pos ) ( 1- total ) )
                                               :test #'char= )
                                     0 ) ) ) )
                              ( format final-stream "~A" text )
                              ( if ( > length-list-code-text 1 )
                                  ( format final-stream "~A" #\Newline ) )
                              ( format final-stream "~A"
                                      ( make-string ( 1- cur-line-length-so-far )
                                       :initial-element #\Space ) ) ) )
                          ( if ( < cur-pos total )
                              ( setf cur-char ( char pprint-template cur-pos ) )
                              ( setf cur-char +dummy-char+ ) ) ) )
                 ;; separate each top level piece of code with a fresh line
                  ( format final-stream "~A" #\Newline ) ) )
             
             ;; finnaly
              ( write-to-file ( or to-other-path ( pfile-path pfile ) )
              ( get-output-stream-string final-stream ) ) ) )


( ;; DEBUG
   when nil
 ( pfile-pprint
  ( or
   ( find ( format nil "/root/Coder/dev-pretty-lisp/~A.lisp" "transform" )
         *editing-files* :key #'pfile-path :test #'string-equal )
   ( car *editing-files* ) )
  ( format nil "/root/Coder/dev-pretty-lisp/~A.lisp" "test" ) ) )
