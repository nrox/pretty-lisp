

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

;;;; FILES 

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

( defun folder-hierarchy ( folder-path &optional extensions )
  "Returns a list of files and folders inside folder-path,
and also the folder hierarchy to reach it."
  ( let*
   ( ( folder-path-name ( pathname folder-path ) )
    ( directory-list ( reverse ( pathname-directory folder-path-name ) ) )
    ( hierarchy ) ( auxiliar ) )
   ( setf hierarchy
           ( maplist #'( lambda ( dir ) ( make-pathname :directory ( reverse dir ) ) )
                    directory-list ) )
   ( setf hierarchy ( reverse hierarchy ) )
   ( setf auxiliar ( list-directory folder-path ) )
   ( setf auxiliar
           ( remove-if #'null auxiliar :key
            #'( lambda ( f )
              ( or ( null extensions )
                  ( find ( pathname-type f ) extensions :test #'string-equal )
                  ( string= ( namestring f ) ( directory-namestring f ) ) ) ) ) )
   ( setf hierarchy ( append hierarchy auxiliar ) )
   ( mapcar #'namestring hierarchy ) ) )

;;;; STRINGS 

( defun format-and-ucase ( att )
  ( declare ( optimize ( speed 3 ) ( safety 0 ) ) )
  ( string-upcase
   ( if ( stringp att )
       att
       ( format nil "~A" att ) ) ) )

( defun string-replace ( str1 sub1 sub2 )
  "Replace in str1 each occurrence of sub1 with sub2"
  
  ;;; http://www.tek-tips.com/viewthread.cfm?qid=1184743&page=2
   ( let ( ( str1 ( string str1 ) )
        ( str2 "" )
        ( sub1 ( string sub1 ) )
        ( sub2 ( string sub2 ) )
        ( index1 0 ) )
    ( loop
     ( if ( string-equal str1 sub1 :start1 index1 :end1
          ( min ( length str1 ) ( + index1 ( length sub1 ) ) ) )
         ( progn
          ( setq str2 ( concatenate 'string str2 sub2 ) )
          ( incf index1 ( length sub1 ) ) )
         ( progn
          ( setq str2
                  ( concatenate 'string str2 ( subseq str1 index1 ( 1+ index1 ) ) ) )
          ( incf index1 ) ) )
     ( unless ( < index1 ( length str1 ) ) ( return str2 ) ) ) ) )

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

( defun levenshtein ( s01 s02 &key ( ignore-case nil ) )
  "The Levenshtein distance between strings.
If limit is a number the function returns nil
if the computed distance is greater."
  
  ;; based on http://www.cliki.net/Levenshtein
   ( declare ( optimize ( speed 3 ) ( safety 0 ) )
           ( type string s01 s02 ) )
  ( let*
   ( ( s1
     ( if ignore-case
         ( string-downcase s01 )
         s01 ) )
    ( s2
     ( if ignore-case
         ( string-downcase s02 )
         s02 ) )
    ( width ( 1+ ( length s1 ) ) ) ( height ( 1+ ( length s2 ) ) )
    ( d ( make-array ( list height width ) ) ) )
   ( dotimes ( x width ) ( setf ( aref d 0 x ) x ) )
   ( dotimes ( y height ) ( setf ( aref d y 0 ) y ) )
   ( dotimes ( x ( length s1 ) )
     ( dotimes ( y ( length s2 ) )
       ( setf ( aref d ( 1+ y ) ( 1+ x ) )
               ( min ( 1+ ( aref d y ( 1+ x ) ) ) ( 1+ ( aref d ( 1+ y ) x ) )
                    ( + ( aref d y x )
                     ( if ( char= ( aref s1 x ) ( aref s2 y ) )
                         0
                         1 ) ) ) ) ) )
   ( aref d ( 1- height ) ( 1- width ) ) ) )

( defun levenshtein-ratio ( to-compare reference )
  "The Levenshtein distance, normalized with the reference/target length."
  
  ;; FIXME this function is a bit weird
   ( let* ( ( len ( length reference ) ) )
   ( if ( = len 0 )
       ( length to-compare )
       ( / ( levenshtein to-compare reference ) len ) ) ) )

;;;; MATH 

( defun farthest ( desc )
  "return the farthest point (x+w y+h) from a list of (x y w h)"
  ( let ( ( x-far 0 ) ( y-far 0 ) )
    ( dolist ( item desc ( list x-far y-far ) )
      ( setf x-far ( max x-far ( + ( first item ) ( third item ) ) ) )
      ( setf y-far ( max y-far ( + ( second item ) ( fourth item ) ) ) ) ) ) )

;;;; LISTS 

( defun truncate-list ( lst n )
  ( if ( <= ( length lst ) n )
      lst
      ( subseq lst 0 n ) ) )

( defun unfold ( tree )
  "Takes the tree and transform to a single list."
  ( if ( consp tree )
      ( apply #'append ( mapcar #'unfold tree ) )
      ( list tree ) ) )