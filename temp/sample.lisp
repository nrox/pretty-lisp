

;;; pretty-LISP Editor (beta) 

;;; Copyright (c) 2012, Nuno Rocha.  All rights reserved. 

;; index sample 

( defun hello ( &optional ( name :world ) ) ( format nil "Hello, ~A!" name ) )

( hello "World" )

;; introduction sample 

( defun read-file ( path )
  "Read file and returns a string with its contents"
  ( with-open-file ( s path )
   ( let* ( ( len ( file-length s ) ) ( data ( make-string len ) ) )
    ( read-sequence data s ) ( values data ) ) ) )