

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

( in-package :cl-user )

( mapcar #'ql:quickload '( :hunchentoot :s-xml :cl-fad ) )

( defpackage "pretty-lisp"
  ( :nicknames :pretty-lisp )
  ( :use :cl :hunchentoot :s-xml :cl-fad :flexi-streams )
  ( :export :up :down :pretty-code ) )

( in-package :pretty-lisp )

( defparameter *auto-start* t )

( defparameter *compile-files*
  nil
  "Files are compiled and then the .fasl loaded (t) or just the .lisp loaded (nil)" )

( defparameter +source-folder+
  ( directory-namestring *load-pathname* )
  "The folder where this file is when loaded" )

( defparameter *source-files*
  '( "security" "utils" "xml" "javascript" "parser" "files" "printer" "layout"
   "edit" "hint" "extension" "requests" )
  "source files for this package, without their .lisp extension,
must be found in the same folder where package.lisp is" )

( mapcar
 #'( lambda ( filename )
   ( let ( ( file-path ( concatenate 'string +source-folder+ filename ".lisp" ) ) )
     ( if *compile-files*
         ( load ( namestring ( compile-file file-path ) ) )
         ( load file-path ) ) ) )
 *source-files* )

( if *auto-start*
    ( up ) )