

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

( defvar +jquery-0+ '( "empty" "remove" ) )

( defvar +jquery-1+ '( "after" "before" "prepend" "html" "replaceWith" "append" ) )

( defvar +jquery-2+ '( "attr" "css" ) )

( defun wrap ( children &key ( tag "dig" ) )
  "Surround the xml children list with xml parent"
  ( make-instance 'xml-node :tag tag :children children ) )

( defun jquery-0 ( jselector jmethod )
  "Makes the xml to be interpreted as a jquery instruction with no arguments."
  ( make-instance 'xml-node :tag "jquery" :attributes
   ( list ( cons "selector" jselector ) ( cons "method" jmethod ) ) ) )

( defun jquery-1 ( jselector jmethod &rest jvalue )
  "Makes the xml to be interpreted as a jquery instruction with 1 argument."
  ( make-instance 'xml-node :tag "jquery" :attributes
   ( list ( cons "selector" jselector ) ( cons "method" jmethod ) ) :children
   ( unfold jvalue ) ) )

( defun jquery-2 ( jselector jmethod arg1 &rest rest-args )
  "Makes the xml to be interpreted as a jquery instruction with 2 arguments."
  ( make-instance 'xml-node :tag "jquery" :attributes
   ( list ( cons "selector" jselector ) ( cons "method" jmethod )
         ( cons "arg1" arg1 ) )
   :children ( unfold rest-args ) ) )

( defun js-predefined ( predefined-fun &rest rest-args )
  ( make-instance 'xml-node :tag "predefined" :attributes
   ( list ( cons "method" predefined-fun ) ) :children
   ( mapcar
    #'( lambda ( item ) ( make-instance 'xml-node :tag "arg" :children ( list item ) ) )
    ( unfold rest-args ) ) ) )

( defun set-status ( status ) ( jquery-1 "#bottomstatus" "html" status ) )