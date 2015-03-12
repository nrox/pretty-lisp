

;;;; pretty-LISP - Common LISP Editor 


;;;;   Nuno Rocha 2012 


( in-package :pretty-lisp )


( defvar +jquery-0+ '( "empty" "remove" ) )


( defvar +jquery-1+ '( "after" "before" "prepend" "html" "replaceWith" ) )


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
