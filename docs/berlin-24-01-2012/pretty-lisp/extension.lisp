

;;;; pretty-LISP - Common LISP IDE


;;;; © Nuno Rocha 2011


;;;;


;;;; this is a chrome extension to transform code in web pages


;;;; it works in association with the extension files


;;;; the code must be inserted in pre tags


(in-package :pretty-lisp )

(defparameter *debug0* "i" )

(defun insert-js-actions (new-editing-nodes preid )(let ((actions (make-instance 'xml-node :tag "dig" ))(reference (concatenate 'string "#" preid )))(xml-append-child actions (make-instance 'xml-node :tag "jquery" :attributes (list (cons "selector" reference )(cons "method" "replaceWith" )):children (list (make-instance 'xml-node :tag "div" :attributes (list (cons :id preid )(cons :class "prettycontainer" ))))));; insert containers and svg for the new and updated elements
(let (;; to insert the first node to the html div, use prepend
(jmethod "append" )(node-svg nil ))(dolist (root-node new-editing-nodes );; create containers and svg
(progn (set-id-all-family root-node )(setf (xml-parent root-node )nil )(setf node-svg (process-dimensioning root-node ))(setf node-svg (make-svg2 node-svg )));; create jquery methods to insert the svg into the right place
(xml-append-child actions (make-instance 'xml-node :tag "jquery" :attributes (list (cons "selector" reference )(cons "method" jmethod )):children (list (make-instance 'xml-node :tag "div" :attributes (list (cons :id (concatenate 'string "p" (xml-attribute-get root-node :roxid )))(cons :class "pcodecontainer" )):children (list node-svg )))))))actions ))

(defun chromex (xml )(make-instance 'xml-node :tag "dig" :children (mapcar #'(lambda (xml-item )(insert-js-actions (parse-pretty-atom (make-instance 'pretty-atom :children (list (format nil "~A" (xml-value-decoded xml-item )))))(format nil "~A" (xml-tag xml-item ))))(xml-children xml ))))