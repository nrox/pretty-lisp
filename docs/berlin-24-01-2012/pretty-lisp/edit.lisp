

;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :pretty-lisp )

(defun topsubmenu (xml )(declare (ignore xml ))(let* ((title (make-instance 'xml-node :tag "span" :children (list "paths:" )))(file-operations nil ))(dolist (operation '("browse" "open" "load" "new" "save" "saveas" "close" "undo" "redo" )nil )(push (make-instance 'xml-node :tag "button" :attributes (list (cons :operation operation )(cons :class "fileoperation" )(cons :onclick (format nil "fileOp('~A')" operation ))):children (list operation ))file-operations ))(setf file-operations (reverse file-operations ))(make-instance 'xml-node :tag "setchildren" :attributes (list (cons "id" "topsubmenu" )):children (append (list title (cur-paths-options *DEFAULT-FOLDER* ))file-operations ))))

(defun undo-redo (pfile &optional (direction :undo ))(let* ((pointer (pfile-pointer pfile ))(history (pfile-history pfile )))(if (eql direction :undo )(incf pointer )(decf pointer ))(when (> pointer (- (length history )1 ))(setf pointer (- (length history )1 )))(when (< pointer 0 )(setf pointer 0 ))(setf (pfile-pointer pfile )pointer )(setf (pfile-xml pfile )(copy-list (nth pointer history )))))

(defun copy-state (xml )#| make a copy of the element that will change |# #| so that the changes will not afect history and make undo redo coherent |# (let* ((svg (car (xml-children xml )))(operation (xml-attribute-get svg :operation ))(roxid (xml-attribute-get svg :roxid ))(pfile (roxid-pfile roxid ))(current (copy-list (pfile-xml pfile )))(element-that-will-change (find-ancestor (find-by-attribute current :roxid roxid )))(copy nil ))(when (find operation '(:update :paste :delete :cut :before :inside :after :surround :free :transpose :comment ):test #'string-equal )(setf copy (mapcar #'(lambda (node )(if (eq element-that-will-change node )(xml-copy node )node ))current ));; what will be edited later is the copy
(setf (pfile-xml pfile )copy ))))

(defun save-state-to-history (pfile current &optional (operation :update ));; save the current editing nodes to history
(let ((history (pfile-history pfile )));; this is necessary only for some editing operations
(when (find operation '(:update :paste :delete :cut :before :inside :after :surround :free :transpose :comment ):test #'string-equal );; if this is processed after a undo redo operation the
;; history more recent than the pointer position is lost
(setf history (subseq history (pfile-pointer pfile )))(setf (pfile-pointer pfile )0 )#| save to history |# (push (copy-list current )history )#| limit the history size |# (when (> (length history )+MAX-UNDO+ )(setf history (subseq history 0 +MAX-UNDO+ )))#| replace the actual history |# (setf (pfile-history pfile )history )#| set current as the working state, nodes list |# (setf (pfile-xml pfile )(copy-list current ))#| process auto-saving for some criteria |# (pfile-auto-save pfile ))))

(defun set-as-focus-element (xml )(if (xml-node-p xml )(xml-attribute-set xml :focus "focus" )))

(defun remove-as-focus-element (xml )(if (xml-node-p xml )(xml-attribute-set xml :focus nil )))

(defun get-focus-element (editing-nodes )(find-by-attribute editing-nodes :focus "focus" ))

#| editing lists and atoms |# 

(defun navigate (elem parent editing-nodes operation )"navigating trough nodes with arrow keys" (let* ((siblings (if parent (xml-children parent )editing-nodes ))(pos (position elem siblings ))(len (length siblings ))(prev-sibling (nth (mod (1- pos )len )siblings ))(next-sibling (nth (mod (1+ pos )len )siblings ))(child (and (pretty-list-p elem )(car (xml-children elem ))))(v '(0 1 ))(h '(1 0 ))(layout (or (and parent (xml-property-get parent :layout ))v )))(when (eq prev-sibling elem )(setf prev-sibling nil )(setf next-sibling nil ))(cond ((string-equal operation :up )(or (and (equal layout v )prev-sibling )(and (or (equal layout h )(= len 1 ))parent )elem ))((string-equal operation :down )(or (and (equal layout v )next-sibling )(and (or (equal layout h )(= len 1 ))(or child parent ))elem ))((string-equal operation :left )(or (and (equal layout h )prev-sibling )(and (or (equal layout v )(= len 1 ))parent )elem ))((string-equal operation :right )(or (and (equal layout h )next-sibling )(and (or (equal layout v )(= len 1 ))child )elem ))(t elem ))))

#||# 

(defun minimize-and-expand (previous-response ancestor editing-nodes operation )"Changing the container height to have some kind of code-colapse and code-expand
To only one container or all of them" (if (find operation '(:collapse :collapseall :expand :expandall ):test #'string-equal )(let* ((only-one (find operation '(:collapse :expand ):test #'string-equal ))(to-process (if only-one (list ancestor )editing-nodes )))(make-instance 'xml-node :tag "dig" :children (cons previous-response (mapcar #'(lambda (node )(make-instance 'xml-node :tag "decodeandeval" :children (list (url-encode (format nil "$(\"[roxid='~A']\").parent().attr(\"height\",\"~A\");" (xml-attribute-get node :roxid )(if (find operation '(:collapse :collapseall ):test #'string-equal )(min 70 (+ +FACTOR-H+ (read-from-string (xml-attribute-get node :height )))); (+ +FACTOR-H+ (read-from-string (xml-attribute-get node :height )))          
(+ 1 +MARGIN-Y+ (read-from-string (xml-attribute-get node :height ))(read-from-string (xml-attribute-get node :y )))))))))to-process ))))previous-response ))

(defgeneric make-triggered-event (obj evtType ))

(defmethod make-triggered-event (obj evtType ))

(defmethod make-triggered-event ((xml xml-node )evtType )(make-instance 'xml-node :tag "decodeandeval" :children (list (url-encode (format nil "roxEvent('~A','~A')" (xml-attribute-get xml :roxid )evtType )))))

(defun surround-with-dig (obj1 obj2 )(make-instance 'xml-node :tag "dig" :children (list obj1 obj2 )))

(defgeneric get-all-texts (xml &optional buffer ))

(defmethod get-all-texts ((xml xml-node )&optional buffer )(dolist (child (xml-children xml )buffer )(setf buffer (union buffer (get-all-texts child ):test #'string= ))))

(defmethod get-all-texts (xml &optional buffer )(cond ((consp xml )(dolist (child xml buffer )(setf buffer (union buffer (get-all-texts child ):test #'string= ))))((pfile-manager-p xml )(setf buffer (union buffer (get-all-texts (pfile-xml xml )):test #'string= )))(t (list (format nil "~A" xml )))))

(defgeneric get-all-atoms-with-text (xml text &optional buffer ))

(defmethod get-all-atoms-with-text ((xml pretty-atom )text &optional buffer )(declare (ignore buffer ))(if (string= (xml-value xml )text )(list xml )nil ))

(defmethod get-all-atoms-with-text (xml text &optional buffer )(cond ((consp xml )(dolist (child xml buffer )(setf buffer (union buffer (get-all-atoms-with-text child text )))))((pfile-manager-p xml )(setf buffer (union buffer (get-all-atoms-with-text (pfile-xml xml )text ))))((pretty-list-p xml )(dolist (child (xml-children xml )buffer )(setf buffer (union buffer (get-all-atoms-with-text child text )))))(t nil )))

(defun hint-complete (xml-answer texto )(setf texto (format-form texto ))(let* ((atoms-list (remove-duplicates (append (get-all-texts *EDITING-FILES* )+HYPERSPEC-LIST+ ):test #'string-equal )); (len-texto (length texto ))
(completions (truncate-list (sort (remove-if-not #'(lambda (txt )(search texto (format-form txt )))atoms-list )#'string< )+MAX-HINTS+ )))(surround-with-dig xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "hintslist" )):children (mapcar #'(lambda (item )(make-instance 'xml-node :tag "div" :attributes (list (cons "class" "hint-complete" )(cons "onclick" (format nil "hintComplete(\"~A\", true)" (string-replace (string-replace item "\\" "\\\\" )"\"" "\\\"" )))):children (list item )))completions )))))

(defun hint-similar (xml-answer texto )(setf texto (format-form texto ))(let* ((atoms-list (remove-duplicates (append (get-all-texts *EDITING-FILES* )+HYPERSPEC-LIST+ ):test #'string= :key #'format-form ))(len-texto (length texto ))(ref-len (* len-texto 1.34 ))(completions (sort (remove-if-not #'(lambda (txt )(setf txt (format-form txt ))(and (< (length txt )ref-len )(< (levenshtein-ratio txt texto )0.34 )))atoms-list )#'string< )))(surround-with-dig xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "hintslist" )):children (mapcar #'(lambda (item )(make-instance 'xml-node :tag "div" :attributes (list (cons "class" "hint-complete" )(cons "onclick" (format nil "hintComplete(\"~A\", true)" (string-replace (string-replace item "\\" "\\\\" )"\"" "\\\"" )))):children (list item )))completions )))))

(defun hint-history (xml-answer )(surround-with-dig xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "hintslist" )):children (mapcar #'(lambda (item )(make-instance 'xml-node :tag "div" :attributes (list (cons "class" "hint-complete" )(cons "onclick" (format nil "hintComplete(\"~A\", true)" (string-replace (string-replace item "\\" "\\\\" )"\"" "\\\"" )))):children (list item )))*HINTS-HISTORY* ))))

(defun hint-hyperspec (xml-answer xml-request )(surround-with-dig xml-answer (loadhyperspec xml-request )))

(defun hint-search (xml-answer pfile texto )(let* ((completions (mapcar #'(lambda (node )(xml-attribute-get node :roxid ))#| really truncate it? find another solution, as a display element with scrolls |# (truncate-list (get-all-atoms-with-text pfile texto )+MAX-HINTS+ )#| (sort (truncate-list (get-all-atoms-with-text pfile texto )+MAX-HINTS+ )#'< :key #'(lambda (node )(let (int )(setf int (parse-integer (xml-attribute-get node :y ):junk-allowed t ))(if int int 0 )))) |# )))(surround-with-dig xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "hintslist" )):children (mapcar #'(lambda (item )(make-instance 'xml-node :tag "div" :attributes (list (cons "class" "hint-complete" )(cons "onclick" (format nil "goToElement('~A');roxEvent('~A','lastclick');" item item ))):children (list item )))completions )))))

(defun hint-suggest (xml-answer texto suggest-function )(let* ((suggestions (sort (truncate-list (remove-duplicates (mapcar #'(lambda (at )(funcall suggest-function at (xml-parent at )))(get-all-atoms-with-text *EDITING-FILES* texto )):test #'string= )+MAX-HINTS+ )#'(lambda (x y )(< (length x )(length y ))))))(surround-with-dig xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "hintslist" )):children (mapcar #'(lambda (item )(make-instance 'xml-node :tag "div" :attributes (list (cons "class" "hint-complete" )(cons "onclick" (format nil "hintComplete(\"~A\", true)" (string-replace (string-replace item "\\" "\\\\" )"\"" "\\\"" )))):children (list item )))suggestions )))))

(defgeneric make-suggestion (elem xml ))

(defmethod make-suggestion (elem xml )"nil" )

(defmethod make-suggestion ((elem pretty-atom )(xml pretty-list ))(concatenate 'string "(" (reduce #'(lambda (str next )(concatenate 'string str (cond ((eq elem next )(xml-value next ))((pretty-atom-p next )"nil" )((pretty-list-p next )"(nil)" )(t nil ))" " ))(xml-children xml ):initial-value " " )")" ))

(defgeneric make-examples (elem xml ))

(defmethod make-examples (elem xml )"nil" )

(defmethod make-examples ((elem pretty-atom )(xml pretty-list ))(concatenate 'string "(" (reduce #'(lambda (str next )(concatenate 'string str (cond ((pretty-atom-p next )(xml-value next ))((pretty-list-p next )(concatenate 'string "(" (reduce #'(lambda (str node )(concatenate 'string str (if (pretty-atom-p node )(xml-value node )"(#|...|#)" )" " ))(xml-children next ):initial-value " " )" )" )))" " ))(xml-children xml ):initial-value " " )")" ))

(defgeneric make-def (elem xml ))

(defmethod make-def (elem xml ))

(defmethod make-def ((elem pretty-atom )(xml pretty-list ))(let* ((def-atom (car (xml-children xml )))(len-children (length (xml-children xml )))(def-atom-text (if (pretty-atom-p def-atom )(xml-value def-atom ))); (len-def-atom-text (length def-atom-text ))
)(if (and def-atom-text (> len-children 2 )(> (length def-atom-text )3 )(string= "def" (subseq def-atom-text 0 3 ))(string/= (xml-value elem )def-atom-text ))(concatenate 'string "(" (reduce #'(lambda (str next )(concatenate 'string str " " (xml-to-code-string next )))(xml-children xml ):initial-value " " :end 3 )(if (> len-children 3 )"(#|...|#)" )")" )(xml-value elem ))))

(defun fileoperation (xml )(let* ((ret nil )(files nil )(button (car (xml-children xml )))(operation (xml-attribute-get button :operation ))(id (xml-attribute-get button :value ))(newfile (xml-attribute-get button :newvalue ))(path nil )(auxiliar nil )(pfile (find id *EDITING-FILES* :key #'pfile-path :test #'string= )))(setf ret (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "bottomstatus" )):children (list operation id )))(when (string-equal operation :browse )(setf auxiliar (pathname newfile ))(setf *DEFAULT-FOLDER* (directory-namestring auxiliar ))(setf files (folder-hierarchy *DEFAULT-FOLDER* '("lisp" "cl" "pretty" "el" )))(setf ret (make-instance 'xml-node :tag "dig" :children (list ret (make-instance 'xml-node :tag "replace" :attributes (list (cons "setid" "fileslist" )):children (list (cur-paths-options newfile )))))))(when (string-equal operation :undo )(undo-redo pfile :undo ))(when (string-equal operation :redo )(undo-redo pfile :redo ))(when (find operation '(:redo :undo ):test #'string-equal )(setf ret (prettyfile xml )))(when (find operation '(:open ):test #'string-equal )(setf path newfile )(if (and (probe-file path )(not (is-directory path )))(setf ret (prettyfile xml ))(xml-append-child ret "CANCELED: FILE DOES NOT EXIST" )))(when (string-equal operation :load )(load newfile ))(when (find operation '(:new ):test #'string-equal )(setf path (concatenate 'string "" newfile ))(if (probe-file path )(xml-append-child ret "CANCELED: FILE ALREADY EXISTS" )(progn (write-to-file path ";;;;" )(setf ret (prettyfile xml )))))(when (string-equal operation :close )(setf +AUTO-SAVE-COUNTER+ +AUTO-SAVE-LIMIT+ )(pfile-auto-save pfile )(setf *EDITING-FILES* (remove pfile *EDITING-FILES* ))(setf ret (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "idlispide" )):children (list " " ))))(when (string-equal operation :saveas )(xml-append-child ret "--}" )(xml-append-child ret newfile )(setf auxiliar (pathname newfile ))(setf (pfile-path pfile )newfile )(setf (pfile-name pfile )(file-namestring auxiliar ))(setf ret (make-instance 'xml-node :tag "dig" :children (list ret (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "curfileid" )):children (list newfile ))))))(when (find operation '(:save :saveas ):test #'string-equal )(pfile-backup pfile )(pfile-save pfile ))ret ))

(defun transpose-layout (event-element )"Switches the layout of the list {horizontal, vertical}.
The internal representation is a list { (0 1), (1 0) }" (let (;; if the layout is user defined, then get it
(layout (xml-attribute-get event-element :layout )));; The attribute is a string. Need to convert to a list
;; If the layout is not user-defined get the default
(if layout (setf layout (read-from-string layout ))(setf layout (pretty-layout event-element )));; the transposition is the simple reverse of the layout
(setf layout (reverse layout ));; set the new layout as an attribute of the element
(xml-attribute-set event-element :layout (format nil "~A" layout ))))

(#| CHANGED|# defun free-element (event-element eltype parent siblings pos event-svg editing-nodes )"If the element is a list, this functions replaces the list with its children
If it is a comment, it removes the comment marks,
so that tha comment words are changed to code" (let ((freed-elements )(insert-position ))(when (string-equal :atom eltype )(let ((text (trim-comment-marks (xml-value event-svg ))))(xml-value-set event-element (if text text "" ))(setf freed-elements (parse-pretty-atom event-element ))))(when (string-equal :list eltype )(setf freed-elements (xml-children event-element )))(set-as-focus-element (car freed-elements ))(when parent (setf (xml-children parent )nil )(dolist (child (append (subseq siblings 0 pos )freed-elements (subseq siblings (+ pos 1 ))))(xml-append-child parent child )))(unless parent (dolist (elem freed-elements )(setf (xml-parent elem )nil ))(setf insert-position (position event-element editing-nodes ))(setf editing-nodes (append (subseq editing-nodes 0 insert-position )freed-elements (subseq editing-nodes (1+ insert-position )))))editing-nodes ))

(#| CHANGED|# defun update-element (event-element eltype parent siblings pos event-svg editing-nodes )"Parses the event-element text and updates the code xml structure" (let (;; the new text is the value received with the event-svg
(text (or (xml-value event-svg )"" ))(new-elements )(insert-position ));; if the element is a list the text is the list type
(when (string-equal :list eltype )(xml-attribute-set event-element :listtype text )(set-as-focus-element event-element ));; if its an atom the new text must be parsed, and the resulting elements
;; inserted in the place of the element
(when (string-equal :atom eltype )(xml-value-set event-element text )(setf new-elements (parse-pretty-atom event-element ))(set-as-focus-element (car new-elements ))(when parent (setf (xml-children parent )nil )(dolist (child (append (subseq siblings 0 pos )new-elements (subseq siblings (+ pos 1 )))nil )(xml-append-child parent child )))(unless parent (dolist (elem new-elements )(setf (xml-parent elem )nil ))(setf insert-position (position event-element editing-nodes ))(setf editing-nodes (append (subseq editing-nodes 0 insert-position )new-elements (subseq editing-nodes (1+ insert-position ))))))editing-nodes ))

(defun comment-element (event-element parent pos )"Returns a pretty-atom with the textual code represented by event-element preceeded by a comment mark" (let* (;; the comment text is the textual representation of the code
;; preceeded with a comment mark
(text (concatenate 'string ";" (xml-to-code-string event-element )));; the internal representation of the comment is an pretty-atom
;; with the comment text as unique children
(comment-element (make-instance 'pretty-atom :children (list text ):attributes (list (cons :class +CLASS-ATOM+ )(cons :type +CLASS-ATOM+ )):parent parent )));; if the event-element has a parent, replace the event-element with the generated comment
(if parent (setf (nth pos (xml-children parent ))comment-element ))comment-element ))

(defun surround-element (event-element parent siblings )"Sorround the element with closed parentheses" (let ((newlist (make-instance 'pretty-list :attributes (list (cons :class +CLASS-LIST+ )(cons :type +CLASS-LIST+ )(cons :listtype "" )):parent parent )))(xml-append-child newlist (xml-copy event-element ))(when parent (setf (xml-children parent )(substitute newlist event-element siblings )))newlist ))

(defun insert-element (event-element operation parent pos siblings editing-nodes )"inserts a new element, depending on the operation: 
- :inside event-element (if a list)
- :before event-element
- :after event-element" (let ((newatom (make-instance 'pretty-atom :children (list "#||#" ):attributes (list (cons :class +CLASS-ATOM+ )(cons :type +CLASS-ATOM+ )):parent parent ))(insert-position ))(when (and (string-equal operation :inside )(pretty-list-p event-element ))(setf (xml-children event-element )(cons newatom (xml-children event-element )))(setf (xml-parent newatom )event-element )(set-as-focus-element newatom ))(when (and (string-equal operation :inside )(pretty-atom-p event-element ))(setf (xml-children event-element )(list "#||#" ))(set-as-focus-element event-element ))(when (and parent (find operation '(:before :after ):test #'string-equal ))(if (string-equal :before operation )(setf insert-position pos );; else :after
(setf insert-position (+ 1 pos )))(setf (xml-children parent )(append (subseq siblings 0 insert-position )(list newatom )(subseq siblings insert-position )))(set-as-focus-element newatom ))(when (and (not parent )(find operation '(:after :before ):test #'string-equal ))(set-as-focus-element newatom )(setf insert-position (position event-element editing-nodes ))(if (string-equal operation :after )(incf insert-position ))(setf editing-nodes (append (subseq editing-nodes 0 insert-position )(list newatom )(subseq editing-nodes insert-position ))))editing-nodes ))

(defun delete-element (event-element parent siblings pos editing-nodes )(if parent (progn (setf (xml-children parent )(remove event-element siblings ))(if (xml-children parent )(set-as-focus-element (nth (max 0 (1- pos ))(xml-children parent )))(set-as-focus-element parent )));; else (no parent - top level)
(let ((insert-position (position event-element editing-nodes )));; avoid empty editing nodes by inserting a dummy node
(if (= 1 (length editing-nodes ))(setf editing-nodes (append editing-nodes (list (make-instance 'pretty-atom :children (list "#||#" ):attributes (list (cons :class +CLASS-ATOM+ )(cons :type +CLASS-ATOM+ )))))))(setf editing-nodes (remove event-element editing-nodes ))(set-as-focus-element (nth (max 0 (1- insert-position ))editing-nodes ))))editing-nodes )

(defun update-editing-nodes (xml-ret pfile new-editing-nodes )(let ((old-editing-nodes (copy-list (pfile-xml pfile )))(actions (make-instance 'xml-node :tag "dig" :children (list xml-ret ))));; remove the nodes that were deleted or updated
(dolist (root-node (copy-list old-editing-nodes ))(unless (member root-node new-editing-nodes )(xml-append-child actions (make-instance 'xml-node :tag "jquery" :attributes (list (cons "selector" (concatenate 'string "#p" (xml-attribute-get root-node :roxid )))(cons "method" "remove" ))))(setf old-editing-nodes (remove root-node old-editing-nodes ))));; insert containers and svg for the new and updated elements
(let ((reference "#nodescontainer" );; to insert the first node to the html div, use prepend
(jmethod "prepend" )(node-svg nil ))(dolist (root-node new-editing-nodes );; to insert the rest use after
;; and when a node already exists, use it as reference
(when (member root-node old-editing-nodes )(setf reference (concatenate 'string "#p" (xml-attribute-get root-node :roxid ))jmethod "after" ));; when its is a new root member, create containers and svg
(unless (member root-node old-editing-nodes );; make the svg for the root-node
(setf *test* root-node )(progn (set-id-all-family root-node )(xml-attribute-set root-node :parent nil )(setf node-svg (process-dimensioning root-node ))(setf node-svg (make-svg2 node-svg )));; create jquery methods to insert the svg into the right place
(xml-append-child actions (make-instance 'xml-node :tag "jquery" :attributes (list (cons "selector" reference )(cons "method" jmethod )):children (list (make-instance 'xml-node :tag "div" :attributes (list (cons :id (concatenate 'string "p" (xml-attribute-get root-node :roxid )))(cons :class "pcodecontainer" )):children (list node-svg )))));; having inserted the node, use it further as reference
;; to apply later the method after
(setf reference (concatenate 'string "#p" (xml-attribute-get root-node :roxid ))jmethod "after" ))))actions ))

(defun editelement (request-xml )"This process the xml request for purposes of editing the elements." ;; identify the file which is being edited
;; and make a backup of the actual state
(copy-state request-xml )(let* (;; keeps track of the element to be signaled as the 
;; changed element
(track nil );; the svg element which co-activated the editing event
;; is the first (and unique!) children of the xml request
(event-svg (car (xml-children request-xml )));; the type of svg element {list, atom, list descriptor}
(eltype (xml-attribute-get event-svg :type ));; the event operation {:execute, :delete, ...}
(operation (xml-attribute-get event-svg :operation ));; the unique id of the element
(roxid (if (string-equal eltype :listdescriptor )(xml-attribute-get event-svg :tgt )(xml-attribute-get event-svg :roxid )));; The opened file which contains the element.
;; Several files may be opened. An alternative is to identify
;; the file as an attribute in the svg element
(pfile (roxid-pfile roxid ));; list of xml nodes, representing the code in the file
;; each element of the xml list represents a code list or an atom
(editing-nodes (copy-list (pfile-xml pfile )));; find the xml node corresponding to the event element
(event-element (find-by-attribute editing-nodes :roxid roxid ));; the parent of the element (a list, if any)
(parent (xml-parent event-element ));; if the parent (list) exists,
;;the siblings are other elements inside the parent (list)
(siblings (if parent (xml-children parent )));; the toplevel element (list) for which
;; the event element is one of the descendant
(ancestor (find-ancestor event-element ));; the position of the event element among siblings
(pos (if parent (position event-element siblings )0 ));; the xml to be returned as the answer to the editing event
(ret (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "bottomstatus" )):children (list operation ))));; remove the classes that are used in the interface
;; to colour newly edited elements
(remove-editing-class ancestor );; if :cancel, set the focus in the same element
(if (string-equal operation :cancel )(set-as-focus-element event-element ));; if collapsing or expanding, set next focus on the top level element
(if (find operation '(:collapse :collapseall :expand :expandall ):test #'string-equal )(set-as-focus-element ancestor ));; transposition of the layout {vertical, horizontal}
(when (string-equal operation :transpose )(transpose-layout event-element )(set-as-focus-element event-element ));; transforms the pretty element into a comment
(when (string-equal operation :comment )(let ((comment-atom (comment-element event-element parent pos )))(set-as-focus-element comment-atom )(if (eq ancestor event-element );; sets the comment-atom as the top level replacement for event-element
(setf editing-nodes (substitute comment-atom event-element editing-nodes )))));; code execution with eval
(when (string-equal operation :execute )(setf (xml-children ret )(list (format nil "~A" (eval (read-from-string (xml-to-code-string event-element ))))))(set-as-focus-element event-element ));; update the element text, or parse it to new elements
(if (string-equal :update operation )(setf editing-nodes (update-element event-element eltype parent siblings pos event-svg editing-nodes )));; remove comment marks or replace the list with its children
(if (string-equal :free operation )(setf editing-nodes (free-element event-element eltype parent siblings pos event-svg editing-nodes )));; surround the element with a closed parentheses
(when (string-equal :surround operation )(let ((newlist (surround-element event-element parent siblings )))(set-as-focus-element newlist )(if (eq ancestor event-element )(setf editing-nodes (substitute newlist event-element editing-nodes )))));; when cut or copy place the element in memory
(when (find operation '(:copy :cut ):test #'string-equal )(setf *EDIT-MEMORY* (xml-copy event-element ))(if (string-equal :copy operation )(set-as-focus-element event-element )));; delete or cut removes the element
(when (find operation '(:delete :cut ):test #'string-equal )(setf editing-nodes (delete-element event-element parent siblings pos editing-nodes )));; the paste operation
(when (and (string-equal :paste operation )*EDIT-MEMORY* )(let ((memory (xml-copy *EDIT-MEMORY* )))(set-id-all-family memory )(setf (xml-parent memory )parent )(set-as-focus-element memory )(if parent (setf (nth pos (xml-children parent ))memory );;else
(progn (setf ancestor memory )(setf editing-nodes (substitute memory event-element editing-nodes ))))));; inserting inside, before and after
(if (find operation '(:after :before :inside ):test #'string-equal )(setf editing-nodes (insert-element event-element operation parent pos siblings editing-nodes )));; update nodes in browser (1)
(when (find operation '(:update :transpose :comment :free :surround :cut :delete :paste :after :before :inside ):test #'string-equal );; ensure that the ancestor will be replaced
(setf editing-nodes (substitute (xml-copy ancestor )ancestor editing-nodes ));; control the focus element
(let ((focus (get-focus-element editing-nodes )))(when focus (setf track focus )(remove-as-focus-element focus )));; update nodes in browser (1)
(setf ret (update-editing-nodes ret pfile editing-nodes )));; mark focus
(when (find operation '(:copy :cancel :execute :collapse :collapseall :expand :expandall ):test #'string-equal );; control the focus element
(let ((focus (get-focus-element editing-nodes )))(when focus (setf track focus )(remove-as-focus-element focus ))))(when (find operation '(:up :down :left :right ):test #'string-equal )(setf track (navigate event-element parent editing-nodes operation )))(save-state-to-history pfile editing-nodes operation )(setf ret (surround-with-dig ret (make-triggered-event track "lastclick" )))(minimize-and-expand ret ancestor editing-nodes operation )))

(defun hint (xml )(let* ((button (car (xml-children xml )))(operation (xml-attribute-get button :operation ))(file (xml-attribute-get button :file ))(texto (xml-attribute-get button :texto ))(pfile (find file *EDITING-FILES* :key #'pfile-path :test #'string= ))(xml-answer (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "bottomstatus" )):children (list operation texto ))))(setf *HINTS-HISTORY* (remove texto *HINTS-HISTORY* :test #'string= ))(if (not (string= texto "" ))(push texto *HINTS-HISTORY* ))(setf *HINTS-HISTORY* (truncate-list *HINTS-HISTORY* +MAX-HINTS+ ))(when (string-equal operation :complete )(setf xml-answer (hint-complete xml-answer texto )))(when (string-equal operation :similar )(setf xml-answer (hint-similar xml-answer texto )))(when (string-equal operation :search )(setf xml-answer (hint-search xml-answer pfile texto )))(when (string-equal operation :history )(setf xml-answer (hint-history xml-answer )))(when (string-equal operation :suggest )(setf xml-answer (hint-suggest xml-answer texto #'make-suggestion )))(when (string-equal operation :examples )(setf xml-answer (hint-suggest xml-answer texto #'make-examples )))(when (string-equal operation :def )(setf xml-answer (hint-suggest xml-answer texto #'make-def )))(when (string-equal operation :hyperspec )(setf xml-answer (hint-hyperspec xml-answer texto )))xml-answer ))