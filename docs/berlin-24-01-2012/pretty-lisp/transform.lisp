

;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :pretty-lisp )

(defparameter +STRING+ "STRING" )

(defparameter +CODE-TAG+ "div" )

(defparameter +MARGIN-X+ 19 #| 16 |# )

(defparameter +MARGIN-Y+ 7 #| 7 |# )

(defparameter +MARGIN-X2+ 20 #| 26 |# )

(defparameter +MARGIN-Y2+ 7 #| 7 |# )

(defparameter +TEXT-HEIGHT+ 11 #| 11 |# )

(defparameter +FACTOR-LEN+ 10 #| 9 |# )

(defparameter +FACTOR-H+ 3 )

(defparameter +MARGIN-LIST-DESC+ 2 )

(defparameter *test2* nil )

(defparameter +X+ "x" )

(defparameter +Y+ "y" )

(defparameter +WIDTH+ "width" )

(defparameter +HEIGHT+ "height" )

(defparameter +TEXT-LENGTH+ "textLength" )

(defparameter +RX+ "7" )

(defparameter +RY+ "10" )

(defparameter +SVG-TEXT+ "text" )

(defparameter +ID+ "id" )

(defparameter +LAYOUT+ "layout" )

(defparameter +TAG-CONT+ "div" )

(defparameter *EDIT-MEMORY* nil )

(defparameter +HYPERSPEC+ nil )

(defparameter +HYPERSPEC-LIST+ nil )

(defvar *EDITING-FILES* nil )

(defparameter +MAX-UNDO+ 50 )

(defvar *DEFAULT-FOLDER* +source-folder+ )

(defvar +BK-DIR+ (concatenate 'string +source-folder+ "backups/" ))

(defvar +HYPERSPEC-FILE+ (concatenate 'string +source-folder+ "public/X_AllSym.htm" ))

(defparameter +LISPWORKS-HYPERSPEC-URL+ "http://www.lispworks.com/documentation/HyperSpec/" )

(defparameter +EVENT-FUN+ "roxEvent" )

(defparameter +AUTO-SAVE-COUNTER+ 0 )

(defparameter +AUTO-SAVE-LIMIT+ 15 )

(defparameter *HINTS-HISTORY* nil )

(defparameter +MAX-HINTS+ 25 )

(defclass pretty-list (xml-node )((tag :accessor xml-tag :initform +LIST+ )))

(defgeneric pretty-list-p (obj ))

(defmethod pretty-list-p (obj )nil )

(defmethod pretty-list-p ((obj pretty-list ))t )

(defclass pretty-atom (xml-node )((tag :accessor xml-tag :initform +ATOM+ )))

(defgeneric pretty-atom-p (obj ))

(defmethod pretty-atom-p (obj )nil )

(defmethod pretty-atom-p ((obj pretty-atom ))t )

(defclass pretty-descriptor (xml-node )((tag :accessor xml-tag :initform +ATOM+ )))

(defun pretty-descriptor-p (obj )(string= (class-name (class-of obj ))"PRETTY-DESCRIPTOR" ))

(defun prepare-to-display2 (svg )(make-instance 'xml-node :tag "dig" :children (list (make-instance 'xml-node :tag "setchildren" :attributes (list (cons "setid" "idlispide" )):children (list svg ))(make-instance 'xml-node :tag "eval" :children (list "displayEpilogue()" )))))

(defun make-svg (xml )"takes the xml and make a list of elements inside svg tag" (if (pretty-atom-p xml )(let* ((str (xml-value xml ))(splitted (string-split str #\Newline )))(if (= 1 (length splitted ))(list xml )(let ((counter -1 )(y (read-from-string (xml-attribute-get xml :y )))(x (xml-attribute-get xml :x )))(list (make-instance 'pretty-atom :attributes (xml-attributes xml ):children (mapcar #'(lambda (span )(make-instance 'xml-node :tag "tspan" :attributes (list (cons :x x )(cons :y (format nil "~A" (+ y (* (incf counter )(+ +TEXT-HEIGHT+ +MARGIN-Y+ )))))):children (list span )))splitted ))))))(let ((newlist (list (make-instance 'pretty-list :attributes (xml-attributes xml )))))(dolist (child (xml-children xml )newlist )(setf newlist (append (make-svg child )newlist ))))))

(defgeneric xml-to-code-string (xml ))

(defmethod xml-to-code-string ((xml pretty-list ))(let ((children-code (mapcar #'xml-to-code-string (xml-children xml ))))(concatenate 'string (xml-attribute-get xml +LIST-TYPE+ )"(" (apply #'concatenate (push 'string children-code ))")" )))

(defmethod xml-to-code-string ((xml pretty-atom ))(let* ((val (xml-value xml ))(newline (or (and (> (length val )0 )(eq #\; (char val 0 ))(coerce '(#\Newline )'string ))" " )))(concatenate 'string val newline )))

(defgeneric rox-node-to-pretty-node (rox-node &optional parent ))

(defmethod rox-node-to-pretty-node ((rox-node xml-node )&optional parent )(let* ((tag (xml-tag rox-node ))(pretty-node (make-instance (if (string-equal tag +LIST+ )'pretty-list 'pretty-atom ))))(setf (xml-parent pretty-node )parent (xml-attributes pretty-node )(xml-attributes rox-node )(xml-properties pretty-node )(xml-properties rox-node )(xml-children pretty-node )(mapcar #'(lambda (child )(rox-node-to-pretty-node child pretty-node ))(xml-children rox-node )))pretty-node ))

(defmethod rox-node-to-pretty-node (string-node &optional parent );; TODO use declare ignore
(format nil "~A" (or string-node parent )))

(defgeneric tamanho-pretty (node ))

(defgeneric posicao-pretty (node ))

(defgeneric pretty-layout (node ))

(defmethod tamanho-pretty ((node xml-node ))(let ((tam (xml-property-get node :size )))(if tam tam (let ((posdim (list (list 0 0 0 0 )))(margins (if (pretty-atom-p node )(list 0 0 )(list +MARGIN-X2+ +MARGIN-Y2+ ))))(if (and (pretty-list-p node )(null (xml-children node )))(add-as-vectors (tamanho-pretty " " )margins )(dolist (child (xml-children node )(add-as-vectors (farthest posdim )margins ))(setf posdim (cons (append (tamanho-pretty child )(posicao-pretty child ))posdim ))))))))

(defmethod tamanho-pretty (string-node )(let ((newlines (count #\Newline string-node )))(if (= newlines 0 )(list (* +FACTOR-LEN+ (length string-node ))+TEXT-HEIGHT+ )(list (* +FACTOR-LEN+ (apply #'max (mapcar #'length (string-split string-node #\Newline ))))(+ +TEXT-HEIGHT+ (* (+ +TEXT-HEIGHT+ +MARGIN-Y+ )newlines ))))))

(defmethod posicao-pretty ((xml xml-node ))(let ((pos (xml-property-get xml :position )))(if pos pos (let ((parent (xml-parent xml ))(list-type-length (* +FACTOR-LEN+ (length (xml-attribute-get xml :listtype )))))(if (null parent )(list (+ list-type-length +MARGIN-X+ )+FACTOR-H+ )(let* ((children (xml-children parent ))(inib (pretty-layout parent ))(idx (position xml children )))(when (> list-type-length 0 )(incf list-type-length +MARGIN-LIST-DESC+ ))(if (= idx 0 )(list (+ list-type-length +MARGIN-X+ )+MARGIN-Y+ )(progn (add-as-vectors (mul-as-vectors inib (add-as-vectors (posicao-pretty (nth (- idx 1 )children ))(tamanho-pretty (nth (- idx 1 )children ))))(list +MARGIN-X+ +MARGIN-Y+ )(list list-type-length 0 ))))))))))

(defmethod posicao-pretty (string-node )(list 0 0 ))

(defun only-pretty-atoms (node )(let ((ret t )(list-of-xml (xml-children node )))(dolist (child list-of-xml )(setf ret (and ret (pretty-atom-p child ))))ret ))

(defun only-pretty-lists (node )(let ((ret t )(list-of-xml (xml-children node )))(dolist (child list-of-xml )(setf ret (and ret (pretty-list-p child ))))ret ))

(defmethod pretty-layout ((node xml-node ))(let ((v '(0 1 ))(h '(1 0 ))(child (car (xml-children node )))(cur-lay (xml-property-get node :layout )))(if (pretty-atom-p child )(setf child (car (xml-children child )))(setf child nil ))(cond (cur-lay cur-lay )((find child '(:let :let* :progn :cond :dolist :dotimes :block :when :eval-when :unless :if :lambda :mapcar :maplist :map :reduce :loop :or :and :do :labels :flet :flet* ):test #'string-equal )v );; comments as the first element inside the list make it vertical
((and (> (length child )0 )(string= ";" (subseq child 0 1 )))v )((only-pretty-lists node )v )((and (only-pretty-atoms node )(< (length (xml-children node ))10 ))h )((> (mismatch child "with'" :test #'string-equal )3 )v )((< (length (xml-children node ))4 )h )(t v ))))

(defgeneric remove-all-positioning-properties (xml ))

(defmethod remove-all-positioning-properties (a-string ))

(defmethod remove-all-positioning-properties ((xml xml-node ))(dolist (par '(:position :size :layout )nil )(xml-property-set xml par nil ))(mapcar #'remove-all-positioning-properties (xml-children xml ))xml )

(defgeneric set-all-positioning-properties (xml ))

(defmethod set-all-positioning-properties (xml ))

(defmethod set-all-positioning-properties ((xml xml-node ))(mapcar #'set-all-positioning-properties (xml-children xml ))(xml-property-set xml :layout (pretty-layout xml ))(xml-property-set xml :size (tamanho-pretty xml ))(xml-property-set xml :position (posicao-pretty xml )))

(defgeneric set-predefined-layout (xml ))

(defmethod set-predefined-layout (str ))

(defmethod set-predefined-layout ((xml pretty-list ))(if (xml-attribute-get xml :layout )(xml-property-set xml :layout (read-from-string (xml-attribute-get xml :layout ))))(mapcar #'set-predefined-layout (xml-children xml )))

(defun set-events-helper (xml )(let ((roxid (xml-attribute-get xml :roxid )))(dolist (evt (list (list "onclick" "dblclick" ))nil )(xml-attribute-set xml (first evt )(concatenate 'string +EVENT-FUN+ "('" roxid "','" (second evt )"')" )))))

(defgeneric set-events (xml ))

(defmethod set-events (obj )(and (consp obj )(mapcar #'set-events obj )))

(defmethod set-events ((xml pretty-list ))(set-events-helper xml )(mapcar #'set-events (xml-children xml )))

(defmethod set-events ((xml pretty-atom ))(set-events-helper xml ))

(defgeneric set-pretty-dimensions (xml ))

(defgeneric set-absolute-pretty-dimensions (string-node ))

(defun get-hyperspec ()(unless +HYPERSPEC+ (let ((hyper (make-hash-table :test #'equal ))(xml (xml-parse-from-string (read-file +HYPERSPEC-FILE+ )))(term ))(setf +HYPERSPEC-LIST+ nil )(dolist (item (xml-children xml ))(setf term (format-form (car (xml-children item ))))(setf (gethash term hyper )(xml-attribute-get item :href ))(push term +HYPERSPEC-LIST+ ))(setf +HYPERSPEC-LIST+ (reverse +HYPERSPEC-LIST+ ))(setf +HYPERSPEC+ hyper )))+HYPERSPEC+ )

(defgeneric set-atom-classes (obj ))

(defmethod set-atom-classes (obj )(and (consp obj )(mapcar #'set-atom-classes obj )))

(defmethod set-atom-classes ((xml pretty-list ))(mapcar #'set-atom-classes (xml-children xml )))

(defmethod set-atom-classes ((xml pretty-atom ))(let* ((txt (car (xml-children xml )))(txt2 (string-trim "#'" txt ))(hyper (get-hyperspec ))(cur-classes (format nil "~A" (xml-attribute-get xml :class )))(new-classes nil ))(if (and (> (length txt )0 )(char= #\: (char txt 0 )))(push "keyword" new-classes ))(if (ignore-errors (+ (read-from-string txt )))(push "number" new-classes ))(if (and (> (length txt )0 )(char= #\" (char txt 0 )))(push "string" new-classes ))(if (string-equal txt "nil" )(push "nil" new-classes ))(if (gethash (format-form txt2 )hyper )(push "hyperspec" new-classes ))(if (or (and (> (length txt )0 )(char= (char txt 0 )#\; ))(and (> (length txt )1 )(char= (char txt 0 )#\# )(char= (char txt 1 )#\| )))(push "comment" new-classes ))(dolist (cls new-classes )(setf cur-classes (string-replace (concatenate 'string (string-replace cur-classes cls "" )" " cls )"  " " " )))(xml-attribute-set xml :class (string-trim " " cur-classes ))))

(defgeneric process-dimensioning (xml ))

(defmethod process-dimensioning (a-string ))

(defmethod process-dimensioning ((xml xml-node ))(remove-all-positioning-properties xml )(set-predefined-layout xml )(set-all-positioning-properties xml )(setf xml (set-pretty-dimensions xml ))(setf xml (set-absolute-pretty-dimensions xml ))(set-atom-classes xml )(set-events xml )xml )

(defgeneric set-id-all-family (str ))

(defmethod set-id-all-family (str ))

(defmethod set-id-all-family ((xml xml-node ))(xml-id-set xml :roxid t )(mapcar #'set-id-all-family (xml-children xml )))

(defmethod set-pretty-dimensions (string-node ))

(defmethod set-pretty-dimensions ((xml xml-node ))(let ((pos (posicao-pretty xml ))(tam (tamanho-pretty xml ))(is-atom (pretty-atom-p xml ))(is-list (pretty-list-p xml )))(xml-id-set xml :roxid nil )(when (xml-parent xml )(xml-attribute-set xml :parent (xml-attribute-get (xml-parent xml ):roxid )))(xml-attribute-set xml :x (format nil "~A" (first pos )))(xml-attribute-set xml :y (format nil "~A" (second pos )))(xml-attribute-set xml :width (format nil "~A" (first tam )))(xml-attribute-set xml :height (format nil "~A" (second tam )))(when is-list (xml-attribute-set xml :rx +RX+ )(xml-attribute-set xml :ry +RY+ ))(when is-atom (xml-attribute-set xml :textlength (format nil "~A" (first tam ))))(mapcar #'set-pretty-dimensions (xml-children xml ))xml ))

(defmethod set-absolute-pretty-dimensions (string-node ))

(defmethod set-absolute-pretty-dimensions ((xml xml-node ))(let ((parent-x 0 )(parent-y 0 )(parent (xml-parent xml ))(x (read-from-string (xml-attribute-get xml :x )))(y (read-from-string (xml-attribute-get xml :y ))))(when parent (setf parent-x (read-from-string (xml-attribute-get parent :x ))parent-y (read-from-string (xml-attribute-get parent :y ))))(setf x (+ x parent-x )y (+ y parent-y ))(when (pretty-atom-p xml )(setf y (+ y +TEXT-HEIGHT+ )))(xml-attribute-set xml :x (format nil "~A" x ))(xml-attribute-set xml :y (format nil "~A" y ))(mapcar #'set-absolute-pretty-dimensions (xml-children xml ))xml ))

#| ide commands |# 

(defun make-top-menu ()(string-to-xml "<appendchildren tgt='topmenu'><b>ola</b></appendchildren>" ))

(defclass pfile-manager ()((name :accessor pfile-name :initarg :name )(path :accessor pfile-path :initarg :path )(id :accessor pfile-id :initform (auto-id "file" ))(xml :accessor pfile-xml :initarg :xml )(history :accessor pfile-history :initform nil )(pointer :accessor pfile-pointer #| pointer to current hitory |# :initform 0 )))

(defgeneric pfile-manager-p (obj ))

(defmethod pfile-manager-p ((obj pfile-manager ))t )

(defmethod pfile-manager-p (obj ))

(defun pfile-update-history (pfile )(let ((copy nil )(history (pfile-history pfile )))(setf history (subseq history (pfile-pointer pfile )))(setf (pfile-pointer pfile )0 )(setf copy (xml-copy (pfile-xml pfile )))(push copy history )(when (> (length history )+MAX-UNDO+ )(setf history (subseq history 0 +MAX-UNDO+ )))(setf (pfile-history pfile )history )))

(defun pfile-undo-redo (pfile &optional (direction :undo ))(let ((pointer (pfile-pointer pfile ))(history (pfile-history pfile )))(if (eql direction :undo )(incf pointer )(decf pointer ))(when (> pointer (- (length history )1 ))(setf pointer (- (length history )1 )))(when (< pointer 0 )(setf pointer 0 ))(setf (pfile-pointer pfile )pointer )(setf (pfile-xml pfile )(copy-list (nth pointer history )))))

(defun pfile-save (pfile )(let ((nodes (pfile-xml pfile ))(path (pfile-path pfile ))(str nil ))(dolist (node nodes nil )(setf str (concatenate 'string str (coerce '(#\Newline #\Newline )'string )(xml-to-code-string node ))))(write-to-file path str )))

(defun pfile-backup (pfile )(let* ((path (pfile-path pfile ))(name (pfile-name pfile ))(bk-path (concatenate 'string +BK-DIR+ (format nil "~A_" (get-universal-time ))name )))(when (probe-file path )(write-to-file bk-path (read-file path )))))

(defun pfile-auto-save (pfile )(when (> (incf +AUTO-SAVE-COUNTER+ )+AUTO-SAVE-LIMIT+ )(setf +AUTO-SAVE-COUNTER+ 0 )(let* ((nodes (pfile-xml pfile ))(name (pfile-name pfile ))(str nil )(bk-path (concatenate 'string +BK-DIR+ (format nil "~A_" (get-universal-time ))name )))(dolist (node nodes nil )(setf str (concatenate 'string str (coerce '(#\Newline #\Newline )'string )(xml-to-code-string node ))))(write-to-file bk-path str ))))

(defun pfile-open (filepath )(block nil (let* ((edit-nodes nil )(code-string (read-file filepath ))(rox-xml ))(setf code-string (concatenate 'string (list #\( #\Newline )code-string (list #\Newline #\) )))(setf rox-xml (ignore-errors (code-string-to-xml-2 code-string )))(when (null rox-xml )(return nil ))(dolist (svg (xml-children rox-xml )nil )(setf (xml-parent svg )nil )(setf svg (rox-node-to-pretty-node svg ))(setf svg (process-dimensioning svg ))(push svg edit-nodes ))(reverse edit-nodes ))))

(defun insert-list-type (svg-list )(let ((ret-svg-list nil )(size 0 )(xml-type-desc nil )(type-text nil ))(dolist (element svg-list (reverse ret-svg-list ))(push element ret-svg-list )(setf type-text (xml-attribute-get element +LIST-TYPE+ ))(setf size (* +FACTOR-LEN+ (length type-text )))(when (and (> size 0 )(pretty-list-p element ))(setf xml-type-desc (make-instance 'pretty-descriptor :children (list type-text )))(xml-id-set xml-type-desc :roxid )(xml-attribute-set xml-type-desc :textLength (car (tamanho-pretty type-text )))(xml-attribute-set xml-type-desc :class "listdescriptor" )(xml-attribute-set xml-type-desc :type "listdescriptor" )(xml-attribute-set xml-type-desc :tgt (xml-attribute-get element :roxid ))(xml-attribute-set xml-type-desc :y (format nil "~A" (+ +TEXT-HEIGHT+ +MARGIN-Y+ (read-from-string (xml-attribute-get element :y )))))(xml-attribute-set xml-type-desc :x (format nil "~A" (- (read-from-string (xml-attribute-get element :x ))(+ size +MARGIN-LIST-DESC+ ))))(push xml-type-desc ret-svg-list )))))

(defun make-svg2 (svg )"surround the element with a svg node and a basis rect" (let ((width (xml-attribute-get svg :width ))(height (xml-attribute-get svg :height ))(x (xml-attribute-get svg :x ))(y (xml-attribute-get svg :y ))(mainrect nil ))(if (pretty-list-p svg )(setf mainrect (list (make-instance 'xml-node :tag "rect" :attributes (list (cons :x x )(cons :y y )(cons :width width )(cons :height height )(cons :rx +RX+ )(cons :ry +RY+ )(cons :class "mainrect" ))))))(setf width (+ +MARGIN-X+ (read-from-string width )(read-from-string x )))(setf height (+ 1 +MARGIN-Y+ (read-from-string height )(read-from-string y )))(setf svg (reverse (make-svg svg )))(setf svg (insert-list-type svg ))(setf svg (make-instance 'xml-node :tag "svg" :children (append mainrect svg )))(xml-attribute-set svg :xmlns "http://www.w3.org/2000/svg" )(xml-attribute-set svg :version "1.1" )(xml-attribute-set svg :x "0" )(xml-attribute-set svg :y "0" )(xml-attribute-set svg :height (format nil "~A" height ))(xml-attribute-set svg :width (format nil "~A" width )))svg )

(defun prettyfile (xmlreq )"takes a file of lisp code and transforms it into a 
xml-node structure with editing capabilities and 
which can be also saved as a editing state, for undo commands" (let* ((button (car (xml-children xmlreq )))(filepath (xml-attribute-get button :newvalue ))(filename (file-namestring (pathname filepath )))nodes-list svglist roxid pfile )(setf pfile (find filepath *EDITING-FILES* :key #'pfile-path :test #'string= ))(unless pfile (setf nodes-list (pfile-open filepath ))(when nodes-list (setf pfile (make-instance 'pfile-manager :name filename :path filepath :xml nodes-list ))(push pfile *EDITING-FILES* )(pfile-update-history pfile )))(when pfile (setf nodes-list (pfile-xml pfile )))(dolist (svg nodes-list nil )(setf roxid (xml-attribute-get svg :roxid ))(setf svg (make-svg2 svg ))(setf svg (make-instance 'xml-node :tag +TAG-CONT+ :attributes (list (cons "id" (concatenate 'string "p" roxid ))(cons :class "pcodecontainer" )):children (list svg )))(push svg svglist ))(setf svglist (make-instance 'xml-node :tag "div" :children (reverse svglist ):attributes (list (cons "id" "nodescontainer" ))))(setf svglist (prepare-to-display2 svglist ))svglist ))

#| "takes a file of lisp code and transforms it into a xml-node structure with editing capabilities and which can be also saved as a editing state, for undo commands" |# 

(defun roxid-pfile (roxid )(block nil (dolist (file *EDITING-FILES* nil )(when (find-by-attribute (pfile-xml file ):roxid roxid )(return file )))))

(defun parse-pretty-atom-2 (xml )(let ((str (concatenate 'string (list #\( #\Newline )(concatenate 'string '(#\; )(xml-value xml ))(list #\Newline #\) ))))(setf str (ignore-errors (code-string-to-xml-2 str )))(if (and (xml-node-p str )(setf str (rox-node-to-pretty-node str ))(pretty-list-p str ))(xml-children str )(list xml ))))

(defgeneric parse-pretty-atom (xml ))

(defmethod parse-pretty-atom (xml )(list xml ))

(defmethod parse-pretty-atom ((xml pretty-atom ))(let ((str (concatenate 'string (list #\( #\Newline )(xml-value xml )(list #\Newline #\) ))))(setf str (ignore-errors (code-string-to-xml-2 str )))(if (and (xml-node-p str )(setf str (rox-node-to-pretty-node str ))(pretty-list-p str ))(xml-children str )(parse-pretty-atom-2 xml ))))

#| se falhou o 1 usa o segundo, comentando |# 

(;; TODO search dependencies and remove
defun parse-pretty-atom-3 (xml )(let ((str (concatenate 'string (list #\( #\Newline )(concatenate 'string '(#\; )(xml-value xml ))(list #\Newline #\) ))))(setf str (ignore-errors (code-string-to-xml str )))(if (and (s-xml:xml-element-p str )(setf str (rox-node-to-pretty-node (s-xml-to-rox-xml str )))(pretty-list-p str ))(xml-children str )(list xml ))))

(defun cur-paths-options (path )(let* ((cur-files (sort (mapcar #'pfile-path *EDITING-FILES* )#'string< ))(cur-pathname (pathname path ))(cur-folder (directory-namestring cur-pathname ))(hierarchy (folder-hierarchy cur-folder '("lisp" "cl" "pretty" "el" ))))(setf cur-files (append '("------OPENED FILES------" )cur-files '("----FOLDER STRUCTURE----" )hierarchy ))(make-instance 'xml-node :tag "select" :attributes (list (cons "id" "fileslist" )):children (mapcar #'(lambda (f )(make-instance 'xml-node :tag "option" :attributes (if (string= f path )(list (cons "class" "openfile" )(cons "selected" "selected" ))(list (cons "class" "openfile" ))):children (list f )))cur-files ))))

(defgeneric set-editing-class (str ))

(defmethod set-editing-class (str )(if (consp str )(mapcar #'set-editing-class str )))

(defmethod set-editing-class ((xml pretty-atom ))(let ((parent (xml-parent xml ))(cls (xml-attribute-get xml :class ))(bkcls (xml-attribute-get xml :bkclass )))(unless bkcls (xml-attribute-set xml :bkclass cls )(xml-attribute-set xml :class "atomclick2" )(when parent (setf bkcls (xml-attribute-get parent :bkclass ))(unless bkcls (setf cls (xml-attribute-get parent :class ))(xml-attribute-set parent :bkclass cls )(xml-attribute-set parent :class "atomparentclick2" ))))))

(defmethod set-editing-class ((xml pretty-list ))(let ((cls (xml-attribute-get xml :class ))(bkcls (xml-attribute-get xml :bkclass )))(unless bkcls (xml-attribute-set xml :bkclass cls )(xml-attribute-set xml :class "listclick2" ))))

(defgeneric remove-editing-class (str ))

(defmethod remove-editing-class (str )(if (consp str )(mapcar #'remove-editing-class str )))

(defmethod remove-editing-class ((xml pretty-atom ))(let ((bkcls (xml-attribute-get xml :bkclass )))(when bkcls (xml-attribute-set xml :class bkcls )(xml-attribute-set xml :bkclass nil ))))

(defmethod remove-editing-class ((xml pretty-list ))(let ((bkcls (xml-attribute-get xml :bkclass )))(when bkcls (xml-attribute-set xml :class bkcls )(xml-attribute-set xml :bkclass nil ))(remove-editing-class (xml-children xml ))))

(defun loadhyperspec (xml )(let ((term (if (xml-node-p xml )(xml-value (car (xml-children xml )))xml ))(href nil )(jscode nil )(ret nil ))(setf href (gethash (format-form term )(get-hyperspec )))(if href (progn (setf href (string-replace href "../" +LISPWORKS-HYPERSPEC-URL+ ))(setf jscode (concatenate 'string "window.open(\"" href "\",\"menubar=yes,toolbar=yes\");" ))(setf jscode (url-encode jscode ))(setf ret (make-instance 'xml-node :tag "decodeandeval" :children (list jscode ))))(setf ret (make-instance 'xml-node :tag "alert" :children (list term "not found in hyperspec" ))))ret ))
