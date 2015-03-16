;;; pretty-LISP - Common LISP IDE


;;; © Nuno Rocha 2011


(in-package :pretty-lisp )

(defun octets-to-string-utf-8 (octets )(if (typep octets 'string )octets (octets-to-string octets :external-format :utf-8 )))

(defun sethash (key value hashtable )(setf (gethash key hashtable )value ))

#| ---- file and folder operations |# 

(defun read-file (path )"Read file to string" (with-open-file (s path )(let* ((len (file-length s ))(data (make-string len )))(read-sequence data s )(values data ))))

(defun write-to-file (filename content )(with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create )(format stream "~A" content ))filename )

(defun list-folder-pathnames (folderpath )"Returns a list of folder contents" (directory (make-pathname :name :wild :type :wild :defaults folderpath )))

(defun pathname-filename (pathname )(concatenate 'string (pathname-name pathname )"." (pathname-type pathname )))

#| use iolib here, to portability |# 

(defun list-folder-filenames (folderpath )(mapcar #'pathname-filename (list-folder-pathnames folderpath )))

(defun folder-hierarchy (folder-path &optional extensions )(let* ((folder-path-name (pathname folder-path ))(directory-list (reverse (pathname-directory folder-path-name )))hierarchy auxiliar )(setf hierarchy (maplist #'(lambda (dir )(make-pathname :directory (reverse dir )))directory-list ))(setf hierarchy (reverse hierarchy ))(setf auxiliar (list-folder-pathnames folder-path ))(setf auxiliar (remove-if #'null auxiliar :key #'(lambda (f )(or (null extensions )(find (pathname-type f )extensions :test #'string-equal )(string= (namestring f )(directory-namestring f ))))))(setf hierarchy (append hierarchy auxiliar ))(mapcar #'namestring hierarchy )))

(defun is-directory (folder-path )(let ((pname (pathname folder-path )))(string= (namestring pname )(directory-namestring pname ))))

#| "http://www.tek-tips.com/viewthread.cfm?qid=1184743&page=2" |# 

(defun string-replace (str1 sub1 sub2 )(let ((str1 (string str1 ))(str2 "" )(sub1 (string sub1 ))(sub2 (string sub2 ))(index1 0 ))(loop (if (string-equal str1 sub1 :start1 index1 :end1 (min (length str1 )(+ index1 (length sub1 ))))(progn (setq str2 (concatenate 'string str2 sub2 ))(incf index1 (length sub1 )))(progn (setq str2 (concatenate 'string str2 (subseq str1 index1 (1+ index1 ))))(incf index1 )))(unless (< index1 (length str1 ))(return str2 )))))

(defun count-substring (substr str )(do ((sum -1 (1+ sum ))(pos -1 (search substr str :start2 (1+ pos ))))((null pos )sum )))

(defun string-split (string &optional (div-char #\Space ))"Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them." (loop for i = 0 then (1+ j )as j = (position div-char string :start i )collect (subseq string i j )while j ))

#| ----------- math --------------- |# 

(defun levenshtein (s01 s02 &key (ignore-case nil ))(let* ((s1 (if ignore-case (string-downcase s01 )s01 ))(s2 (if ignore-case (string-downcase s02 )s02 ))(width (1+ (length s1 )))(height (1+ (length s2 )))(d (make-array (list height width ))))(dotimes (x width )(setf (aref d 0 x )x ))(dotimes (y height )(setf (aref d y 0 )y ))(dotimes (x (length s1 ))(dotimes (y (length s2 ))(setf (aref d (1+ y )(1+ x ))(min (1+ (aref d y (1+ x )))(1+ (aref d (1+ y )x ))(+ (aref d y x )(if (char= (aref s1 x )(aref s2 y ))0 1 ))))))(aref d (1- height )(1- width ))))

(defun levenshtein-ratio (to-compare reference )(let ((len (length reference )))(if (= len 0 )(length to-compare )(/ (levenshtein to-compare reference )len ))))

(defun add-as-vectors (lst &rest others )(let ((res (copy-list lst )))(dolist (lstn others res )(loop for i from 0 to (- (length lst )1 )do (setf (nth i res )(+ (nth i res )(nth i lstn )))))))

(defun mul-as-vectors (lst &rest others )(let ((res (copy-list lst )))(dolist (lstn others res )(loop for i from 0 to (- (length lst )1 )do (setf (nth i res )(* (nth i res )(nth i lstn )))))))

(defun mul-list (factor lst )(mapcar #'(lambda (x )(* factor x ))lst ))

(defun farthest (desc )"return the farthest point (x+w y+h) from a list of (x y w h)" (let ((x-far 0 )(y-far 0 ))(dolist (item desc (list x-far y-far ))(setf x-far (max x-far (+ (first item )(third item ))))(setf y-far (max y-far (+ (second item )(fourth item )))))))

(defun truncate-list (lst n )(if (<= (length lst )n )lst (subseq lst 0 n )))

#| time |# 

(defun get-time-for-path ()(let (ret (time-list (multiple-value-list (get-decoded-time ))))(setf time-list (subseq (reverse time-list )3 ))(dolist (num time-list (string-trim "- " ret ))(setf ret (concatenate 'string ret "-" (format nil "~A" num ))))))
