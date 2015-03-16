

(;; Nuno Rocha
;; Berlin Lispers Meetup, Jan 24, 2012 @ co.up 
;; A suggestion for parentheses representation in a Common Lisp Editor
(pretty-LISP ))

(:INTRO )

(Editor for Common LISP code )

(Converts closed parentheses into SVG rect elements )

(User interface with Chrome, Javascript in background )

(Server side on SBCL with external packages Hunchentoot, S-XML )

;;; Examples (free comments):


;(:a '(1 2 ))


;'(a b c (d e f )(g h (i j )))


;(defun hello (&optional (name :world ))(format nil "Hello, ~A!" ))


;(mapcar #'(lambda (a )(if (< a 0 )"-" "+" ))'(-3 -2 -1 0 1 2 3 4 ))


;(()(((()))((())))(((()))((()))))


;(k (a )((c (()))(((b ))d ))(y ((x ()))y ((()))))
