

(;; Nuno Rocha
;; Berlin Lispers Meetup, Jan 24, 2012 @ co.up 
;; A suggestion for parentheses representation in a Common Lisp Editor
(pretty-LISP ))

(:FEATURES )

;;; FILE OPERATIONS - on top right corner


'((:browse ;lists the folder contents and hierarchy
)(:open ;parses and opens the file for editing
)(:load ;apply the load function to the file (on disk)
)(:new ;creates a new file
)(:save ;converts the current file to textual code and save it
)(:saveas ;like :save, with another name
)(:close ;close the current file (without prompt or save)
)(:undo ;C z - undo editing commands
)(:redo ;C y - redo editing command
))

;;;


;;; EDITING COMMANDS - on bottom, left corner


;;; Keyboard shortcuts for editing commands may interfere with browser shortcuts


;;; To diminish this efect the commands are in the form:


;;; C x, meaning Press CTRL, then release CTRL, and press x


'(;; buttons near to the editing text area:
(:show ;navigates to the current element being edited
)(:close ;closes the editing text area and editing commands menu
))

;;; commands in the left menu


'(;; ESC or C 0
;; closes the editing text area without any modifications to the element
(0 - Cancel ))

'(;; C 1 or C-Enter
;; updates the element with the changes made.
;; - If an atom/string/comment, the inner text
;; - If a list, the list type
(1 - Update ))

'(;; C 2
;; evals the selected element
;; Displays (format nil "~A" result) in the top right corner 
(2 - Execute ))

'(;; C x
;; removes the element and keep it
;; in memory for further use with :paste
(x - Cut );; try it:
(1 3 ))

'(;; C c 
;; copy element to memory
(c - Copy );; try it:
(a b c ))

'(;; C v
;; paste the element in memory
;; replacing the selected element
(v - Paste );; try it:
(:x :y :z ))

'(;; C f
;; delete the selected element
(f - Delete );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C a
;; insert a closed comment #||# before the element
;; the inserted comment can be later edited 
;; and transformed into arbitray elements
(a - Before );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C s
;; - if the selected element is a list inserts a closed comment #||# to the beginning of the list
;; - if the selected element is an atom, replaces the atom with a closed comment
(s - Inside );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C d
;; inserts a #||# after the element
(d - After );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C q
;; if the selected element is a list
;; the outer parentheses are removed
;; if the selected element is a comment,
;; the comment marks are removed
;; in other cases the command is ignored
(q - Free );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C w
;; surround the element with a closed parentheses
(w - Surround );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C e
;; transforms the element into textual LISP code
;; and comment it
(e - Comment );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C t
;; switches the layout {vertical, horizontal}
(t - Transpose );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

'(;; C k, C l
;; reduces the visible height of the top level element(s)
(k - Collapse )(l - CollapseAll );; try it anywhere with Expand and ExpandAll
)

'(;; C o, C p
;; restores the visible height of the top level element(s)
(o - Expand )(p - ExpandAll );; try it anywhere with Collapse and CollapseAll
)

'(;; C b, C h, C m, C n
;; navigate trought elements
;; also possible to use Ctrl followed by arrow keys,
;; although interferes with browser shortcuts
(b - Left )(h - Up )(m - Right )(n - Down );; try it:
(abc (1 2 3 )(c (d ))(:x :y :z )))

;;;


;;; HINT COMMANDS - on bottom, right corner


'((:set ;set the text in the editing text area with the text in the hints text area
)(:get ;get the text from the hints text area to the auxiliar text area
)(:close ;close hints text area and commands
))

'((:complete ;search the elements that textually complete the inserted text
)(:similar ;list elements with similar text
)(:search ; list elements matching the text, in the current file
)(:def ;looks for the definition (defun, defparameter,...), if any
)(:suggest ;gives a reduced hint about usage
)(:examples ;search in the current file for usage examples
)(:history ;history of searched text with this group of funcionalities
)(:hyperspec ;search for the term in the hyperspecification and pop ups it
)(:case ;switch the case {upper, lower}
))