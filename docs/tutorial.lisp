

;;; pretty-LISP Editor (beta) 

;;; Copyright (c) 2012, Nuno Rocha.  All rights reserved. 

( ( ( ) ) ( pretty-lisp ) ( ) ( ( :online Demo ) and ( :interactive Tutorial ) ) ( ( ( ) ) )
 '( ( ) introduction examples features credits ) )

;;; 

;;; Notes: 

;;; You can change the background color by clicking on the READY signal, on top right corner. 

;;; Some features are not available for the online demo 

;;; 

INTRODUCTION

;;; 

;;; pretty-LISP is an editor for Common LISP and can be used for basic 

;;; programming. The main characteristic is the conversion of parenthesis to 

;;; SVG rect elements. 

;;; The user interface runs in a browser, with Javascript. The server side runs 

;;; with SBCL and Hunchentoot. 

;;; The code can be edited with keyboard and mouse inputs for  copying, 

;;; pasting, undoing, updating; the usual functionalities of a text editor. 

;;; The interaction is done on top of SVG elements,  and the changes are 

;;; reflected requesting the server, where the states of the files being edited are kept. 

;;; It is possible to evaluate code, with some feedback on exceptions. 

;;; 

EXAMPLES

;;; 

;;; Select the following lines and press the 'q - Free' left menu command, 

;;; or use the keyboard shortcut  C q, which stands for Ctrl (release) q: 

;(:a '(1 2 )) 

;'(a b c (d e f )(g h (i j ))) 

;(defun hello (&optional (name :world ))(format nil "Hello, ~A!" )) 

;(mapcar #'(lambda (a )(if (< a 0 )"-" "+" ))'(-3 -2 -1 0 1 2 3 4 )) 

;(()(((()))((())))(((()))((())))) 

;(k (a )((c (()))(((b ))d ))(y ((x ()))y ((())))) 

;;; Elements inside lists have a vertical or horizontal layout 

;;; 

FEATURES

;;; 

;;; FILE OPERATIONS - on top right corner 

( ( :browse ;list contents of the selected folder
           ) ( :open ;parse and open the file for editing
                    ) ( :load ;apply the load function to the file (on disk)
                             ) ( :new ;create a new file
                                     ) ( :save ;convert the current file to textual code and save it
                                              ) ( :saveas ;save with another name
                                                         ) ( :close ;close the current file
                                                                   )
 ( :closeAll ;close all opened Files
             ) ( :saveAll ;save all opened files
                         ) ( :openAll ; open all files in current directory
                                     ) )

;;; 

;;; EDITING COMMANDS - on bottom, left corner 

;;; Keyboard shortcuts for editing commands are in the form 

;;; C x, meaning: press CTRL, then release CTRL and press x 

( ;; buttons near to the editing text area:
   ( :memo ;show recent text introduced in the text area
          ) ( :scroll ;scroll to the current element being edited
                     ) ( :close ;closes the editing text area and editing commands menu
                               ) )

;;; commands in the left menu 

(   ;; ESC or C 0
    ;; closes the editing text area without any modifications to the element
    ;; and set focus again in the same element
     ( 0 - Cancel ) )

(    ;; C 1 or C-Enter
     ;; updates the element with the changes made.
     ;; - If an atom/string/comment, the inner text
     ;; - If a list/array the "list type"
      ( 1 - Update )   ;;
                    ;; try it! Select a token and make some modifications:
                     ( :keyword "string" #'( lambda ( c )  ;;nil
                                                     nil ) ) )

(   ;; C 2
    ;; evals the selected element
    ;; Displays (format nil "~A" result) in the top right corner
     ( 2 - Execute )  ;;
                   ;; This feature is not available for the online demo
                    )

(   ;; C x
    ;; removes the element and keep it
    ;; in memory to Paste afterwards
     ( x - Cut )   ;;
                ;; try it:
                 ( 1 2 3 ) )

(  ;; C c
   ;; copy element to memory
    ( c - Copy )   ;;
                ;; try it:
                 ( a b c ) )

(   ;; C v
    ;; paste the element in memory
    ;; replacing the selected element
     ( v - Paste )   ;;
                  ;; try it:
                   ( :x :y :z ) )

(  ;; C f
   ;; delete the selected element
    ( f - Delete )   ;;
                  ;; try it:
                   ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(    ;; C a
     ;; insert a closed comment #||# before the selected element
     ;; the inserted comment can be immediately edited
     ;; and transformed into arbitray elements
      ( a - Before )   ;;
                    ;; try it:
                     ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(   ;; C s
    ;; - if the selected element is a list inserts a closed comment #||# to the beginning of the list
    ;; - if the selected element is an atom, replaces the atom with a closed comment
     ( s - Inside )   ;;
                   ;; try it:
                    ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(  ;; C d
   ;; inserts a #||# after the element
    ( d - After )   ;;
                 ;; try it:
                  ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(      ;; C q
       ;; if the selected element is a list
       ;; the outer parentheses are removed;
       ;; if the selected element is a comment,
       ;; the comment marks are removed
       ;; in other cases the command is ignored
        ( q - Free )   ;;
                    ;; try it:
                     ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(  ;; C w
   ;; surround the element with a closed parentheses
    ( w - Surround )   ;;
                    ;; try it:
                     ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(   ;; C e
    ;; transforms the element into textual LISP code
    ;; and comment it
     ( e - Comment )   ;;
                    ;; try it:
                     ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(  ;; C z, C y
   ;; Undo and Redo recent editing commands
    ( z - Undo ) ( y - Redo )     ;;
                             ;; try it:
                             ;; do some changes (delete, copy, paste...)
                             ;; and then use Undo or Redo
                              ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(  ;; C t
   ;; switches the layout {vertical, horizontal}
    ( t - Transpose )   ;;
                     ;; try it:
                      ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

(  ;; C k, C l
   ;; reduces the visible height of the top level element(s)
    ( k - Collapse ) ( l - CollapseAll )   ;;
                                      ;; try it
                                      ;; anywhere with Expand and ExpandAll
                                       )

(  ;; C o, C p
   ;; restores the visible height of the top level element(s)
    ( o - Expand ) ( p - ExpandAll )   ;;
                                  ;; try it
                                  ;; anywhere with Collapse and CollapseAll
                                   )

(    ;; C b, C h, C m, C n
     ;; navigate trought elements
     ;; also possible to use Ctrl followed by arrow keys,
     ;; although interferes with browser shortcuts
      ( b - Left ) ( h - Up ) ( m - Right ) ( n - Down )  
 ;;
 ;; try it:
  ( abc ( 1 2 3 ) ( c ( d ) ) ( :x :y :z ) ) )

;;; 

;;; HINT COMMANDS - on bottom, right corner 

;;; 

( ( :set ;set the text in the editing text area with the text in the hints text area
        ) ( :get ;get the text from the editing text area and set it in the hints text area
                ) ( :close ;close hints text area and commands
                          ) )

( ( :complete ;search the elements that textually complete the inserted text
             ) ( :similar ;list elements with similar text.
                         ) ( :search ;list elements matching the text, in the current file
                                    ) ( :history ;history of input text
                                                ) ( :hyperspec ;search for the term in the hyperspecification and pops it up
                                                              ) ( :case ;switch the case {upper, lower}
                                                                       ) )

;;; 

CREDITS

;;; 

( ( ( defun pretty-lisp ( )
    "Playing around with parenthesis representation in a Common Lisp Editor" ) )
 ( ( ( ( ( ) ) ( ( ( ( ( ( pretty-lisp ) :editor ) ( ) ( ( ) ) ( :by nrox ) ) ) ) ) ) ) ) ( ( ) )
 ( ( ( ( ) ) ) ) ( ) )