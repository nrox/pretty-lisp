

( in-package :cl-user )

( defun get-into-library ( library )
  ( in-package :cl-user )
  ( quicklisp:quickload library )
  ( setf *package* ( find-package library ) ) )

( cl-user::get-into-library :anaphora )

( aif ( + 1 2 ) ( format nil "the results is ~A" it ) )

( acond ( t it ) ( ( + 3 4 ) it ) )

;(cl-user::get-into-library :archive ) 

( cl-user::get-into-library :arnesi )

( cl-user::get-into-library :array-operations )

( cl-user::get-into-library :asn.1 )

( cl-user::get-into-library :bk-tree )

( cl-user::get-into-library :chtml-matcher )

( cl-user::get-into-library :black-tie )