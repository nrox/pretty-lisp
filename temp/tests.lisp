

( defun amb ( &rest args ) ( funcall ( nth ( random ( length args ) ) args ) ) )

( defparameter test-var -1 )

( progn
 ( setf test-var -1 )
 ( amb #'( lambda ( ) ( setf test-var 0 ) ) #'( lambda ( ) ( setf test-var 1 ) )
      #'( lambda ( ) ( setf test-var 2 ) ) ) )

( defmacro ambm ( &rest args ) ( funcall ( nth ( random ( length args ) ) args ) ) )