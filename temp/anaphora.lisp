

( quicklisp:quickload "anaphora" )

( in-package :anaphora )

( aif ( + 1 2 ) ( format nil "the results is ~A" it ) )

( acond ( t it ) ( ( + 3 4 ) it ) )