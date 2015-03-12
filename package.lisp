

;;; pretty-LISP - Common LISP Editor 


;;;   Nuno Rocha 2012 


( in-package :cl-user )


( mapcar #'ql:quickload '( :hunchentoot :s-xml ) )


( defpackage "pretty-lisp"
  ( :nicknames :pretty-lisp )
  ( :use :cl :hunchentoot :s-xml :flexi-streams )
  ( :export :run :down ) )


( in-package :pretty-lisp )


( defvar *ide-port* 4555 )


( defparameter *source-files*
  '( "utils" "xml" "javascript" "parser2" "files" "printer" "transform" "edit"
   "hint" "extension" "requests" "server" )
  "source files for this package, without their .lisp extension,
must be found in the same folder where package.lisp is" )


( defparameter +source-folder+
  ( directory-namestring *load-pathname* )
  "The folder where this file is when loaded" )


( defparameter *error-alerts*
  t
  "Alert errors in browser (t) or normal log to std out (nil)" )


( defparameter *see-logs*
  nil
  "Logs are printed to std out (t) or not printed at all (nil)" )


( defparameter *compile-files*
  nil
  "Files are compiled and then the .fasl loaded (t) or just the .lisp loaded (nil)" )


( defvar *pretty-lisp-acceptor*
  nil
  "The unique instance of the acceptor intended to be used." )


( format t
        "~%~% to a quickstart on port ~A, execute ~%~% ( pretty-lisp:run :port ~A) ~%~%"
        *ide-port* *ide-port* )


( defun down ( )
  ( and *pretty-lisp-acceptor* ( hunchentoot:stop *pretty-lisp-acceptor* ) )
  ( format nil "~%~% server down ~%~% ~A ~%~%"
          ( and *pretty-lisp-acceptor*
               ( hunchentoot:stop *pretty-lisp-acceptor* ) ) ) )


( defun run
       ( &key ( port *ide-port* ) ( alerts *error-alerts* )
        ( compile-first *compile-files* ) )
  ( setf *ide-port* port )
  ( setf *see-logs* ( not ( setf *error-alerts* alerts ) ) )
  ( setf *compile-files* compile-first )
  
  ;; compile and/or load source files
   ( mapcar
   #'( lambda ( filename )
     ( let ( ( file-path ( concatenate 'string +source-folder+ filename ".lisp" ) ) )
       ( if *compile-files* ( load ( namestring ( compile-file file-path ) ) )
           ( load file-path ) ) ) )
   *source-files* )
  ( and *pretty-lisp-acceptor* ( hunchentoot:start *pretty-lisp-acceptor* ) )
  ( format nil
          "~%~% Running on ~%~% http://localhost:~A/ ~%~% Errors logged to ~A ~%~%"
          port ( if alerts "Browser" "REPL" ) ) )
