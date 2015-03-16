

;;; pretty-LISP Editor (beta) 

#|
 Copyright (c) 2012, Nuno Rocha.  All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials
     provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|# 

( in-package :pretty-lisp )

( defvar *multi-user-demo*
  nil
  "To consider several users, for this package demo." )

( defvar *EDITING-FILES* nil "Files currently being edited are kept here." )

( defvar *hints-history* nil "Records the words used in the hints text area." )

( defvar *ID-COUNTER*
  0
  "This will be incremented to assign single id values to elements." )

( defvar *acceptor-message-stream*
  ( make-string-output-stream )
  "Messages will be logged here" )

( defvar *sessions-timeout*
  ( if *multi-user-demo* ( * 15 60 ) ( * 365 24 3600 ) )
  "Session timeout" )

( defvar *backup-files*
  ( not *multi-user-demo* )
  "If the original file is backed up before saving." )

( defvar *autosave-files*
  ( not *multi-user-demo* )
  "If the original file is auto-saved." )

( defvar +allowed-file-operations+
  ( append '( list :open :close :closeall )
          ( if ( not *multi-user-demo* )
              '( :browse :load :new :save :saveas :saveall :openall ) ) )
  "If the file operation is not in this list it will not be processed." )

( defvar +allowed-edit-operation+
  ( append
   '( :cancel :update :cut :copy :paste :delete :before :inside :after :free
    :surround :comment :transpose :collapse :collapseall :expand :expandall
    :left :up :right :down :undo :redo )
   ( if ( not *multi-user-demo* ) '( :execute ) ) )
  "If the editing operation is not in this list it will not be processed." )

( defvar +allowed-hint-operations+
  ( append '( :complete :search :history :hyperspec :case )
          ( if ( not *multi-user-demo* ) '( :similar ) ) ) )