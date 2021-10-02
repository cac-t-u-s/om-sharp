;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/dde:lispworks-ide.lisp,v 1.13.8.1 2017/01/19 11:50:02 martin Exp $" -*-

;;;==============================
;;; OM# DDE server for Windows
;;; Loaded from the delivery file
;;;==============================


;;; ========================================================================
;;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.
;;; ========================================================================

;;; ========================================================================
;;; examples/dde/lispworks-ide.lisp
;;;
;;; This example demonstrates DDE support for Windows Explorer interaction
;;; with the LispWorks IDE.
;;;
;;; To test it, you need to configure the Windows Explorer (below), and
;;; then you can do one of the following:
;;; a) load this file into an image and call
;;;    (STARTUP-EDITOR-DDE-SERVER)
;;; b) Save an image with this file loaded and select it when configuring
;;;    the Explorer in step (6) below.
;;; c) Load this file in .lispworks (or the siteinit file).
;;; d) Specify this file (or a file that loads it) as the -init file
;;;    when configuring the Explorer.


;;; Configuring Windows Explorer:
;;; You can configure any file type, but it is assumed here that
;;; you want to configure the file type "lisp" to be opened
;;; by LispWorks.
;;;
;;;   1. Choose Tools->Folder Options... from any explorer window.
;;;   2. Select the File Types tab
;;;   3. if the extension "lisp" already exists, select it,
;;;      otherwise click the "New" button and enter "lisp" as
;;;      the extension.
;;;   4. With "lisp" selected, click on the "Advanced" button.
;;;   5. If the action "open" exists, double click on it. Otherwise
;;;      click the "New" button and enter "open" as the "Action:"
;;;   6. Enter the LispWorks executable as the "application used to perform
;;;      action". You can use the "Browse" button to find it. By default,
;;;      it is "C:/Program Files/LispWorks/lispworks-<version>-<platform>.exe
;;;      If you want it to load any special file (e.g. this file), add -init
;;;      and the filename.
;;;   7. Select "Use DDE".
;;;   8. Enter [open("%1")] as the "message".
;;;      The syntax of the square brackets is fixed. The word "open"
;;;      is the function name, which must match the function name in
;;;      WIN32:DEFINE-DDE-SERVER-FUNCTION below. The arguments in the
;;;      parentheses must match the arguments of the function.
;;;   9. Enter LispWorks as the "application".
;;;      This must must the :SERVICE name in WIN32:DEFINE-DDE-SERVER below.
;;;  10. Leave the "DDE Application Not Running:" blank.
;;;  11. Enter Editor as the "topic".
;;;      This must match the topic in WIN32:DEFINE-DDE-DISPATCH-TOPIC below.
;;;  12. Click OK on the New Action dialog.
;;;  13. Click Close on the Add New File Type dialog and the Options dialog.
;;;
;;; Once you finished the configuration, you should be able to invoke
;;; LispWorks by clicking on a file with the extension "lisp" (or
;;; whatever extension you used).


;;; The code below also defines LOAD server-function. To make
;;; double-clicking cause LispWorks to load the file rather than edit
;;; it, you need to use [load("%1")] instead of [open("%1")] in the
;;; message (step (8) above).  You can make opening a fasl be loading
;;; into LispWorks by using the message load with the appropriate fasl
;;; extension (in steps (2) and (3)):
;;;     32-bit LispWorks    ofasl
;;;     64-bit LispWorks    64ofasl



;;; The code below demonstrates how to deal with issues that may arise
;;; in a "complex" application.
;;;
;;; The main point to note is that for the DDE server to work, the
;;; thread (a MP:PROCESS inside LISP) on which it was registered (the
;;; call to WIN32:START-DDE-SERVER) must be processing Windows
;;; messages. That means that this thread should not be involved in
;;; other activity which may cause it to stop processing Windows
;;; events. Below we use a dedicated process which processes Windows
;;; messages by using MP:WAIT-PROCESSING-EVENTS.
;;;
;;; The other problems are all to do with starting up. The Windows
;;; mechanism that the Windows Explorer uses waits for the first
;;; thread in the process that it starts to process Windows messages.
;;; If this thread does not process DDE messages, the operation fails.
;;; Therefore it is essential to start the DDE server (i.e. call
;;; WIN32:START-DDE-SERVER) before starting any other thread that may
;;; process Windows messages. This includes any GUI thread.
;;;
;;; The "application" that is used here is the LispWorks IDE, which
;;; does start GUI threads. Normally, you can write your code to start
;;; the DDE server, and then start other processes after
;;; WIN32:START-DDE-SERVER was called. But the IDE starts
;;; automatically, so the code below must make sure that
;;; WIN32:START-DDE-SERVER is called before the IDE windows are made.
;;;
;;; This raises another issue, because the DDE callback below calls
;;; ED, and it should interact properly with the IDE (for example, if
;;; you use MDI, it should use an editor inside the MDI). For this to
;;; work, the code needs to make sure that the IDE is ready before it
;;; actually processes DDE events.
;;; The order of actions needs to be:
;;;   1) Call WIN32:START-DDE-SERVER
;;;   2) Start the IDE windows
;;;   3) Start processing events
;;;
;;; To get the right order of action the code below hooks into the
;;; "Initialize LispWorks Tools" action (using DEFINE-ACTION), and
;;; action (2) above is actually
;;;   (2.a) Telling the IDE that it can make windows
;;;   (2.b) Wait for the IDE to make its windows.
;;; This is implemented by RUN-LISPWORKS-IDE-SERVER-LOOP.
;;;
;;; The hook on "Initialize LispWorks Tools",
;;; STARTUP-EDITOR-DDE-SERVER, starts a process with
;;; RUN-LISPWORKS-IDE-SERVER-LOOP as the function, waits for it to
;;; signal that it can make windows, and then returns.  The
;;; initialization of the environment then continues as usual, and
;;; once it finish (a listener has been made)
;;; RUN-LISPWORKS-IDE-SERVER-LOOP continues to process DDE events.

;;; The overall order of actions is:
;;; 1) The IDE starts to initialize.
;;; 2) STARTUP-EDITOR-DDE-SERVER is called, starts a process with
;;;    RUN-LISPWORKS-IDE-SERVER-LOOP and calls PROCESS-WAIT.
;;; 3) RUN-LISPWORKS-IDE-SERVER-LOOP starts the DDE.
;;; 4) RUN-LISPWORKS-IDE-SERVER-LOOP signals that it started the DDE
;;;    by calling SIGNAL-DDE-STARTED.
;;; 5) RUN-LISPWORKS-IDE-SERVER-LOOP calls WAIT-FOR-THE-APPLICATION
;;;    which calls PROCESS-WAIT.
;;; 6) STARTUP-EDITOR-DDE-SERVER wakes up and returns.
;;; 7) The IDE continues to initialize.
;;; 8) When the listener is made, WAIT-FOR-THE-APPLICATION wakes
;;;    up and returns.
;;; 9) RUN-LISPWORKS-IDE-SERVER-LOOP starts to process DDE events.



(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "dde"))

;;; Define the server functionality

(win32:define-dde-server omsharp-server ()
  ()
  (:service "OM#"))

(win32:define-dde-dispatch-topic document :server omsharp-server)

(win32:define-dde-server-function (open :topic document)
    :execute
    ((filename string))
  (let ((path (print (probe-file filename))))
    (when path
      (setf *omsharp-app-fileopen* t)
      (print path)
      (om::open-om-document path)
      t)))


;;; The initial function of the DDE server process.
;;; 1) start the DDE server
;;; 2.a) Signal that the DDE started for anybody that waits for it.
;;; 2.b) Wait for the "application" (i.e. the IDE) to be ready.
;;; 3) Process events.

(defun run-omsharp-server-loop (wait-object)
  "Starts the DDE server and runs its message loop."
  (win32:start-dde-server 'omsharp-server) ;; (1)
  (signal-dde-started wait-object)               ;; (2.a)
  (wait-for-the-application)                     ;; (2.b)
  (my-loop-processing-dde-events))               ;; (3)



;;; (3) Processing events. This could be just a call
;;; (mp:wait-processing-events nil)
;;; if you don't have any periodic cleanups.

(defun my-loop-processing-dde-events ()
  (loop
   ;;; (mp:wait-processing-events 100)
   ;;; (do-periodic-cleanups)
   (mp:wait-processing-events 100)
   ))


;;; (2.b) This one waits for the "application" (i.e te IDE) to be ready.
;;; It checks for that by checking if there is any LW-TOOLS:LISTENER
;;; running.

(defun wait-for-the-application ()
  (mp:process-wait "Waiting for environment to start"
                   'capi:collect-interfaces 'om::om-main-window))

;;;; If an image was saved with this file loaded, this
;;;; gives the option to start LispWorks without it
;;;; acting as a DDE server.

(defun want-to-start-editor-dde-server ()
  (not (SYSTEM::HAVE-LINE-ARGUMENT-P "-no-dde")))


;;; (2.a) Signals that the DDE started. The waiting caller
;;; (in startup-editor-dde-server) waits for the
;;; car to become non-nil.

(defun signal-dde-started (wait-object)
  (setf (car wait-object) t))

;;; Start the Editor DDE server.
;;; First check if really want to do it.
;;; Create a process to run the DDE server, and then waits
;;; for it to be ready, so nothing else happens until
;;; win32:start-dde-server have been called.


(defun startup-omsharp-dde-server (&optional screen)
  (declare (ignore screen))
  (when (want-to-start-editor-dde-server)
    (let ((wait-object (list nil)))
      (mp:process-run-function "OM# DDE server" '() 'run-omsharp-server-loop wait-object)
      (mp:process-wait "Waiting for OM# DDE server to register" 'car wait-object))))


; (startup-omsharp-dde-server)
