;============================================================================
; om#: visual programming language for computer-assisted music composition
;============================================================================
;
;   This program is free software. For information on usage
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
;============================================================================
; File author: J. Bresson
;============================================================================


(in-package :om)

(defvar *init-func-list* nil)

(defun add-om-init-fun (func-name)
  (unless (member func-name *init-func-list* :test 'equal)
    (pushnew func-name *init-func-list*)))

(defun om-init-funcall ()
  ;(om-print-dbg "START INIT CALLS")
  (mapc #'(lambda (x)
            (om-print-dbg (string x))
            (funcall x))
        (reverse *init-func-list*))
  ;(om-print-dbg "END INIT CALLS")
  )




(mapc #'(lambda (filename)
          (cl-user::compile&load (cl-user::decode-local-path filename)))
      '(
        "utils/lisptools"
        "utils/pathnames"
        "utils/strings"

        "core/objects"
        "core/metaclasses"
        "core/defmethod"
        "core/defclass"

        "graphics/graphic-tools"
        "graphics/icon-picts"
        "graphics/graphic-components"
        "graphics/frames"
        "graphics/rulers"

        "environment/doc-manager"
        "environment/session"
        "environment/properties"
        "environment/preferences"
        "environment/package"
        "environment/library"
        "environment/function-reference"
        "environment/swank"
        "environment/tty-listener"

        "windows/windows"
        "windows/main-window"
        "windows/preferences-window"
        "windows/editor"
        "windows/multi-editor"

        "patch/box"
        "patch/box-io"
        "patch/boxcall"
        "patch/boxsimple"
        "patch/boxobject"
        "patch/connection"
        "patch/patch"
        "patch/patch-component-boxes"
        "patch/in-out"
        "patch/patch-editor"
        "patch/boxframe"
        "patch/box-abstraction"
        "patch/boxpatch"
        "patch/eval"
        "patch/gen-code"
        "patch/reactive"
        "patch/comments"
        "patch/encapsulation"
        "patch/interfacebox"
        "patch/lost-reference"
        "patch/loop"
        "patch/lisp-function"

        "boxes/special-boxes"
        "boxes/control-boxes"
        "boxes/system-boxes"
        "boxes/repeat-n"
        "boxes/mem-collect"
        "boxes/global"
        "boxes/init-do"
        "boxes/send-receive-route"

        "tools/data-structures"
        "tools/file-utils"
        "tools/write-to-disk"
        "tools/file-stream"
        "tools/collections"
        "tools/networking"

        "utils/traduction"
        "utils/documentation"
        "utils/help"
        "utils/copy"
        "utils/save"
        "utils/undo"
        "utils/compatibility"

        "packages"

        ))



