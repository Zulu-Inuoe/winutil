(in-package #:com.inuoe.winutil)

(defvar %*windows* (make-hash-table)
  "Table of created `window' instances")

(defclass window (d:disposable)
  ((%wndclass-wrapper
    :type wndclass-wrapper)
   (%hwnd-wrapper
    :type hwnd-wrapper))
  (:documentation
   "Higher level interface to an `hwnd' allowing for generic function dispatch of wndproc"))

(declaim (type window %*creating-window*))
(defvar %*creating-window*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '%*creating-window* 'variable) "The `window' currently being created"))

(declaim (type (or null (cons cffi:foreign-pointer list)) %*wndclass-manager-hook*))
(defvar %*wndclass-manager-hook* nil)
(pushnew '(%*wndclass-manager-hook* . nil) bt:*default-special-bindings* :test #'equal)
(when (find-package #1='#:slynk)
  (pushnew '(%*wndclass-manager-hook* . nil) (symbol-value (find-symbol (string '#:*default-worker-thread-bindings*) #1#)) :test #'equal))

(defmsgproc window-manager-hook (code wparam msg)
  "Hook for cleaning up wndclass instances after their `window' has been destroyed."
  (loop
    :for wndclass :in (nreverse (cdr %*wndclass-manager-hook*))
    :do (d:dispose wndclass))
  (or (win32:unhook-windows-hook-ex (car %*wndclass-manager-hook*))
      (win32-error))
  (setf %*wndclass-manager-hook* nil)
  (win32:call-next-hook-ex (cffi:null-pointer) code wparam msg))

(defgeneric call-wndproc (window msg wparam lparam)
  (:method (window msg wparam lparam)
    "Try getting the hwnd of `window' and calling its wndproc."
    (let ((hwnd (hwnd window)))
      (cffi:foreign-funcall-pointer (cffi:make-pointer (win32:get-window-long-ptr hwnd win32:+gwl-wndproc+))
                                    (:convention :stdcall)
                                    win32:hwnd hwnd
                                    win32:uint msg
                                    win32:wparam wparam
                                    win32:lparam lparam)))

  (:method :around ((window window) msg wparam lparam)
    ;; Guard against already-disposed windows
    (when (d:disposedp window)
      (error "The window ~A has already been disposed." window))
    (call-next-method))
  (:method ((window window) msg wparam lparam)
    "Pass to def-window-proc"
    ;; The window may have been disposed by a more
    ;; specialized method which then used `call-next-method'
    (if (not (d:disposedp window))
        (win32:def-window-proc (hwnd window) msg wparam lparam)
        0))
  (:documentation
   "Invokes the wndproc of `window' with the given arguments."))

(defun %ensure-wndclass-manager ()
  (unless %*wndclass-manager-hook*
    (setf %*wndclass-manager-hook* (list (not-null-or-error
                                          (win32:set-windows-hook-ex
                                           win32:+wh-getmessage+
                                           (cffi:callback window-manager-hook)
                                           (cffi:null-pointer)
                                           (win32:get-current-thread-id)))))))

(defwndproc %window-wndproc (hwnd msg wparam lparam)
  "wndproc used for `window' subclasses.
Ensures correct context and dispatches to `call-wndproc'"
  (let ((hwnd-addr (cffi:pointer-address hwnd)))
    (case msg
      ;; TODO - Looks like when a child window is created the first message
      ;;        it receives is nccreate, as opposed to getminmaxinfo
      ;;        need to investigate this more
      ((#.win32:+wm-getminmaxinfo+ #.win32:+wm-nccreate+)
       ;; Special hook to catch initial window creation
       (when (boundp '%*creating-window*)
         (setf (slot-value %*creating-window* '%hwnd-wrapper) %*creating-hwnd-wrapper*
               (slot-value (slot-value %*creating-window* '%hwnd-wrapper) '%hwnd) hwnd
               (gethash hwnd-addr %*windows*) %*creating-window*))

       (call-wndproc (gethash hwnd-addr %*windows*) msg wparam lparam))
      (#.win32:+wm-ncdestroy+
       (let ((window (gethash hwnd-addr %*windows*)))
         (unwind-protect (call-wndproc window msg wparam lparam)
           (remhash hwnd-addr %*windows*)
           (slot-makunbound (slot-value window '%hwnd-wrapper) '%hwnd)
           (slot-makunbound window '%hwnd-wrapper)
           (%ensure-wndclass-manager)
           (push (slot-value window '%wndclass-wrapper) (cdr %*wndclass-manager-hook*))
           (d:dispose window))))
      (t
       (call-wndproc (gethash hwnd-addr %*windows*) msg wparam lparam)))))

(defun %exe-name ()
  "Get the current executable's path and type as <path>.<type>"
  (let ((path (exe-pathname)))
    (concatenate 'string (pathname-name path) "." (pathname-type path))))

(defmethod initialize-instance :after ((obj window)
                                       &key
                                         (class-style 0)
                                         (cls-extra 0)
                                         (wnd-extra 0)
                                         (instance (win32:get-module-handle (cffi:null-pointer)))
                                         (icon (cffi:null-pointer))
                                         (cursor (win32:load-cursor (cffi:null-pointer) win32:+idc-arrow+))
                                         (background (cffi:make-pointer (1+ win32:+color-window+)))
                                         (menu-name (cffi:null-pointer))
                                         (icon-sm (cffi:null-pointer))
                                         wndclass-name
                                         (ex-style win32:+ws-ex-overlapped-window+)
                                         (name "MainWindow")
                                         (style win32:+ws-overlappedwindow+)
                                         (x win32:+cw-usedefault+)
                                         (y win32:+cw-usedefault+)
                                         (width win32:+cw-usedefault+)
                                         (height win32:+cw-usedefault+)
                                         (parent (cffi:null-pointer))
                                         (menu (cffi:null-pointer))
                                         (lparam (cffi:null-pointer))
                                       &allow-other-keys)
  (let* ((wndclass-name (or wndclass-name
                            (format nil "Window[~A(~A);~A]"
                                    (let ((name (%exe-name)))
                                      (if (<= (length name) 128)
                                          name
                                          (subseq name 0 128)))
                                    (let ((name (bt:thread-name (bt:current-thread))))
                                      (if (<= (length name) 64)
                                          name
                                          (subseq name 0 64)))
                                    (%make-guid))))
         (wndclass-wrapper (make-instance 'wndclass-wrapper
                                          :style class-style :cls-extra cls-extra
                                          :wnd-extra wnd-extra :instance instance
                                          :icon icon :cursor cursor
                                          :background background :menu-name menu-name
                                          :icon-sm icon-sm :name wndclass-name
                                          :wndproc (cffi:callback %window-wndproc)))
         (success nil))
    (unwind-protect
         (let ((%*creating-window* obj))
           (setf (slot-value obj '%wndclass-wrapper) wndclass-wrapper)
           (let ((hwnd-wrapper (make-instance 'hwnd-wrapper :ex-style ex-style :wndclass wndclass-wrapper
                                                            :name name :style style
                                                            :x x :y y :width width :height height
                                                            :parent parent :menu menu :instance instance
                                                            :lparam lparam)))
             (unless (eq hwnd-wrapper (slot-value obj '%hwnd-wrapper))
               (d:dispose hwnd-wrapper)
               (error "Window initialization failed")))
           (setf success t))
      (unless success
        (slot-makunbound obj '%wndclass-wrapper)
        (d:dispose wndclass-wrapper)))))

(d:define-dispose (obj window)
  ;; Don't dispose wndclass - that'll be done by `window-manager',
  (when (slot-boundp obj '%hwnd-wrapper)
    (d:dispose (slot-value obj '%hwnd-wrapper))))

(defmethod hwnd ((obj window))
  (hwnd (slot-value obj '%hwnd-wrapper)))

(defmethod wndclass-name ((obj window))
  (wndclass-name (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-instance ((obj window))
  (wndclass-instance (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-atom ((obj window))
  (wndclass-atom (slot-value obj '%wndclass-wrapper)))
