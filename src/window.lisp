(in-package #:winutil)

(defclass window (disposable)
  ((%wndclass-wrapper
    :type wndclass-wrapper)
   (%hwnd-wrapper
    :type hwnd-wrapper))
  (:documentation
   "Higher level interface to an `hwnd' allowing for generic function dispatch of wndproc"))

(defvar %*windows* (make-hash-table))

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
  (:method ((window window) msg wparam lparam)
    "Pass to def-window-proc"
    (win32:def-window-proc (hwnd window) msg wparam lparam))
  (:documentation
   "Invokes the wndproc of `window' with the given arguments."))

(defvar %*creating-window* nil
  "The `window' currently being created")
(makunbound '%*creating-window*)

(defwndproc %window-wndproc (hwnd msg wparam lparam)
  "wndproc used for `window' subclasses.
Ensures correct context and dispatches to `call-wndproc'"
  (case msg
    (#.win32:+wm-getminmaxinfo+
     ;; Special hook to catch initial window creation
     (when (boundp '%*creating-window*)
       (setf (slot-value %*creating-window* '%hwnd-wrapper) %*creating-hwnd-wrapper*
             (slot-value (slot-value %*creating-window* '%hwnd-wrapper) '%hwnd) hwnd
             (gethash (cffi:pointer-address hwnd) %*windows*) %*creating-window*))

     (call-wndproc (gethash (cffi:pointer-address hwnd) %*windows*) msg wparam lparam))
    (#.win32:+wm-ncdestroy+
     (let ((window (gethash (cffi:pointer-address hwnd) %*windows*)))
       (unwind-protect (call-wndproc window msg wparam lparam)
         (remhash (cffi:pointer-address hwnd) %*windows*)
         (slot-makunbound (slot-value window '%hwnd-wrapper) '%hwnd)
         (slot-makunbound window '%hwnd-wrapper)
         (dispose window))))
    (t
     (call-wndproc (gethash (cffi:pointer-address hwnd) %*windows*) msg wparam lparam))))

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
                                         (wndclass-name (format nil "Window[~A(~A);~A]"
                                                                (lisp-implementation-type)
                                                                (lisp-implementation-version)
                                                                (%make-guid)))
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
  (let ((wndclass-wrapper (make-instance 'wndclass-wrapper
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
               (dispose hwnd-wrapper)
               (error "Window initialization failed")))
           (setf success t))
      (unless success
        (slot-makunbound obj '%wndclass-wrapper)
        (dispose wndclass-wrapper)))))

(define-dispose (obj window)
  (unwind-protect (when (slot-boundp obj '%hwnd-wrapper)
                    (dispose (slot-value obj '%hwnd-wrapper)))
    (dispose (slot-value obj '%wndclass-wrapper))
    (slot-makunbound obj '%wndclass-wrapper)))

(defmethod hwnd ((obj window))
  (hwnd (slot-value obj '%hwnd-wrapper)))

(defmethod wndclass-name ((obj window))
  (wndclass-name (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-instance ((obj window))
  (wndclass-instance (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-atom ((obj window))
  (wndclass-atom (slot-value obj '%wndclass-wrapper)))
