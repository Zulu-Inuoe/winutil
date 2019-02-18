(in-package #:winutil)

(defclass window (disposable)
  ((%wndclass-name
    :type string
    :initform (format nil "Window[~A(~A);~A]"
                      (lisp-implementation-type)
                      (lisp-implementation-version)
                      (%make-guid))
    :initarg :wndclass-name
    :reader window-wndclass-name)
   (%instance
    :type cffi:foreign-pointer
    :initarg :instance
    :initform (win32:get-module-handle (cffi:null-pointer))
    :reader window-instance)
   (%class-atom
    :type cffi:foreign-pointer
    :reader window-class-atom)
   (%hwnd
    :type cffi:foreign-pointer
    :reader window-hwnd))
  (:documentation
   "Higher level interface to an `hwnd' allowing for generic function dispatch of wndproc"))

(defvar %*windows* (make-hash-table))

(defgeneric window-wndproc (window msg wparam lparam)
  (:method ((window window) msg wparam lparam)
    (win32:def-window-proc (window-hwnd window) msg wparam lparam))
  (:documentation
   "A win32 wndproc."))

(defvar %*creating-window*)

(defwndproc %window-wndproc (hwnd msg wparam lparam)
  "wndproc used for `window' subclasses.
Ensures correct context and dispatches to `window-wndproc'"
  (case msg
    (#.win32:+wm-getminmaxinfo+
     (cond
       ((boundp '%*creating-window*)
        (setf (slot-value %*creating-window* '%hwnd) hwnd
              (gethash (cffi:pointer-address hwnd) %*windows*) %*creating-window*)
        (window-wndproc %*creating-window* msg wparam lparam))
       (t
        (window-wndproc (gethash (cffi:pointer-address hwnd) %*windows*) msg wparam lparam))))
    (#.win32:+wm-ncdestroy+
     (let ((window (gethash (cffi:pointer-address hwnd) %*windows*)))
       (unwind-protect
            (window-wndproc window msg wparam lparam)
         (remhash (cffi:pointer-address hwnd) %*windows*)
         (slot-makunbound window '%hwnd)
         (dispose window))))
    (t
     (window-wndproc (gethash (cffi:pointer-address hwnd) %*windows*) msg wparam lparam))))

(defmethod initialize-instance :after ((obj window)
                                       &key
                                         (class-style 0)
                                         (cls-extra 0)
                                         (wnd-extra 0)
                                         (icon (cffi:null-pointer))
                                         (cursor (win32:load-cursor (cffi:null-pointer) win32:+idc-arrow+))
                                         (background (cffi:make-pointer (1+ win32:+color-window+)))
                                         (menu-name (cffi:null-pointer))
                                         (icon-sm (cffi:null-pointer))
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
  (let* ((class-atom
           (cffi:make-pointer
            (cffi:with-foreign-object (class 'win32:wndclassex)
              (%zero-memory class 'win32:wndclassex)
              (cffi:with-foreign-slots ((win32:size win32:style win32:wndproc
                                                    win32:cls-extra win32:wnd-extra
                                                    win32:instance
                                                    win32:icon win32:cursor
                                                    win32:background win32:menu-name
                                                    win32:wndclass-name win32:icon-sm)
                                        class win32:wndclassex)
                (setf win32:size (cffi:foreign-type-size 'win32:wndclassex)
                      win32:style class-style
                      win32:wndproc (cffi:callback %window-wndproc)
                      win32:cls-extra cls-extra
                      win32:wnd-extra wnd-extra
                      win32:instance (window-instance obj)
                      win32:icon icon
                      win32:cursor cursor
                      win32:background background
                      win32:menu-name menu-name
                      win32:wndclass-name (window-wndclass-name obj)
                      win32:icon-sm icon-sm))
              (win32:register-class-ex class))))
        (success nil))
    (unwind-protect
         (let ((%*creating-window* obj))
           (setf (slot-value obj '%class-atom) class-atom)
           (win32:create-window-ex ex-style class-atom name style x y width height
                                   parent
                                   menu
                                   (window-instance obj)
                                   lparam)
           (setf success t))
      (unless success
        (slot-makunbound obj '%class-atom)
        (win32:unregister-class class-atom (window-instance obj))))))

(define-dispose (obj window)
  (unwind-protect
       (when (slot-boundp obj '%hwnd)
         (win32:destroy-window (window-hwnd obj)))
    (win32:unregister-class (window-class-atom obj) (window-instance obj))
    (slot-makunbound obj '%class-atom)))

(defmethod hwnd ((obj window))
  (window-hwnd obj))

(defmethod wndclass-name ((obj window))
  (window-wndclass-name obj))

(defmethod wndclass-instance ((obj window))
  (window-instance obj))

(defmethod wndclass-atom ((obj window))
  (window-class-atom obj))
