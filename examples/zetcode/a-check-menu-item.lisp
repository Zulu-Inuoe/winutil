(defpackage #:com.inuoe.winutil.examples.a-check-menu-item
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:win32)
  (:import-from #:winutil)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.a-check-menu-item)

(defconstant +idm-view-stb+ 1)

(defclass a-check-menu-item (winutil:window)
  ((menu
    :type cffi:foreign-pointer
    :accessor menu)
   (sub-window
    :type (or null winutil:hwnd-wrapper)
    :initform nil
    :accessor sub-window))
  (:default-initargs
   :class-name "Check menu item"
   :name "Check menu item"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 350 :height 250))

(defun add-menus (window)
  (let ((menu-bar (win32:create-menu))
        (menu (win32:create-menu)))
    (setf (menu window) menu)

    (win32:append-menu menu win32:+mf-string+ +idm-view-stb+ "&Statusbar")
    (win32:check-menu-item menu +idm-view-stb+ win32:+mf-checked+)

    (win32:append-menu menu-bar win32:+mf-popup+ (cffi:pointer-address menu) "&View")

    (win32:set-menu (winutil:hwnd window) menu-bar)))

(defmethod winutil:call-wndproc ((window a-check-menu-item) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (add-menus window)
     (win32:init-common-controls)
     (setf (sub-window window)
           (make-instance 'winutil:hwnd-wrapper
                          :wndclass win32:+statusclassname+
                          :style (logior win32:+ws-child+ win32:+ws-visible+)
                          :parent window :menu 1)))
    (#.win32:+wm-command+
     (case (win32:loword wparam)
       (#.+idm-view-stb+
        (let ((menu (menu window)))
          (multiple-value-bind (sw check-state)
              (case  (win32:get-menu-state menu +idm-view-stb+ win32:+mf-bycommand+)
                (#.win32:+mf-checked+
                 (values win32:+sw-hide+ win32:+mf-unchecked+))
                (t
                 (values win32:+sw-showna+ win32:+mf-checked+)))
            (win32:show-window (winutil:hwnd (sub-window window)) sw)
            (win32:check-menu-item menu +idm-view-stb+ check-state))))))
    (#.win32:+wm-size+
     (win32:send-message (winutil:hwnd (sub-window window)) win32:+wm-size+ wparam lparam))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)))

  (call-next-method))

(defun main (&optional argv)
  (declare (ignore argv))
  (let ((window (make-instance 'a-check-menu-item)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
