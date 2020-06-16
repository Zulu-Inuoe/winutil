(defpackage #:com.inuoe.winutil.examples.a-simple-menu
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:win32)
  (:import-from #:winutil)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.a-simple-menu)

(defconstant +idm-file-new+ 1)
(defconstant +idm-file-open+ 2)
(defconstant +idm-file-quit+ 3)

(defclass a-simple-menu (winutil:window)
  ()
  (:default-initargs
   :class-name "Simple menu"
   :name "Simple menu"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 350 :height 250))

(defun add-menus (window)
  (let ((menu-bar (win32:create-menu))
        (menu (win32:create-menu)))
    (win32:append-menu menu win32:+mf-string+ +idm-file-new+ "&New")
    (win32:append-menu menu win32:+mf-string+ +idm-file-open+ "&Open")
    (win32:append-menu menu win32:+mf-separator+ 0 (cffi:null-pointer))
    (win32:append-menu menu win32:+mf-string+ +idm-file-quit+ "&Quit")
    (win32:append-menu menu-bar win32:+mf-popup+ (cffi:pointer-address menu) "&File")
    (win32:set-menu (winutil:hwnd window) menu-bar)))

(defmethod winutil:call-wndproc ((window a-simple-menu) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (add-menus window))
    (#.win32:+wm-command+
     (case (win32:loword wparam)
       ((#.+idm-file-new+ #.+idm-file-open+)
        (win32:message-beep win32:+mb-iconinformation+))
       (#.+idm-file-quit+
        (win32:send-message (winutil:hwnd window) win32:+wm-close+ 0 0))))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)))

  (call-next-method))

(defun main (&optional argv)
  (declare (ignore argv))
  (let ((window (make-instance 'a-simple-menu)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
