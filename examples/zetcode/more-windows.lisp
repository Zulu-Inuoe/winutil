(defpackage #:com.inuoe.winutil.examples.more-windows
  (:use #:cl)
  (:import-from #:win32)
  (:import-from #:winutil)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.more-windows)

(defclass more-windows (winutil:window)
  ((panels
    :type list
    :initform nil
    :accessor panels))
  (:default-initargs
   :class-name "Windows"
   :name "Windows"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 250 :height 180))

(dispose:define-dispose (window more-windows)
  (dolist (panel (panels window))
    (dispose:dispose panel)))

(defclass panel (winutil:window)
  ((background
    :reader background
    :initarg :background))
  (:default-initargs
   :style (logior win32:+ws-child+ win32:+ws-visible+)))

(dispose:define-dispose (window panel)
  (win32:delete-object (background window)))

(defclass red-panel (panel)
  ()
  (:default-initargs
   :class-name "RedPanelClass"
   :background (win32:create-solid-brush (win32:rgb 255 0 0))
   :menu 1
   :x 20 :y 20 :width 80 :height 80))

(defclass blue-panel (panel)
  ()
  (:default-initargs
   :class-name "BluePanelClass"
   :background (win32:create-solid-brush (win32:rgb 0 0 255))
   :menu 2
   :x 120 :y 20 :width 80 :height 80))

(defmethod winutil:call-wndproc ((window panel) msg wparam lparam)
  (case msg
    (#.win32:+wm-lbuttonup+
     (win32:message-beep win32:+mb-ok+)))
  (call-next-method))

(defmethod winutil:call-wndproc ((window more-windows) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (push (make-instance 'red-panel :parent window) (panels window))
     (push (make-instance 'blue-panel :parent window) (panels window)))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)
     (return-from winutil:call-wndproc 0)))
  (call-next-method))

(defun main (&optional argv)
  (declare (ignore argv))
  (let ((window (make-instance 'more-windows)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
