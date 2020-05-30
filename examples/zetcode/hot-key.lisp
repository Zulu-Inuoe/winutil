(defpackage #:com.inuoe.winutil.examples.hot-key
  (:use #:cl)
  (:export
   #:hot-key))

(in-package #:com.inuoe.winutil.examples.hot-key)

(defun center-window (window)
  (let ((screen-w (win32:get-system-metrics win32:+sm-cxscreen+))
        (screen-h (win32:get-system-metrics win32:+sm-cyscreen+)))
    (multiple-value-bind (width height) (winutil:hwnd-size window)
      (setf (winutil:hwnd-pos window) (values (truncate (- screen-w width) 2)
                                              (truncate (- screen-h height) 2)) ))))

(defclass hot-key (winutil:window)
  ()
  (:default-initargs
   :class-name "Application"
   :name "Hot key"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 270 :height 170))

(defconstant +id-hotkey+ 1)

(defmethod winutil:call-wndproc ((window hot-key) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (win32:register-hot-key (winutil:hwnd window) +id-hotkey+ win32:+mod-control+ #x43))
    (#.win32:+wm-hotkey+
     (case wparam
       (#.+id-hotkey+
        (center-window window))))
    (#.win32:+wm-destroy+
     (win32:unregister-hot-key (winutil:hwnd window) +id-hotkey+)
     (win32:post-quit-message 0)))

  (call-next-method))

(defun hot-key ()
  (let ((window (make-instance 'hot-key)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
