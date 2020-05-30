(defpackage #:com.inuoe.winutil.examples.the-escape-key
  (:use #:cl)
  (:export
   #:the-escape-key))

(in-package #:com.inuoe.winutil.examples.the-escape-key)

(defclass the-escape-key (winutil:window)
  ()
  (:default-initargs
   :class-name "Escape"
   :name "Escape"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 250 :height 180))

(defmethod winutil:call-wndproc ((window the-escape-key) msg wparam lparam)
  (case msg
    (#.win32:+wm-keydown+
     (case wparam
       (#.win32:+vk-escape+
        (case (win32:message-box (winutil:hwnd window) "Are you sure to quit?" "Message" win32:+mb-okcancel+)
          (#.win32:+idok+
           (win32:send-message (winutil:hwnd window) win32:+wm-close+ 0 0))))))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)))
  (call-next-method))

(defun the-escape-key ()
  (let ((window (make-instance 'the-escape-key)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))