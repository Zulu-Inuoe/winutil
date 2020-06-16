(defpackage #:com.inuoe.winutil.examples.centering-a-window
  (:use #:cl)
  (:import-from #:win32)
  (:import-from #:winutil)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.centering-a-window)

(defun center-window (window)
  (let ((screen-w (win32:get-system-metrics win32:+sm-cxscreen+))
        (screen-h (win32:get-system-metrics win32:+sm-cyscreen+)))
    (multiple-value-bind (width height) (winutil:hwnd-size window)
      (setf (winutil:hwnd-pos window) (values (truncate (- screen-w width) 2)
                                              (truncate (- screen-h height) 2)) ))))

(defclass centering-a-window (winutil:window)
  ()
  (:default-initargs
   :class-name "Center"
   :name "Center"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 270 :height 170))

(defmethod winutil:call-wndproc ((window centering-a-window) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (center-window window))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)
     (return-from winutil:call-wndproc 0)))

  (call-next-method))

(defun main (&optional argv)
  (declare (ignore argv))
  (let ((window (make-instance 'centeting-a-window)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
