(defpackage #:com.inuoe.winutil.examples.flashing-a-window
  (:use #:cl)
  (:export
   #:flashing-a-window))

(in-package #:com.inuoe.winutil.examples.flashing-a-window)

(defclass flashing-a-window (winutil:window)
  ()
  (:default-initargs
   :class-name "Flash"
   :name "Flash"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 100 :y 100 :width 250 :height 180))

(defmethod winutil:call-wndproc ((window flashing-a-window) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (make-instance 'winutil:hwnd-wrapper :wndclass "Button" :name "Flash"
                                          :style (logior win32:+ws-child+ win32:+ws-visible+)
                                          :x 10 :y 10 :width 80 :height 25
                                          :parent window :menu 1))
    (#.win32:+wm-command+
     (cffi:with-foreign-object (fwi 'win32:flashwindowinfo)
       (cffi:with-foreign-slots ((win32:size win32:flags win32:timeout win32:hwnd win32:count)
                                 fwi win32:flashwindowinfo)
         (setf win32:size (cffi:foreign-type-size 'win32:flashwindowinfo)
               win32:flags win32:+flashw-all+
               win32:timeout 0
               win32:hwnd (winutil:hwnd window)
               win32:count 4))
       (win32:flash-window-ex fwi)))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)))
  (call-next-method))

(defun flashing-a-window ()
  (let ((window (make-instance 'flashing-a-window)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
