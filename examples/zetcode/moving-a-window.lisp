(defpackage #:com.inuoe.winutil.examples.moving-a-window
  (:use #:cl)
  (:import-from #:win32)
  (:import-from #:winutil)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.moving-a-window)

(defclass moving-a-window (winutil:window)
  ((x-label
    :type winutil:hwnd-wrapper
    :accessor x-label)
   (y-label
    :type winutil:hwnd-wrapper
    :accessor y-label))
  (:default-initargs
   :class-name "Moving"
   :name "Moving"
   :style (logior win32:+ws-overlappedwindow+ win32:+ws-visible+)
   :background (win32:get-sys-color-brush win32:+color-3dface+)
   :x 150 :y 150 :width 250 :height 180))

(defun create-labels (window)
  (make-instance 'winutil:hwnd-wrapper :wndclass "static" :name "x: "
                                       :style (logior win32:+ws-child+ win32:+ws-visible+)
                                       :parent window :menu 1
                                       :x 10 :y 10 :width 25 :height 25)

  (setf (x-label window) (make-instance 'winutil:hwnd-wrapper :wndclass "static" :name "150"
                                                              :style (logior win32:+ws-child+ win32:+ws-visible+)
                                                              :parent window :menu 2
                                                              :x 40 :y 10 :width 55 :height 25))

  (make-instance 'winutil:hwnd-wrapper :wndclass "static" :name "y: "
                                       :style (logior win32:+ws-child+ win32:+ws-visible+)
                                       :parent window :menu 3
                                       :x 10 :y 30 :width 25 :height 25)

  (setf (y-label window) (make-instance 'winutil:hwnd-wrapper :wndclass "static" :name "150"
                                                              :style (logior win32:+ws-child+ win32:+ws-visible+)
                                                              :parent window :menu 4
                                                              :x 40 :y 30 :width 55 :height 25)))

(defmethod winutil:call-wndproc ((window moving-a-window) msg wparam lparam)
  (case msg
    (#.win32:+wm-create+
     (create-labels window))
    (#.win32:+wm-move+
     (multiple-value-bind (x y) (winutil:hwnd-pos window)
       (setf (winutil:hwnd-text (x-label window)) (format nil "~D" x)
             (winutil:hwnd-text (y-label window)) (format nil "~D" y))))
    (#.win32:+wm-destroy+
     (win32:post-quit-message 0)))
  (call-next-method))

(defun main (&optional argv)
  (declare (ignore argv))
  (let ((window (make-instance 'moving-a-window)))
    (when (find-package '#:slynk)
      (win32:show-window (winutil:hwnd window) win32:+sw-show+))
    (winutil:message-pump)))
