(in-package #:winutil)

(defclass hwnd-wrapper (disposable)
  ((%hwnd
    :type cffi:foreign-pointer
    :reader hwnd-wrapper-hwnd)))

(defmethod initialize-instance :after ((obj hwnd-wrapper)
                                       &key
                                         (wndclass (required-argument :wndclass))
                                         (ex-style win32:+ws-ex-overlapped-window+)
                                         (name "MainWindow")
                                         (style win32:+ws-overlappedwindow+)
                                         (x win32:+cw-usedefault+)
                                         (y win32:+cw-usedefault+)
                                         (width win32:+cw-usedefault+)
                                         (height win32:+cw-usedefault+)
                                         (parent (cffi:null-pointer))
                                         (menu (cffi:null-pointer))
                                         (instance (wndclass-instance wndclass))
                                         (lparam (cffi:null-pointer))
                                       &allow-other-keys)
  (let ((hwnd (win32:create-window-ex
               ex-style (wndclass-atom wndclass) name style
               x y width height
               (hwnd parent) menu instance lparam)))
    (check-win32-not-null hwnd)
    (setf (slot-value obj '%hwnd) hwnd)))

(define-dispose (obj hwnd-wrapper)
  (or (win32:destroy-window (hwnd-wrapper-hwnd obj))
      (win32-error))
  (slot-makunbound obj '%hwnd))

(defmethod hwnd ((obj hwnd-wrapper))
  (hwnd-wrapper-hwnd obj))
