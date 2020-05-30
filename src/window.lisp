(in-package #:winutil)

(defclass window-manager ()
  ((%wndclass-wrapper
    :type wndclass-wrapper)
   (%hwnd-wrapper
    :type hwnd-wrapper)
   (%destroyed-wndclasses
    :type list
    :initform nil))
  (:documentation
   "Container for wndclass+hwnd who's wndproc shall `dispose' of `window' instances.
 This is necessary because otherwise we would attempt to `win32:unregister-class' while a `window' was still using it."))

(declaim (type (or null window-manager) %*window-manager*))
(defvar %*window-manager* nil
  "The current thread's `window-manager'.")

(pushnew '(%*window-manager* . nil) bt:*default-special-bindings* :test #'equal)

(when (find-package #1='#:slynk)
  (pushnew '(%*window-manager* . nil) (symbol-value (find-symbol (string '#:*default-worker-thread-bindings*) #1#)) :test #'equal))

(defconstant +wm-window-destroyed+ (+ win32:+wm-user+ 0))

(defwndproc %window-manager-wndproc (hwnd msg wparam lparam)
  (case msg
    (#.+wm-window-destroyed+
     (with-slots (%destroyed-wndclasses) %*window-manager*
       (loop
         :while %destroyed-wndclasses
         :do (dispose (pop %destroyed-wndclasses))))
     0)
    (t
     (win32:def-window-proc hwnd msg wparam lparam))))

(defun %exe-name ()
  "Get the current executable's path and type as <path>.<type>"
  (let ((path (exe-pathname)))
    (concatenate 'string (pathname-name path) "." (pathname-type path))))

(defmethod initialize-instance :after ((window-manager window-manager) &key &allow-other-keys)
  (let* ((name (format nil "WindowManager[~A;~A;0x~X]"
                       (let ((name (%exe-name)))
                         (if (<= (length name) 128)
                             name
                             (subseq name 0 128)))
                       (let ((name (bt:thread-name (bt:current-thread))))
                         (if (<= (length name) 64)
                             name
                             (subseq name 0 64)))
                       (win32:get-current-thread-id)))
         (wndclass (make-instance 'wndclass-wrapper
                                  :name name
                                  :wndproc (cffi:callback %window-manager-wndproc)
                                  :background (cffi:null-pointer)
                                  :cursor (cffi:null-pointer)))
         (success nil))
    (unwind-protect (with-slots (%wndclass-wrapper %hwnd-wrapper) window-manager
                      (setf %hwnd-wrapper (make-instance 'hwnd-wrapper
                                                 :wndclass wndclass
                                                 :name name
                                                 :x 0 :y 0 :width 0 :height 0
                                                 :ex-style 0
                                                 :parent win32:+hwnd-message+)
                            %wndclass-wrapper wndclass
                            success t))
      (unless success
        (dispose wndclass)))))

(defmethod hwnd ((window-manager window-manager))
  (hwnd (slot-value window-manager '%hwnd-wrapper)))

(defun %ensure-window-manager ()
  (unless %*window-manager*
    (setf %*window-manager* (make-instance 'window-manager)))
  %*window-manager*)

(defvar %*windows* (make-hash-table)
  "Table of created `window' instances")

(defclass window (disposable)
  ((%wndclass-wrapper
    :type wndclass-wrapper)
   (%hwnd-wrapper
    :type hwnd-wrapper))
  (:documentation
   "Higher level interface to an `hwnd' allowing for generic function dispatch of wndproc"))

(declaim (type window %*creating-window*))
(defvar %*creating-window* nil
  "The `window' currently being created")
(makunbound '%*creating-window*)

(defgeneric call-wndproc (window msg wparam lparam)
  (:method (window msg wparam lparam)
    "Try getting the hwnd of `window' and calling its wndproc."
    (let ((hwnd (hwnd window)))
      (cffi:foreign-funcall-pointer (cffi:make-pointer (win32:get-window-long-ptr hwnd win32:+gwl-wndproc+))
                                    (:convention :stdcall)
                                    win32:hwnd hwnd
                                    win32:uint msg
                                    win32:wparam wparam
                                    win32:lparam lparam)))

  (:method :around ((window window) msg wparam lparam)
    ;; Guard against already-disposed windows
    (when (disposedp window)
      (error "The window ~A has already been disposed." window))
    (call-next-method))
  (:method ((window window) msg wparam lparam)
    "Pass to def-window-proc"
    ;; The window may have been disposed by a more
    ;; specialized method which then used `call-next-method'
    (if (not (disposedp window))
        (win32:def-window-proc (hwnd window) msg wparam lparam)
        0))
  (:documentation
   "Invokes the wndproc of `window' with the given arguments."))

(defwndproc %window-wndproc (hwnd msg wparam lparam)
  "wndproc used for `window' subclasses.
Ensures correct context and dispatches to `call-wndproc'"
  (let ((hwnd-addr (cffi:pointer-address hwnd)))
    (case msg
      ;; TODO - Looks like when a child window is created the first message
      ;;        it receives is nccreate, as opposed to getminmaxinfo
      ;;        need to investigate this more
      ((#.win32:+wm-getminmaxinfo+ #.win32:+wm-nccreate+)
       ;; Special hook to catch initial window creation
       (when (boundp '%*creating-window*)
         (setf (slot-value %*creating-window* '%hwnd-wrapper) %*creating-hwnd-wrapper*
               (slot-value (slot-value %*creating-window* '%hwnd-wrapper) '%hwnd) hwnd
               (gethash hwnd-addr %*windows*) %*creating-window*))

       (call-wndproc (gethash hwnd-addr %*windows*) msg wparam lparam))
      (#.win32:+wm-ncdestroy+
       (let ((window (gethash hwnd-addr %*windows*)))
         (unwind-protect (call-wndproc window msg wparam lparam)
           (remhash hwnd-addr %*windows*)
           (slot-makunbound (slot-value window '%hwnd-wrapper) '%hwnd)
           (slot-makunbound window '%hwnd-wrapper)
           (let ((wm (%ensure-window-manager)))
             (push (slot-value window '%wndclass-wrapper) (slot-value wm '%destroyed-wndclasses))
             (slot-makunbound window '%wndclass-wrapper)
             (win32:post-message (hwnd wm) +wm-window-destroyed+ 0 0))
           (dispose window))))
      (t
       (call-wndproc (gethash hwnd-addr %*windows*) msg wparam lparam)))))

(defmethod initialize-instance :after ((obj window)
                                       &key
                                         (class-style 0)
                                         (cls-extra 0)
                                         (wnd-extra 0)
                                         (instance (win32:get-module-handle (cffi:null-pointer)))
                                         (icon (cffi:null-pointer))
                                         (cursor (win32:load-cursor (cffi:null-pointer) win32:+idc-arrow+))
                                         (background (cffi:make-pointer (1+ win32:+color-window+)))
                                         (menu-name (cffi:null-pointer))
                                         (icon-sm (cffi:null-pointer))
                                         wndclass-name
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
  (let* ((wndclass-name (or wndclass-name
                            (format nil "Window[~A(~A);~A]"
                                    (let ((name (%exe-name)))
                                      (if (<= (length name) 128)
                                          name
                                          (subseq name 0 128)))
                                    (let ((name (bt:thread-name (bt:current-thread))))
                                      (if (<= (length name) 64)
                                          name
                                          (subseq name 0 64)))
                                    (%make-guid))))
         (wndclass-wrapper (make-instance 'wndclass-wrapper
                                          :style class-style :cls-extra cls-extra
                                          :wnd-extra wnd-extra :instance instance
                                          :icon icon :cursor cursor
                                          :background background :menu-name menu-name
                                          :icon-sm icon-sm :name wndclass-name
                                          :wndproc (cffi:callback %window-wndproc)))
         (success nil))
    (unwind-protect
         (let ((%*creating-window* obj))
           (setf (slot-value obj '%wndclass-wrapper) wndclass-wrapper)
           (let ((hwnd-wrapper (make-instance 'hwnd-wrapper :ex-style ex-style :wndclass wndclass-wrapper
                                                            :name name :style style
                                                            :x x :y y :width width :height height
                                                            :parent parent :menu menu :instance instance
                                                            :lparam lparam)))
             (unless (eq hwnd-wrapper (slot-value obj '%hwnd-wrapper))
               (dispose hwnd-wrapper)
               (error "Window initialization failed")))
           (setf success t))
      (unless success
        (slot-makunbound obj '%wndclass-wrapper)
        (dispose wndclass-wrapper)))))

(define-dispose (obj window)
  ;; Don't dispose wndclass - that'll be done by `window-manager',
  (when (slot-boundp obj '%hwnd-wrapper)
    (dispose (slot-value obj '%hwnd-wrapper))))

(defmethod hwnd ((obj window))
  (hwnd (slot-value obj '%hwnd-wrapper)))

(defmethod wndclass-name ((obj window))
  (wndclass-name (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-instance ((obj window))
  (wndclass-instance (slot-value obj '%wndclass-wrapper)))

(defmethod wndclass-atom ((obj window))
  (wndclass-atom (slot-value obj '%wndclass-wrapper)))
