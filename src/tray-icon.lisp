(in-package #:com.inuoe.winutil)

(defconstant %+tray-icon-message+ (+ win32:+wm-user+ 0)
  "Windows Message ID for tray icon events.")

(defclass tray-icon-window (window)
  ((%existing-icons
    :type list
    :initform nil)
   (%taskbar-created-message
    :type (unsigned-byte 32)
    :initform (win32:register-window-message "TaskbarCreated")))
  (:default-initargs
   :ex-style 0
   :style win32:+mf-popup+
   :width 0 :height 0))

(declaim (type (or null tray-icon-window) %*tray-icon-window*))
(defvar %*tray-icon-window* nil
  "The current thread's `tray-icon-window' to which tray icons are attached.")

(pushnew '(%*tray-icon-window* . nil) bt:*default-special-bindings* :test #'equal)

(when (find-package #1='#:slynk)
  (pushnew '(%*tray-icon-window* . nil) (symbol-value (find-symbol (string '#:*default-worker-thread-bindings*) #1#)) :test #'equal))

(defclass tray-icon (d:disposable)
  ((%id
    :type (unsigned-byte 32))
   (%tooltip
    :type (or null string)
    :initarg :tooltip
    :reader tray-icon-tooltip)
   (%icon
    :type t
    :initarg :icon
    :reader tray-icon-icon))
  (:default-initargs
   :tooltip nil
   :icon nil))

(defgeneric tray-event (icon lparam)
  (:documentation "Event called on `tray-icon' when a tray event occurs.")
  (:method ((tray-icon tray-icon) lparam)
    (declare (ignore tray-icon lparam))
    (values)))

(defmethod call-wndproc ((tray-icon-window tray-icon-window) msg wparam lparam)
  "wndproc for calling into `tray-event'"
  (with-slots (%existing-icons %taskbar-created-message) tray-icon-window
    (cond
      ((= msg %+tray-icon-message+)
       (let ((tray-icon-cons (assoc wparam %existing-icons)))
         (or tray-icon-cons (error "No such icon '~A'" wparam))
         (tray-event (cdr tray-icon-cons) lparam))
       0)
      ((= msg %taskbar-created-message)
       (loop
         :for (id . icon) :in %existing-icons
         :do (%tray-icon-notify icon win32:+nim-add+))
       0)
      (t
       (call-next-method)))))

(defun %tray-icon-notify (tray-icon message)
  "Wrapper around `win32:shell-notify-icon' which notifies `message' using data from `tray-icon'"
  (let ((tooltip (tray-icon-tooltip tray-icon))
        (icon (hicon
               (or (tray-icon-icon tray-icon)
                   (win32:load-icon (cffi:null-pointer) win32:+idi-application+)))))
    (cffi:with-foreign-object (nid 'win32:notify-icon-data)
      (win32:zero-memory nid (cffi:foreign-type-size 'win32:notify-icon-data))
      (cffi:with-foreign-slots ((win32:size win32:hwnd win32:id win32:flags
                                            win32:callback-message win32:icon win32:tip)
                                nid win32:notify-icon-data)
        (setf win32:size (cffi:foreign-type-size 'win32:notify-icon-data)
              win32:hwnd (hwnd %*tray-icon-window*)
              win32:id (slot-value tray-icon '%id)
              win32:flags (logior (if tooltip win32:+nif-tip+ 0)
                                  win32:+nif-icon+ win32:+nif-message+)
              win32:callback-message %+tray-icon-message+
              win32:icon icon)
        (when tooltip
          (lisp-to-tstring tooltip win32:tip 128))
        (or (win32:shell-notify-icon message nid)
            (win32-error))))))

(defmethod initialize-instance :after ((tray-icon tray-icon) &key &allow-other-keys)
  (unless %*tray-icon-window*
    (let ((name (format nil "Tray[~A(~A);~A]"
                        (lisp-implementation-type)
                        (lisp-implementation-version)
                        (%make-guid))))
      (setf %*tray-icon-window* (make-instance 'tray-icon-window :wndclass-name name :name name))))
  (with-slots (%id %tooltip) tray-icon
    (with-slots (%existing-icons) %*tray-icon-window*
      (setf %id (if %existing-icons (1+ (caar %existing-icons)) 0))
      (push (cons %id tray-icon) %existing-icons)))

  (%tray-icon-notify tray-icon win32:+nim-add+))

(d:define-dispose (tray-icon tray-icon)
  (%tray-icon-notify tray-icon win32:+nim-delete+)
  (with-slots (%existing-icons) %*tray-icon-window*
    (with-slots (%id) tray-icon
      (a:deletef %existing-icons %id :key #'car)))
  (slot-makunbound tray-icon '%id))

(defmethod hwnd ((tray-icon tray-icon))
  (hwnd %*tray-icon-window*))

(defgeneric (setf tray-icon-tooltip) (value tray-icon)
  (:method (value (tray-icon tray-icon))
    (with-slots (%id %tooltip) tray-icon
      (prog1 (setf %tooltip value)
        (%tray-icon-notify tray-icon win32:+nim-modify+)))))

(defgeneric (setf tray-icon-icon) (value tray-icon)
  (:method (value (tray-icon tray-icon))
    (with-slots (%id %icon) tray-icon
      (prog1 (setf %icon value)
        (%tray-icon-notify tray-icon win32:+nim-modify+)))))
