(in-package #:winutil)

(defclass tray-icon-wrapper (disposable)
  ((%hwnd
    :type cffi:foreign-pointer
    :reader tray-icon-wrapper-hwnd)
   (%message-id
    :type (unsigned-byte 32)
    :initform (required-argument :message-id)
    :initarg :message-id
    :reader tray-icon-wrapper-message-id)
   (%id
    :type (unsigned-byte 32)
    :reader tray-icon-wrapper-id)
   (%tooltip
    :type (or null string)
    :initform nil
    :initarg :tooltip
    :reader tray-icon-wrapper-tooltip)))

(defvar %*hwnd-icons* (make-hash-table)
  "Maps a `hwnd' to its active icon ids as a list.
Used in order to obtain a unique ID for each.")

(defun %notify-icon (message icon)
  (cffi:with-foreign-object (nid 'win32:notify-icon-data)
    (%zero-memory nid 'win32:notify-icon-data)
    (cffi:with-foreign-slots ((win32:size win32:hwnd win32:id win32:flags
                                          win32:callback-message win32:icon win32:tip)
                              nid win32:notify-icon-data)
      (setf win32:size (cffi:foreign-type-size 'win32:notify-icon-data)
            win32:hwnd (tray-icon-wrapper-hwnd icon)
            win32:id (tray-icon-wrapper-id icon)
            win32:flags (logior (if (tray-icon-wrapper-tooltip icon) win32:+nif-tip+ 0)
                                win32:+nif-icon+ win32:+nif-message+)
            win32:callback-message (tray-icon-wrapper-message-id icon)
            win32:icon (win32:load-icon (cffi:null-pointer) win32:+idi-application+))
      (when (tray-icon-wrapper-tooltip icon)
        (lisp-to-tstring (tray-icon-wrapper-tooltip icon) win32:tip 128))
      (win32:shell-notify-icon message nid))))

(defmethod initialize-instance :after ((obj tray-icon-wrapper)
                                       &key
                                         (hwnd (required-argument :hwnd))
                                       &allow-other-keys)
  (with-slots (%hwnd %id) obj
    (setf %hwnd (hwnd hwnd))
    (let* ((key (cffi:pointer-address %hwnd))
           (existing-icons (gethash key %*hwnd-icons*)))
      (setf %id (if existing-icons (1+ (car existing-icons)) 0)
            (gethash key %*hwnd-icons*) (cons %id existing-icons))))
  (%notify-icon win32:+nim-add+ obj))

(define-dispose (obj tray-icon-wrapper)
  (%notify-icon win32:+nim-delete+ obj)
  (with-slots (%hwnd %id) obj
    (let* ((key (cffi:pointer-address %hwnd))
           (existing-icons (delete %id (gethash key %*hwnd-icons*))))
      (if existing-icons
          (setf (gethash key %*hwnd-icons*) existing-icons)
          (remhash key %*hwnd-icons*))))
  (slot-makunbound obj '%id))

(defmethod (setf tray-icon-wrapper-tooltip) (value (obj tray-icon-wrapper))
  (prog1 (setf (slot-value obj '%tooltip) value)
    (%notify-icon win32:+nim-modify+ obj)))
