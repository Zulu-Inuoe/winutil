(in-package #:winutil)

(defclass wndclass-wrapper (disposable)
  ((%name
    :type string
    :initarg :name
    :reader wndclass-wrapper-name)
   (%instance
    :type cffi:foreign-pointer
    :initarg :instance
    :reader wndclass-wrapper-instance)
   (%icon
    :initarg :icon)
   (%cursor
    :initarg :cursor)
   (%icon-sm
    :initarg :icon-sm)
   (%class-atom
    :type cffi:foreign-pointer
    :reader wndclass-wrapper-atom))
  (:default-initargs
   :name (format nil "Wndclass-Wrapper[~A(~A);~A]"
                 (lisp-implementation-type)
                 (lisp-implementation-version)
                 (%make-guid))
   :instance (win32:get-module-handle (cffi:null-pointer))
   :icon (cffi:null-pointer)
   :cursor (win32:load-cursor (cffi:null-pointer) win32:+idc-arrow+)
   :icon-sm (cffi:null-pointer)))

(defmethod initialize-instance :after ((obj wndclass-wrapper)
                                       &key
                                         (class-style 0)
                                         (wndproc (cffi:foreign-symbol-pointer "DefWindowProcW" :library 'win32:user32))
                                         (cls-extra 0)
                                         (wnd-extra 0)
                                         (background (cffi:null-pointer))
                                         (menu-name (cffi:null-pointer))
                                       &allow-other-keys)
  (cffi:with-foreign-object (class 'win32:wndclassex)
    (win32:zero-memory class (cffi:foreign-type-size 'win32:wndclassex))
    (cffi:with-foreign-slots ((win32:size win32:style win32:wndproc
                                          win32:cls-extra win32:wnd-extra
                                          win32:instance
                                          win32:icon win32:cursor
                                          win32:background win32:menu-name
                                          win32:wndclass-name win32:icon-sm)
                              class win32:wndclassex)
      (with-slots (%icon %cursor %icon-sm) obj
        (setf win32:size (cffi:foreign-type-size 'win32:wndclassex)
              win32:style class-style
              win32:wndproc wndproc
              win32:cls-extra cls-extra
              win32:wnd-extra wnd-extra
              win32:instance (wndclass-wrapper-instance obj)
              win32:icon (hicon %icon)
              win32:cursor (hcursor %cursor)
              win32:background background
              win32:menu-name menu-name
              win32:wndclass-name (wndclass-wrapper-name obj)
              win32:icon-sm (hicon %icon-sm))))
    (let ((class-atom (win32:register-class-ex class)))
      (or (/= class-atom 0) (win32-error))
      (setf (slot-value obj '%class-atom) (cffi:make-pointer class-atom)))))

(define-dispose (obj wndclass-wrapper)
  (or (win32:unregister-class (wndclass-wrapper-atom obj) (wndclass-wrapper-instance obj))
      (win32-error))
  (slot-makunbound obj '%class-atom))

(defmethod wndclass-name ((obj wndclass-wrapper))
  (wndclass-wrapper-name obj))

(defmethod wndclass-instance ((obj wndclass-wrapper))
  (wndclass-wrapper-instance obj))

(defmethod wndclass-atom ((obj wndclass-wrapper))
  (wndclass-wrapper-atom obj))
