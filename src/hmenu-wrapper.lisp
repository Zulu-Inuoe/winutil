(in-package #:com.inuoe.winutil)

(defclass hmenu-wrapper (d:disposable)
  ((%hmenu
    :type cffi:foreign-pointer
    :initform (win32:create-menu)
    :initarg :hmenu
    :reader hmenu-wrapper-hmenu))
  (:documentation
   "Wrapper around an `hmenu'"))

(d:define-dispose (obj hmenu-wrapper)
  (win32:destroy-menu (hmenu-wrapper-hmenu obj))
  (slot-makunbound obj '%hmenu))

(defmethod hmenu ((obj hmenu-wrapper))
  (hmenu-wrapper-hmenu obj))
