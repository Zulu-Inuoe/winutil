(in-package #:winutil)

(defgeneric wndclass-name (obj)
  (:documentation "Get a suitable wndclass-name from `obj' for `win32:create-window-ex'"))

(defgeneric wndclass-instance (obj)
  (:documentation "Get a suitable `hinstance' from `obj' for `win32:create-window-ex'"))

(defmethod wndclass-name ((obj string))
  obj)

(defmethod wndclass-instance ((obj string))
  (win32:get-module-handle (cffi:null-pointer)))
