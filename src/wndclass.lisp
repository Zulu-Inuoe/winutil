(in-package #:com.inuoe.winutil)

(defgeneric wndclass-name (obj)
  (:documentation "Get wndclass-name from `obj'"))

(defgeneric wndclass-instance (obj)
  (:documentation "Get a suitable `hinstance' from `obj' for `win32:create-window-ex'"))

(defgeneric wndclass-atom (obj)
  (:documentation "Get an atom or class name from `obj' for `win32:create-window-ex'")
  (:method (obj)
    (wndclass-name obj)))

(defmethod wndclass-name ((obj string))
  obj)

(defmethod wndclass-instance ((obj string))
  (win32:get-module-handle (cffi:null-pointer)))

(defmethod wndclass-atom ((obj string))
  obj)
