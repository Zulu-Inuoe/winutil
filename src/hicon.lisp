(in-package #:winutil)

(defgeneric hicon (obj)
  (:documentation "Coerce `obj' into a `win32:hicon'")
  (:method (obj)
    obj)
  (:method ((obj null))
    (cffi:null-pointer)))
