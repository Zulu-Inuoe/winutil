(in-package #:winutil)

(defgeneric hcursor (obj)
  (:documentation "Coerce `obj' into a `win32:hcursor'")
  (:method (obj)
    obj))
