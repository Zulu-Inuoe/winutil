(in-package #:winutil)

(defclass message-only-hwnd (window)
  ()
  (:default-initargs
   :cursor (cffi:null-pointer)
   :background (cffi:null-pointer)
   :ex-style 0
   :name (format nil "MessageOnlyHwnd[~A(~A);~A]"
                 (lisp-implementation-type)
                 (lisp-implementation-version)
                 (%make-guid))
   :x 0 :y 0 :width 0 :height 0
   :parent (cffi:make-pointer win32:+hwnd-message+)))
