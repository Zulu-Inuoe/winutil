(defpackage #:com.inuoe.winutil.examples.simple-program
  (:use #:cl)
  (:export
   #:simple-program))

(in-package #:com.inuoe.winutil.examples.simple-program)

(defun simple-program ()
  (win32:message-box (cffi:null-pointer) "First Program" "First" win32:+mb-ok+)
  0)
