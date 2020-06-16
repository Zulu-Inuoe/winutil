(defpackage #:com.inuoe.winutil.examples.simple-program
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:win32)
  (:export
   #:main))

(in-package #:com.inuoe.winutil.examples.simple-program)

(defun main (&optional argv)
  (declare (ignore argv))
  (win32:message-box (cffi:null-pointer) "First Program" "First" win32:+mb-ok+)
  0)
