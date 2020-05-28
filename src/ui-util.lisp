(in-package #:winutil)

(defun %make-guid ()
  "Create a randomly-generated 128-bit GUID in the form of a {} wrapped, 32-digit, hyphen-separated, hexadecimal string.
  Example: {A3C78807-EA6D-A4AE-1CCA-797A6BF88C31}"
  (let ((value (random (ash 1 128))))
    (format nil "{~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X}"
            (ldb (byte 32 96) value)
            (ldb (byte 16 80) value)
            (ldb (byte 16 64) value)
            (ldb (byte 16 48) value)
            (ldb (byte 48 0) value))))

(defun clip-cursor (left top right bottom)
  (cffi:with-foreign-object (r 'win32:rect)
    (setf (cffi:foreign-slot-value r 'win32:rect 'win32:left) left
          (cffi:foreign-slot-value r 'win32:rect 'win32:top) top
          (cffi:foreign-slot-value r 'win32:rect 'win32:right) right
          (cffi:foreign-slot-value r 'win32:rect 'win32:bottom) bottom)
    (or (win32:clip-cursor r)
        (win32-error)))
  (values))

(defun unclip-cursor ()
  (or (win32:clip-cursor (cffi:null-pointer))
      (win32-error))
  (values))

(defun cursor-position ()
  (cffi:with-foreign-object (pt 'win32:point)
    (or (win32:get-cursor-pos pt)
        (win32-error))
    (values
     (cffi:foreign-slot-value pt 'win32:point 'win32:x)
     (cffi:foreign-slot-value pt 'win32:point 'win32:y))))

(defun set-cursor-position (x y)
  (or (win32:set-cursor-pos x y)
      (win32-error))
  (values))

(define-setf-expander cursor-position ()
  (let ((x-var (gensym "X"))
        (y-var (gensym "Y")))
    (values ()
            ()
            `(,x-var ,y-var)
            `(set-cursor-position (or ,x-var 0) (or ,y-var 0))
            `(cursor-position))))

(defun cursor-position* ()
  (cffi:with-foreign-object (pt 'win32:point)
    (or (win32:get-cursor-pos pt)
        (win32-error))
    (cons
     (cffi:foreign-slot-value pt 'win32:point 'win32:x)
     (cffi:foreign-slot-value pt 'win32:point 'win32:y))))

(defun (setf cursor-position*) (value)
  (or (win32:set-cursor-pos (car value) (cdr value))
      (win32-error))
  value)

(defun window-text (hwnd &aux (hwnd (hwnd hwnd)))
  (let ((len (1+ (win32:get-window-text-length hwnd))))
    (cffi:with-foreign-objects ((buf 'win32:tchar len))
      (win32:get-window-text hwnd buf len)
      (tstring-to-lisp buf))))

(defun (setf window-text) (value hwnd &aux (hwnd (hwnd hwnd)))
  (win32:set-window-text hwnd value)
  value)

(defmacro defwndproc (name (hwnd msg wparam lparam) &body body)
  "Utility Wrapper around `cffi:defcallback' for defining wndproc callbacks.
Defines a `cffi:callback' with appropriate signature, a well as a regular `defun'.
The callback shall not be redefined on repeated evaluation, but instead the `defun' will, allowing for better livecoding.

`name' - The name of the callback and function
`hwnd' - Symbol bound to the window handle
`msg' - Symbol bound to the msg parameter
`wparam' - Symbol bound to the wparam parameter
`lparam' - Symbol bound to the lparam parameter"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name (,hwnd ,msg ,wparam ,lparam)
       (declare (type cffi:foreign-pointer ,hwnd)
                (type (unsigned-byte 32) ,msg)
                (type (unsigned-byte #.(* (cffi:foreign-type-size :pointer) 8)) ,wparam)
                (type (signed-byte #.(* (cffi:foreign-type-size :pointer) 8)) ,lparam))
       ,@body)
     ;; There's no way to check if the callback is already defined
     ;; But `cffi:get-callback' will signal an error if it doesn't exist
     (handler-case
         (cffi:get-callback ',name)
       (error ()
         (cffi:defcallback (,name :convention :stdcall) win32:lresult
             ((,hwnd win32:hwnd) (,msg win32:uint) (,wparam win32:wparam) (,lparam win32:lparam))
           (prog ()
            retry
              (restart-case
                  (let ((ret (,name ,hwnd ,msg ,wparam ,lparam)))
                    (unless (typep ret '(signed-byte #.(* (cffi:foreign-type-size :pointer) 8)))
                      (error 'type-error :datum ret :expected-type '(signed-byte #.(* (cffi:foreign-type-size :pointer) 8))))
                    (return ret))
                (retry-wndproc ()
                  :report ,(format nil "Retry calling the wndproc (~A)." name)
                  (go retry))
                (def-window-proc ()
                  :report "Call `win32:def-window-proc' and return its value."
                  (return (win32:def-window-proc ,hwnd ,msg ,wparam ,lparam))))))))
     ',name))

(defun message-pump ()
  "Run a typical win32 message pump until quit."
  (cffi:with-foreign-object (msg 'win32:msg)
    (loop
      :for res := (win32:get-message msg (cffi:null-pointer) 0 0)
      :until (zerop res)
      :do
         (cond
           ((= res -1)
            (win32-error))
           (t
            (win32:translate-message msg)
            (win32:dispatch-message msg))))))

(defun wparam (value)
  "Coerces `value' into an `win32:wparam'"
  (etypecase value
    ((unsigned-byte #1=#.(* 8 (cffi:foreign-type-size 'win32:wparam))) value)
    ((signed-byte #1#) (ldb (byte #1# 0) value))
    (cffi:foreign-pointer (cffi:pointer-address value))))

(defun lparam (value)
  "Coerces `value' into an `win32:lparam'"
  (etypecase value
    ((signed-byte #1=#.(* 8 (cffi:foreign-type-size 'win32:lparam))) value)
    ((unsigned-byte #1#) (logior value (- (mask-field (byte 1 #.(1- #1#)) value))))
    (cffi:foreign-pointer (lparam (cffi:pointer-address value)))))
