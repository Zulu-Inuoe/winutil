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
  (values x y))

(define-setf-expander cursor-position ()
  (let ((x-var (gensym "X"))
        (y-var (gensym "Y")))
    (values ()
            ()
            `(,x-var ,y-var)
            `(set-cursor-position ,x-var ,y-var)
            `(values ,x-var ,y-var))))

(defun cursor-position* ()
  (multiple-value-bind (x y) (cursor-position)
    (cons x y)))

(defun (setf cursor-position*) (value)
  (set-cursor-position (car value) (cdr value))
  value)

(defun cursor-x ()
  (cffi:with-foreign-object (pt 'win32:point)
    (or (win32:get-cursor-pos pt)
        (win32-error))
    (cffi:foreign-slot-value pt 'win32:point 'win32:x)))

(defun cursor-y ()
  (cffi:with-foreign-object (pt 'win32:point)
    (or (win32:get-cursor-pos pt)
        (win32-error))
    (cffi:foreign-slot-value pt 'win32:point 'win32:y)))

(defun (setf cursor-x) (value)
  (set-cursor-position value (cursor-y))
  value)

(defun (setf cursor-y) (value)
  (set-cursor-position (cursor-x) value)
  value)

(deftype wparam ()
  "Valid value range for `win32:wparam'"
  `(unsigned-byte #.(* 8 (cffi:foreign-type-size 'win32:wparam))))

(defun wparam (value)
  "Coerces `value' into an `win32:wparam'"
  (etypecase value
    (wparam value)
    ((signed-byte #1=#.(* 8 (cffi:foreign-type-size 'win32:wparam)))
     (ldb (byte #1# 0) value))
    (cffi:foreign-pointer (cffi:pointer-address value))))

(deftype lparam ()
  "Value value range for `win32:lparam'"
  `(signed-byte #.(* 8 (cffi:foreign-type-size 'win32:lparam))))

(defun lparam (value)
  "Coerces `value' into an `win32:lparam'"
  (etypecase value
    (lparam value)
    ((unsigned-byte #1=#.(* 8 (cffi:foreign-type-size 'win32:lparam)))
     (logior value (- (mask-field (byte 1 #.(1- #1#)) value))))
    (cffi:foreign-pointer
     (let ((value (cffi:pointer-address value)))
       (logior value (- (mask-field (byte 1 #.(1- #1#)) value)))))))

(deftype lresult ()
  "Value value range for `win32:lresult'"
  `(signed-byte #.(* 8 (cffi:foreign-type-size 'win32:lresult))))

(defun lresult (value)
  "Coerces `value' into an `win32:lresult'"
  (etypecase value
    (lresult value)
    ((unsigned-byte #1=#.(* 8 (cffi:foreign-type-size 'win32:lresult)))
     (logior value (- (mask-field (byte 1 #.(1- #1#)) value))))
    (cffi:foreign-pointer
     (let ((value (cffi:pointer-address value)))
       (logior value (- (mask-field (byte 1 #.(1- #1#)) value)))))))

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
                (type wparam ,wparam)
                (type lparam ,lparam))
       ,@body)
     ;; There's no way to check if the callback is already defined
     ;; But `cffi:get-callback' will signal an error if it doesn't exist
     (handler-case (cffi:get-callback ',name)
       (error ()
         (cffi:defcallback (,name :convention :stdcall) win32:lresult
             ((,hwnd win32:hwnd) (,msg win32:uint) (,wparam win32:wparam) (,lparam win32:lparam))
           (prog (normal)
              (return (unwind-protect
                           (prog1 (prog ()
                                   retry
                                     (restart-case (let ((ret (,name ,hwnd ,msg ,wparam ,lparam)))
                                                     (unless (typep ret 'lresult)
                                                       (error 'type-error :datum ret :expected-type 'lresult))
                                                     (return ret))
                                       (retry-wndproc ()
                                         :report ,(format nil "Retry calling the wndproc (~A)." name)
                                         (go retry))
                                       (use-default-wndproc ()
                                         :report "Call `win32:def-window-proc' and return its value."
                                         (return (win32:def-window-proc ,hwnd ,msg ,wparam ,lparam)))
                                       (abort (&optional value)
                                         :report "Return abort and return a specified value"
                                         (return (or value 0)))))
                             (setf normal t))
                        (unless normal
                          (warn ,(format nil "Caught abnormal unwind from ~A" name))
                          (return 0))))))))
     ',name))

(defmacro defmsgproc (name (code wparam msg) &body body)
  "Utility Wrapper around `cffi:defcallback' for defining timerproc callbacks.
Defines a `cffi:callback' with appropriate signature, a well as a regular `defun'.
The callback shall not be redefined on repeated evaluation, but instead the `defun' will, allowing for better livecoding.

`name' - The name of the callback and function
`code'  - Symbol bound to the code parameter
`wparam' - Symbol bound to the wparam parameter
`msg' - Symbol bound to the msg parameter"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name (,code ,wparam ,msg)
       (declare (type (signed-byte #.(* 8 (cffi:foreign-type-size :int))) ,code)
                (type wparam ,wparam)
                (type lparam ,msg))
       ,@body)
     ;; There's no way to check if the callback is already defined
     ;; But `cffi:get-callback' will signal an error if it doesn't exist
     (handler-case (cffi:get-callback ',name)
       (error ()
         (cffi:defcallback (,name :convention :stdcall) win32:lresult
             ((,code :int) (,wparam win32:wparam) (,msg win32:lparam))
           (prog ()
            retry
              (restart-case (let ((ret (,name ,code ,wparam ,msg)))
                              (unless (typep ret 'lresult)
                                (error 'type-error :datum ret :expected-type 'lresult))
                              (return ret))
                (retry-msgproc ()
                  :report ,(format nil "Retry calling the msgproc (~A)." name)
                  (go retry))
                (call-next-hook ()
                  :report "Invoke `win32:call-next-hook-ex'"
                  (return (win32:call-next-hook-ex (cffi:null-pointer) ,code ,wparam ,msg))))))))
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
