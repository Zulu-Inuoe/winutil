(in-package #:com.inuoe.winutil)

(defmacro defhookfn (name (ncode wparam lparam) &body body)
  "Utility Wrapper around `cffi:defcallback' for defining hook callbacks.
Defines a `cffi:callback' with appropriate signature, a well as a
regular `defun'.  The callback shall not be redefined on repeated
evaluation, but instead the `defun' will, allowing for better
livecoding.

`name' - The name of the callback and function
`ncode' - Symbol bound to the ncode parameter
`wparam' - Symbol bound to the wparam parameter
`lparam' - Symbol bound to the lparam parameter
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name (,ncode ,wparam ,lparam)
       (declare (type (signed-byte #.(* 8 (cffi:foreign-type-size :int))) ,ncode)
                (type wparam ,wparam)
                (type lparam ,lparam)
                (ignorable ,ncode ,wparam ,lparam))
       ,@body)
     ;; There's no way to check if the callback is already defined
     ;; But `cffi:get-callback' will signal an error if it doesn't exist
     (handler-case
         (cffi:get-callback ',name)
       (error ()
         (cffi:defcallback (,name :convention :stdcall) win32:lresult
             ((,ncode :int)
              (,wparam win32:wparam) (,lparam win32:lparam))
           (prog ()
            retry
              (restart-case
                  (let ((ret (,name ,ncode ,wparam ,lparam)))
                    (unless (typep ret 'lresult)
                      (error 'type-error :datum ret :expected-type 'lresult))
                    (return ret))
                (retry-proc ()
                  :report ,(format nil "Retry calling the ll-keyboard-proc (~A)." name)
                  (go retry))
                (call-next-hook ()
                  :report "Invoke `win32:call-next-hook-ex'"
                  (return (win32:call-next-hook-ex (cffi:null-pointer) ,ncode ,wparam ,lparam))))))))
     ',name))
