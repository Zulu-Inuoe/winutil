(in-package #:winutil)

(defun error-code-string (code)
  "Retrieve the string representation for `code'.
Returns two values:
1. On success, the error string, or `nil' if there was an error retrieving it
2. On success, `nil'. Otherwise an error code indicating the operation failed."
  (cffi:with-foreign-object (result '(:pointer win32:tchar))
    (let ((tchar-count
            (win32:format-message (logior win32:+format-message-allocate-buffer+
                                          win32:+format-message-ignore-inserts+
                                          win32:+format-message-from-system+)
                                  (cffi:null-pointer)
                                  code
                                  (win32:make-lang-id win32:+lang-neutral+ win32:+sublang-default+)
                                  result
                                  0
                                  (cffi:null-pointer)))
          (last-error (win32:get-last-error)))
      (values
       (unless (zerop tchar-count)
         (unwind-protect
              (let ((tstr (cffi:mem-ref result '(:pointer win32:tchar))))
                ;; Cut LF
                (when (and (> tchar-count 0)
                           (= (cffi:mem-aref tstr 'win32:tchar (1- tchar-count)) 10))
                  (decf tchar-count)
                  ;; Cut CR
                  (when (and (> tchar-count 0)
                             (= (cffi:mem-aref tstr 'win32:tchar (1- tchar-count)) 13))
                    (decf tchar-count)))
                (tstring-to-lisp tstr :count tchar-count))
           (win32:local-free (cffi:mem-ref result '(:pointer win32:tchar)))))
       (when (zerop tchar-count)
         last-error)))))

(define-condition win32-error (error)
  ((%code
    :type (unsigned-byte 32)
    :initarg :code
    :initform (required-argument :code)
    :reader win32-error-code)
   (%string
    :type (or null string)
    :initarg :string
    :initform (required-argument :string)
    :reader win32-error-string))
  (:report (lambda (c stream)
             (format stream "Win32 error ~D~:* (0x~X): \"~:[<unavailable>~;~:*~A~]\"" (win32-error-code c) (win32-error-string c)))))

(defun win32-error (&optional (code (win32:get-last-error)))
  "Signals an error of type `win32-error' using `code' as the error code"
  (check-type code (or (unsigned-byte 32) (signed-byte 32)))
  (let ((unsigned-code (ldb (byte 32 0) code)))
    (error 'win32-error :code unsigned-code :string (error-code-string code))))

(defun check-last-error (&optional (code (win32:get-last-error)))
  "Signals an error if `code' indicates a failure result (severity bit set)
 Otherwise returns `code'"
  (when (logbitp 31 code)
    (win32-error code)))

(defun not-null-or-error (value &optional (code (win32:get-last-error)))
  "Signals an error of type `win32-error' using `code' as the error code if `value' is `cffi:null-pointer-p'
 Otherwise returns `value'"
  (if (cffi:null-pointer-p value)
      (win32-error code)
      value))
