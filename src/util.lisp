(in-package #:winutil)

(defun lisp-to-tstring (string tchar-buf tchar-count)
  "Convert from a lisp string to a `win32:lptstr' and write it into `tchar-buf'"
  (cffi:lisp-string-to-foreign
   string
   tchar-buf
   (* tchar-count (cffi:foreign-type-size 'win32:tchar))
   :encoding win32:+win32-string-encoding+))

(defun tstring-to-lisp (tchar-buf tchar-count)
  "Convert from a foreign `win32:lptstr' to a lisp string"
  (values
   (cffi:foreign-string-to-lisp
    tchar-buf
    :count (* tchar-count (cffi:foreign-type-size 'win32:tchar))
    :encoding win32:+win32-string-encoding+)))

(defun error-code-string (code)
  "Retrieve the string representation for `code'.
Returns two values:
1. On success, the error string, or `nil' if there was an error retrieving it
2. On success, `nil'. Otherwise an error code indicating the operation failed."
  (cffi:with-foreign-object (result 'win32:lptstr)
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
              (let ((tstr (cffi:mem-ref result '(:pointer (:pointer win32:tchar)))))
                (tstring-to-lisp tstr
                                 ;; Don't include the CRLF Windows puts at the end..
                                 (if (and (> tchar-count 1)
                                          (= (cffi:mem-aref tstr 'win32:tchar (- tchar-count 1))
                                             (char-code #\LineFeed))
                                          (= (cffi:mem-aref tstr 'win32:tchar (- tchar-count 2))
                                             (char-code #\Return)))
                                     (- tchar-count 2)
                                     tchar-count)))
           (win32:local-free result)))
       (when (zerop tchar-count)
         last-error)))))

(define-condition win32-error ()
  ((%code
    :type integer
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
  (error 'win32-error :code code :string (error-code-string code)))

(defun %module-path-string-dyn (&optional
                                  (module (cffi:null-pointer))
                                  (start-size 512))
  "As `%exe-path-string' but always using heap memory.
Meant as a fallback."
  (let* ((size start-size)
         (buf (cffi:foreign-alloc 'win32:tchar :count size)))
    (unwind-protect
         (loop
           :for res := (win32:get-module-file-name module buf size)
           :for last-error := (win32:get-last-error)
           :do
           (cond
             ;; On Windows XP we can't tell between our size
             ;; being exactly right, or if it got truncated
             ((= res size)
              (cffi:foreign-free buf)
              (setf buf (cffi:null-pointer)
                    size (* size 2)
                    buf (cffi:foreign-alloc 'win32:tchar :count size)))
             ((zerop res)
              (win32-error last-error))
             (t
              (return (tstring-to-lisp buf res)))))
      (unless (cffi:null-pointer-p buf)
        (cffi:foreign-free buf)))))

(defun %module-path-string (&optional (module (cffi:null-pointer))
                            &aux (buffer-size 512))
  "Returns the path to the current given `win32:hmodule' on disk as a string."
  ;; Try first with a static sized buffer
  (cffi:with-foreign-object (buffer 'win32:tchar buffer-size)
    (let ((res (win32:get-module-file-name module buffer buffer-size))
          (last-error (win32:get-last-error)))
      (cond
        ((zerop res)
         (win32-error last-error))
        ((/= res buffer-size)
         (return-from %module-path-string (tstring-to-lisp buffer res))))))
  ;;Fallback on dynamic alloc
  (%module-path-string-dyn module (* 2 buffer-size)))

(defun exe-pathname ()
  "Get the pathname to the current executable."
  (uiop:parse-native-namestring (%module-path-string)))

(defun exe-dir-pathname ()
  "Get the directory the current exectable resides in.
Note: This is not the current working directory."
  (uiop:pathname-directory-pathname (exe-pathname)))

(defun processor-count ()
  "Get the number of logical processors on this machine."
  (cffi:with-foreign-object (system-info 'win32:system-info)
    (win32:get-system-info system-info)
    (cffi:foreign-slot-value system-info 'win32:system-info 'win32:number-of-processors)))

(defun %zero-memory (ptr type)
  (dotimes (i (cffi:foreign-type-size type))
    (setf (cffi:mem-ref ptr :uint8 i) 0)))

