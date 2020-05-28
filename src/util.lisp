(in-package #:winutil)

(defun lisp-to-tstring (string tchar-buf tchar-count &key (start 0) end (offset 0))
  "Convert from a lisp string to a `win32:lptstr' and write it into `tchar-buf'"
  (cffi:lisp-string-to-foreign
   string
   tchar-buf
   (* tchar-count (cffi:foreign-type-size 'win32:tchar))
   :start start
   :end end
   :offset (* offset (cffi:foreign-type-size 'win32:tchar))
   :encoding win32:+win32-string-encoding+))

(defun tstring-to-lisp (tchar-buf &key (offset 0) count)
  "Convert from a foreign `win32:lptstr' to a lisp string"
  (values
   (cffi:foreign-string-to-lisp
    tchar-buf
    :offset (* offset (cffi:foreign-type-size 'win32:tchar))
    :count (and count (* count (cffi:foreign-type-size 'win32:tchar)))
    :encoding win32:+win32-string-encoding+)))

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
              (return (tstring-to-lisp buf :count res)))))
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
         (return-from %module-path-string (tstring-to-lisp buffer :count res))))))
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

(defun %memcpy (dst src size)
  (dotimes (i size)
    (setf (cffi:mem-aref dst :uint8 i) (cffi:mem-aref src :uint8 i))))

(defun %memset (buf value size)
  (dotimes (i size)
    (setf (cffi:mem-aref buf :uint8 i) value)))
