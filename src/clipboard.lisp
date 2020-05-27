(in-package #:winutil)

(defmacro with-open-clipboard ((&optional (owner '(cffi:null-pointer))) &body body)
  `(progn
     (or (win32:open-clipboard ,owner) (win32-error))
     (unwind-protect (progn ,@body)
       (or (win32:close-clipboard) (win32-error)))))

(defun get-clipboard-text (&optional (owner (cffi:null-pointer)))
  (with-open-clipboard ((hwnd owner))
    (let ((hglb (win32:get-clipboard-data win32:+cf-unicodetext+)))
      (when (cffi:null-pointer-p hglb)
        (return-from get-clipboard-text ""))
      (let ((tstr (not-null-or-error (win32:global-lock hglb))))
        (unwind-protect (tstring-to-lisp tstr)
          (win32:global-unlock hglb)
          (check-last-error))))))

(defun set-clipboard-text (string &key (owner (cffi:null-pointer)) (start 0) end)
  (cffi:with-foreign-string ((str size) string :encoding win32:+win32-string-encoding+ :start start :end end)
    (let ((hglb (not-null-or-error (win32:global-alloc win32:+gmem-moveable+ size)))
          success)
      (unwind-protect
           (progn
             (let ((tstr (not-null-or-error (win32:global-lock hglb))))
               (unwind-protect (%memcpy tstr str size)
                 (win32:global-unlock hglb)
                 (check-last-error)))
             (with-open-clipboard ((hwnd owner))
               (or (win32:empty-clipboard) (win32-error))
               (not-null-or-error (win32:set-clipboard-data win32:+cf-unicodetext+ hglb)))
             (setf success t))
        (or success (cffi:null-pointer-p (win32:global-free hglb)) (win32-error)))))
  (values))

(defun clear-clipboard (&optional (owner (cffi:null-pointer)))
  (with-open-clipboard ((hwnd owner))
    (or (win32:empty-clipboard) (win32-error)))
  (values))
