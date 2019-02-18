(in-package #:winutil)

(defmacro with-open-clipboard ((&optional (owner '(cffi:null-pointer))) &body body)
  `(progn
     (unless (win32:open-clipboard ,owner)
       (win32-error))
     (unwind-protect
          (progn ,@body)
       (win32:close-clipboard))))

(defun get-clipboard-text (&optional (owner (cffi:null-pointer)))
  (with-open-clipboard ((hwnd owner))
    (let ((data (win32:get-clipboard-data win32:+cf-unicodetext+)))
      (when (cffi:null-pointer-p data)
        (return-from get-clipboard-text ""))
      (win32:global-lock data)
      (unwind-protect
           (tstring-to-lisp data)
        (win32:global-unlock data)))))

(defun set-clipboard-text (string &key (owner (cffi:null-pointer)) (start 0) end)
  (cffi:with-foreign-string ((str size) string :encoding win32:+win32-string-encoding+ :start start :end end)
    (let ((hmem (win32:global-alloc win32:+gmem-moveable+ size))
          success)
      (unwind-protect
           (progn
             (%memcpy (win32:global-lock hmem) str size)
             (win32:global-unlock hmem)
             (with-open-clipboard ((hwnd owner))
               (win32:empty-clipboard)
               (when (cffi:null-pointer-p (win32:set-clipboard-data win32:+cf-unicodetext+ hmem))
                 (win32-error)))
             (setf success t))
        (unless success
          (win32:global-free hmem)))))
  (values))

(defun clear-clipboard (&optional (owner (cffi:null-pointer)))
  (with-open-clipboard ((hwnd owner))
    (win32:empty-clipboard))
  (values))
