(in-package #:winutil)

(defgeneric hwnd (obj)
  (:documentation "Coerce `obj' into an `hwnd'.")
  (:method (obj) obj)
  (:method ((obj integer))
    (cffi:make-pointer obj)))

(defun hwnd-x (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (cffi:foreign-slot-value r 'win32:rect 'win32:left)))

(defun (setf hwnd-x) (value hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (win32:set-window-pos
     (hwnd hwnd) (cffi:null-pointer)
     value (cffi:foreign-slot-value r 'win32:rect 'win32:top)
     0 0
     (logior win32:+swp-nosize+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+))))

(defun hwnd-y (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (cffi:foreign-slot-value r 'win32:rect 'win32:top)))

(defun (setf hwnd-y) (value hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (win32:set-window-pos
     (hwnd hwnd) (cffi:null-pointer)
     (cffi:foreign-slot-value r 'win32:rect 'win32:left) value
     0 0
     (logior win32:+swp-nosize+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+))))

(defun hwnd-pos (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (values
     (cffi:foreign-slot-value r 'win32:rect 'win32:left)
     (cffi:foreign-slot-value r 'win32:rect 'win32:top))))

(defun hwnd-pos* (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (cons
     (cffi:foreign-slot-value r 'win32:rect 'win32:left)
     (cffi:foreign-slot-value r 'win32:rect 'win32:top))))

(defun (setf hwnd-pos*) (value hwnd)
  (win32:set-window-pos
   (hwnd hwnd) (cffi:null-pointer)
   (car value) (cdr value)
   0 0
   (logior win32:+swp-nosize+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+)))

(defun hwnd-width (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (- (cffi:foreign-slot-value r 'win32:rect 'win32:right)
       (cffi:foreign-slot-value r 'win32:rect 'win32:left))))

(defun (setf hwnd-width) (value hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (win32:set-window-pos
     (hwnd hwnd) (cffi:null-pointer)
     0 0
     value (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom) (cffi:foreign-slot-value r 'win32:rect 'win32:top))
     (logior win32:+swp-nomove+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+))))

(defun hwnd-height (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom)
       (cffi:foreign-slot-value r 'win32:rect 'win32:top))))

(defun (setf hwnd-height) (value hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (win32:set-window-pos
     (hwnd hwnd) (cffi:null-pointer)
     0 0
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:right) (cffi:foreign-slot-value r 'win32:rect 'win32:left)) value
     (logior win32:+swp-nomove+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+))))

(defun hwnd-dimensions (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (values
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:right)
       (cffi:foreign-slot-value r 'win32:rect 'win32:left))
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom)
        (cffi:foreign-slot-value r 'win32:rect 'win32:top)))))

(defun hwnd-dimensions* (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (win32:get-window-rect (hwnd hwnd) r)
    (cons
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:right)
        (cffi:foreign-slot-value r 'win32:rect 'win32:left))
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom)
        (cffi:foreign-slot-value r 'win32:rect 'win32:top)))))

(defun (setf hwnd-dimensions*) (value hwnd)
  (win32:set-window-pos
   (hwnd hwnd) (cffi:null-pointer)
   0 0
   (car value) (cdr value)
   (logior win32:+swp-nomove+ win32:+swp-nozorder+ win32:+swp-noactivate+ win32:+swp-noownerzorder+)))

(defun hwnd-text (hwnd &aux (hwnd hwnd))
  (let ((len (win32:get-window-text-length hwnd)))
    (cond
      ((zerop len) "")
      (t
       (cffi:with-foreign-object (buf 'win32:tchar (1+ len))
         (let ((res (win32:get-window-text hwnd buf (1+ len)))
               (last-error (win32:get-last-error)))
           (when (zerop len)
             (win32-error last-error))
           (tstring-to-lisp buf :count res)))))))

(declaim (type (function (cffi:foreign-pointer (signed-byte #.(* (cffi:foreign-type-size :pointer) 8))) (values boolean &rest t))
               %*map-windows-fn*))
(defvar %*map-windows-fn*)

(cffi:defcallback (%map-windows-proc :convention :stdcall) win32:bool
    ((hwnd win32:hwnd) (lparam win32:lparam))
  "Helper function for `win32:enum-windows' and the like.
Invokes `%*make-windows-fn*' on each `hwnd' and `lparam', and returns its result."
  (funcall %*map-windows-fn* hwnd lparam))

(defun list-windows ()
  "Lists all the toplevel Windows on the system."
  (let* ((res ())
         (%*map-windows-fn* (lambda (hwnd lparam)
                              (declare (ignore lparam))
                              (push hwnd res)
                              t)))
    ;; On Windows XP there's a memory fault that is trapped
    ;; by SBCL at the end of `win32:enum-windows'
    #+sbcl
    (if (win32:is-windows-vista-or-greater)
        (win32:enum-windows (cffi:callback %map-windows-proc) 0)
        (ignore-errors (win32:enum-windows (cffi:callback %map-windows-proc) 0)))
    #-sbcl
    (win32:enum-windows (cffi:callback %map-windows-proc) 0)
    res))

(defun list-child-windows (hwnd)
  "List all the child windows of `hwnd'"
  (let* ((res ())
         (%*map-windows-fn* (lambda (hwnd lparam)
                              (declare (ignore lparam))
                              (push hwnd res)
                              t)))
    ;; On Windows XP there's a memory fault that is trapped
    ;; by SBCL at the end of `win32:enum-child-windows'
    #+sbcl
    (if (win32:is-windows-vista-or-greater)
        (win32:enum-child-windows (hwnd hwnd) (cffi:callback %map-windows-proc) 0)
        (ignore-errors (win32:enum-child-windows (hwnd hwnd) (cffi:callback %map-windows-proc) 0)))
    #-sbcl
    (win32:enum-child-windows (hwnd hwnd) (cffi:callback %map-windows-proc) 0)
    res))