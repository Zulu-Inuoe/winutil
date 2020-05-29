(in-package #:winutil)

(defgeneric hwnd (obj)
  (:documentation "Coerce `obj' into an `hwnd'.")
  (:method (obj) obj)
  (:method ((obj integer))
    (cffi:make-pointer obj)))

(defun hwnd-pos (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (values
     (cffi:foreign-slot-value r 'win32:rect 'win32:left)
     (cffi:foreign-slot-value r 'win32:rect 'win32:top))))

(defun set-hwnd-pos (hwnd x y)
  (or (win32:set-window-pos (hwnd hwnd) (cffi:null-pointer) x y 0 0
                            (logior win32:+swp-nosize+ win32:+swp-nozorder+
                                    win32:+swp-noactivate+ win32:+swp-noownerzorder+))
      (win32-error))
  (values x y))

(define-setf-expander hwnd-pos (hwnd)
  (let ((hwnd-var (gensym "HWND"))
        (x-var (gensym "X"))
        (y-var (gensym "Y")))
    (values `(,hwnd-var)
            `(,hwnd)
            `(,x-var ,y-var)
            `(set-hwnd-pos ,hwnd-var (or ,x-var 0) (or ,y-var 0))
            `(values ,x-var ,y-var))))

(defun hwnd-pos* (hwnd)
  (multiple-value-bind (x y) (hwnd-pos hwnd)
    (cons x y)))

(defun (setf hwnd-pos*) (value hwnd)
  (set-hwnd-pos hwnd (car value) (cdr value))
  value)

(defun hwnd-x (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (cffi:foreign-slot-value r 'win32:rect 'win32:left)))

(defun hwnd-y (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (cffi:foreign-slot-value r 'win32:rect 'win32:top)))

(defun (setf hwnd-x) (value hwnd)
  (set-hwnd-pos hwnd value (hwnd-y hwnd))
  value)

(defun (setf hwnd-y) (value hwnd)
  (set-hwnd-pos hwnd (hwnd-x hwnd) value)
  value)

(defun hwnd-size (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (values
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:right)
       (cffi:foreign-slot-value r 'win32:rect 'win32:left))
     (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom)
        (cffi:foreign-slot-value r 'win32:rect 'win32:top)))))

(defun set-hwnd-size (hwnd width height)
  (or (win32:set-window-pos (hwnd hwnd) (cffi:null-pointer)  0 0 width height
                            (logior win32:+swp-nomove+ win32:+swp-nozorder+
                                    win32:+swp-noactivate+ win32:+swp-noownerzorder+))
      (win32-error))
  (values width height))

(define-setf-expander hwnd-size (hwnd)
  (let ((hwnd-var (gensym "HWND"))
        (width-var (gensym "WIDTH"))
        (height-var (gensym "HEIGHT")))
    (values `(,hwnd-var)
            `(,hwnd)
            `(,width-var ,height-var)
            `(set-hwnd-size ,hwnd-var ,width-var ,height-var)
            `(values ,width-var ,height-var))))

(defun hwnd-size* (hwnd)
  (multiple-value-bind (width height) (hwnd-size hwnd)
    (cons width height)))

(defun (setf hwnd-size*) (value hwnd)
  (set-hwnd-size hwnd (car value) (cdr value)))

(defun hwnd-width (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (- (cffi:foreign-slot-value r 'win32:rect 'win32:right)
       (cffi:foreign-slot-value r 'win32:rect 'win32:left))))

(defun hwnd-height (hwnd)
  (cffi:with-foreign-object (r 'win32:rect)
    (or (win32:get-window-rect (hwnd hwnd) r)
        (win32-error))
    (- (cffi:foreign-slot-value r 'win32:rect 'win32:bottom)
       (cffi:foreign-slot-value r 'win32:rect 'win32:top))))

(defun (setf hwnd-width) (value hwnd)
  (set-hwnd-size hwnd value (hwnd-height hwnd))
  value)

(defun (setf hwnd-height) (value hwnd)
  (set-hwnd-size hwnd (hwnd-width hwnd) value)
  value)

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

(defun (setf hwnd-text) (value hwnd &aux (hwnd (hwnd hwnd)))
  (or (win32:set-window-text hwnd value)
      (win32-error))
  value)

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
