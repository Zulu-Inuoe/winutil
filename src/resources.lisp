(in-package #:winutil)

(defun %make-resource-data (rgba width height icon-p hotspot-x hotspot-y)
  "Creates a `vector' describing the icon or cursor resource."
  (multiple-value-bind (height flip) (if (plusp height)
                                         (values height nil)
                                         (values (- height) t))
    (let* ((hotspot-off 0)
           (hotspot-len (if icon-p 0 4))
           (head-off (+ hotspot-off hotspot-len))
           (head-len 40)
           (stride (* width 4))
           (rgba-off (+ head-off head-len))
           (rgba-len (* height stride))
           (mask-off (+ rgba-off rgba-len))
           (mask-len (* height (truncate (+ width 7) 8)))
           (icon-len (+ head-len rgba-len mask-len))
           (data (cffi:make-shareable-byte-vector icon-len)))
      (unless icon-p
        (setf
         ;; 16 hotspot x
         (aref data (+ hotspot-off 0)) (ldb (byte 8 0) hotspot-x)
         (aref data (+ hotspot-off 1)) (ldb (byte 8 8) hotspot-x)
         ;; 16 hotspot y
         (aref data (+ hotspot-off 2)) (ldb (byte 8 0) hotspot-y)
         (aref data (+ hotspot-off 3)) (ldb (byte 8 8) hotspot-y)))

      (labels (((setf bmi8) (value offset)
                 (setf (aref data (+ head-off offset)) value))
               ((setf bmi32) (value offset)
                 (setf (bmi8 (+ offset 0)) (ldb (byte 8 0) value)
                       (bmi8 (+ offset 1)) (ldb (byte 8 8) value)
                       (bmi8 (+ offset 2)) (ldb (byte 8 16) value)
                       (bmi8 (+ offset 3)) (ldb (byte 8 24) value))))
        (setf
         ;; 32 header size
         (bmi32 0) head-len
         ;; 32 width
         (bmi32 4) width
         ;; 32 height
         (bmi32 8) (* height 2)
         ;; 16 planes
         (bmi8 12) 1
         ;; 16 bit-count
         (bmi8 14) 32
         ;;32 size
         (bmi32 20) rgba-len
         ;; Leave others 0
         (bmi32 24) 0
         (bmi32 28) 0
         (bmi32 32) 0
         (bmi32 36) 0))

      ;; RGBA
      (if flip
          ;; Copy rows upside-down
          (loop
            :for y :below height
            :for ysrc := (* y stride)
            :for ydst := (+ rgba-off (* (- height 1 y) stride))
            :do (replace data rgba :start1 ydst :start2 ysrc :end2 (+ ysrc stride)))
          (replace data rgba :start1 rgba-off))

      ;; Mask
      (fill data #xFF :start mask-off)

      data)))

(defclass resource ()
  ((%data
    :type (simple-array (unsigned-byte 8) (*))
    :initarg :data))
  (:default-initargs
   :data (required-argument :data)))

(defmethod make-load-form ((resource resource) &optional environment)
  (declare (ignore environment))
  (let ((data (slot-value resource '%data)))
    (values
     `(make-instance ',(class-of resource) :data (replace (cffi:make-shareable-byte-vector ,(length data))
                                                           ,data))
     nil)))

#+sbcl
(progn
  (defgeneric on-quit (obj))
  (defgeneric on-restore (obj))

  (defclass restoreable (finalizable)
    ((%weak-ptr
      :type sb-ext:weak-pointer)))

  (defvar %*restoreables* (make-hash-table :test 'eq :synchronized t))

  (defmethod initialize-instance :after ((obj restoreable) &key &allow-other-keys)
    (let ((weak-ptr (sb-ext:make-weak-pointer obj)))
      (setf (slot-value obj '%weak-ptr) weak-ptr
            (gethash weak-ptr %*restoreables*) weak-ptr)))

  (define-finalizer restoreable (%weak-ptr)
    (remhash %weak-ptr %*restoreables*))

  (defmethod on-quit ((obj restoreable))
    nil)

  (defmethod on-restore ((obj restoreable))
    nil)

  (defun %quit-on-quit ()
    (maphash (lambda (k v)
               (declare (ignore v))
               (multiple-value-bind (value validp) (sb-ext:weak-pointer-value k)
                 (if validp
                     (on-quit value)
                     (remhash k %*restoreables*))))
             %*restoreables*))

  (pushnew '%quit-on-quit sb-ext:*exit-hooks*)
  (pushnew '%quit-on-quit sb-ext:*save-hooks*)

  (defun %restore-on-init ()
    (maphash (lambda (k v)
               (declare (ignore v))
               (multiple-value-bind (value validp) (sb-ext:weak-pointer-value k)
                 (if validp
                     (on-restore value)
                     (remhash k %*restoreables*))))
             %*restoreables*))

  (pushnew '%restore-on-init sb-ext:*init-hooks*))

(defclass icon-resource (resource
                         #+sbcl
                         restoreable
                         finalizable)
  ((%hicon
    :type cffi:foreign-pointer
    :reader hicon)))

(defun %initialize-hicon (icon-resource)
  (setf (slot-value icon-resource '%hicon)
        (not-null-or-error
         (let ((data (slot-value icon-resource '%data)))
           (cffi:with-pointer-to-vector-data (data-ptr data)
             (win32:create-icon-from-resource data-ptr (length data) t #x00030000))))))

(defmethod initialize-instance :after ((icon-resource icon-resource) &key &allow-other-keys)
  (%initialize-hicon icon-resource))

(define-finalizer icon-resource (%hicon)
  (win32:destroy-icon %hicon))

#+sbcl
(defmethod on-quit ((icon-resource icon-resource))
  (win32:destroy-icon (slot-value icon-resource '%hicon))
  (slot-makunbound icon-resource '%hicon))

#+sbcl
(defmethod on-restore ((icon-resource icon-resource))
  (%initialize-hicon icon-resource))

(defun make-icon-resource (rgba width height)
  (make-instance 'icon-resource
                 :data (%make-resource-data rgba width height t 0 0)))

(defclass cursor-resource (resource
                           #+sbcl
                           restoreable
                           finalizable)
  ((%hcursor
    :type cffi:foreign-pointer
    :reader hcursor)))

(defun %initialize-hcursor (cursor-resource)
  (setf (slot-value cursor-resource '%hcursor)
        (not-null-or-error
         (let ((data (slot-value cursor-resource '%data)))
           (cffi:with-pointer-to-vector-data (data-ptr data)
             (win32:create-icon-from-resource data-ptr (length data) nil #x00030000))))))

(defmethod initialize-instance :after ((cursor-resource cursor-resource) &key &allow-other-keys)
  (%initialize-hcursor cursor-resource))

(define-finalizer cursor-resource (%hcursor)
  (win32:destroy-cursor %hcursor))

#+sbcl
(defmethod on-quit ((cursor-resource cursor-resource))
  (win32:destroy-cursor (slot-value cursor-resource '%hcursor))
  (slot-makunbound cursor-resource '%hcursor))

#+sbcl
(defmethod on-restore ((cursor-resource cursor-resource))
  (%initialize-hcursor cursor-resource))

(defun make-cursor-resource (rgba width height &key hotspot-x hotspot-y)
  (make-instance 'cursor-resource
                 :data (%make-resource-data rgba width height nil
                                            (or hotspot-x (truncate width 2))
                                            (or hotspot-y (truncate (abs height) 2)))))
