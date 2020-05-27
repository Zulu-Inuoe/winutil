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
           (data (make-array icon-len :element-type '(unsigned-byte 8) :initial-element 0)))
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
         (bmi8 0) head-len
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
         ))

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
  (make-load-form-saving-slots resource :slot-names '(%data) :environment environment))

(defclass icon-resource (resource)
  ((%hicon
    :type cffi:foreign-pointer)))

(defun make-icon-resource (rgba width height)
  (make-instance 'icon-resource
                 :data (%make-resource-data rgba width height t 0 0)))

(defmethod hicon ((icon-resource icon-resource))
  (unless (slot-boundp icon-resource '%hicon)
    (setf (slot-value icon-resource '%hicon)
          (let ((data (slot-value icon-resource '%data)))
            (cffi:with-pointer-to-vector-data (data-ptr data)
              (not-null-or-error (win32:create-icon-from-resource data-ptr (length data) t #x00030000))))))
  (slot-value icon-resource '%hicon))

(define-dispose (icon-resource icon-resource)
  (when (slot-boundp icon-resource '%hicon)
    (win32:destroy-icon (slot-value icon-resource '%hicon))
    (slot-makunbound icon-resource '%hicon))
  (values))

(defclass cursor-resource (resource)
  ((%hcursor
    :type cffi:foreign-pointer)
   (%hicon
    :type cffi:foreign-pointer)))

(defun make-cursor-resource (rgba width height &key (hotspot-x (truncate width 2)) (hotspot-y (truncate (abs height) 2)))
  (make-instance 'cursor-resource
                 :data (%make-resource-data rgba width height nil hotspot-x hotspot-y)))

(defmethod hcursor ((cursor-resource cursor-resource))
  (unless (slot-boundp cursor-resource '%hcursor)
    (setf (slot-value cursor-resource '%hcursor)
          (not-null-or-error
           (let ((data (slot-value cursor-resource '%data)))
             (cffi:with-pointer-to-vector-data (data-ptr data)
               (win32:create-icon-from-resource data-ptr (length data) nil #x00030000))))))
  (slot-value cursor-resource '%hcursor))

(defmethod hicon ((cursor-resource cursor-resource))
  (unless (slot-boundp cursor-resource '%hicon)
    (setf (slot-value cursor-resource '%hicon)
          (not-null-or-error
           (let ((data (slot-value cursor-resource '%data)))
             (cffi:with-pointer-to-vector-data (data-ptr data)
               (win32:create-icon-from-resource (cffi:inc-pointer data-ptr 4) (- (length data) 4) t #x00030000))))))
  (slot-value cursor-resource '%hicon))

(define-dispose (cursor-resource cursor-resource)
  (when (slot-boundp cursor-resource '%hcursor)
    (win32:destroy-cursor (slot-value cursor-resource '%hcursor))
    (slot-makunbound cursor-resource '%hcursor))
  (when (slot-boundp cursor-resource '%hicon)
    (win32:destroy-icon (slot-value cursor-resource '%hicon))
    (slot-makunbound cursor-resource '%hicon))
  (values))
