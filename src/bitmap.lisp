(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun display-size ()
    (values (win32:get-system-metrics win32:+sm-xvirtualscreen+)
            (win32:get-system-metrics win32:+sm-yvirtualscreen+)
            (win32:get-system-metrics win32:+sm-cxvirtualscreen+)
            (win32:get-system-metrics win32:+sm-cyvirtualscreen+))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun screenshot (&key x y width height)
    (multiple-value-bind (dx dy dw dh) (display-size)
      (let* ((x (or x dx))
             (y (or y dy))
             (width (or width (- dw x)))
             (height (or height (- dh y)))
             (screen-dc (winutil:not-null-or-error (win32:get-dc (cffi:null-pointer)))))
        (unwind-protect
             (let ((memory-dc (winutil:not-null-or-error (win32:create-compatible-dc screen-dc))))
               (unwind-protect
                    (let* ((bitmap (winutil:not-null-or-error (win32:create-compatible-bitmap screen-dc width height)))
                           (success nil))
                      (unwind-protect
                           (let ((old-bitmap (winutil:not-null-or-error (win32:select-object memory-dc bitmap))))
                             (or (win32:bit-blt memory-dc 0 0 width height screen-dc x y win32:+srccopy+)
                                 (winutil:win32-error))
                             (prog1 (winutil:not-null-or-error (win32:select-object memory-dc old-bitmap))
                               (setf success t)))
                        (unless success
                          (win32:delete-object bitmap))))
                 (win32:delete-dc memory-dc)))
          (win32:release-dc (cffi:null-pointer) screen-dc))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bitmap-size (bitmap)
    (cffi:with-foreign-object (bm 'win32:bitmap)
      (when (zerop (win32:get-object bitmap (cffi:foreign-type-size 'win32:bitmap) bm))
        (winutil:win32-error))
      (values (ldb (byte 15 0) (cffi:foreign-slot-value bm 'win32:bitmap 'win32:width))
              (ldb (byte 15 0) (cffi:foreign-slot-value bm 'win32:bitmap 'win32:height))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-bitmap-data (bitmap width height bit-count buffer)
    (cffi:with-foreign-object (bmi 'win32:bitmapinfoheader)
      (win32:zero-memory bmi (cffi:foreign-type-size 'win32:bitmapinfoheader))
      (cffi:with-foreign-slots ((win32:size win32:planes win32:bit-count win32:width
                                            win32:height win32:compression win32:size-image)
                                bmi win32:bitmapinfoheader)
        (setf win32:size (cffi:foreign-type-size 'win32:bitmapinfoheader)
              win32:planes 1
              win32:bit-count bit-count
              win32:compression win32:+bi-rgb+
              win32:size-image 0
              win32:width width
              win32:height (- height)))

      (let ((hscreen (winutil:not-null-or-error (win32:get-dc (cffi:null-pointer)))))
        (unwind-protect
             (when (member (win32:get-di-bits hscreen bitmap 0 height buffer bmi win32:+dib-rgb-colors+)
                           '(0 #.win32:+error-invalid-parameter+))
               (winutil:win32-error))
          (win32:release-dc (cffi:null-pointer) hscreen))))
    (values)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bitmap->rgba (hbitmap)
    (multiple-value-bind (width height) (bitmap-size hbitmap)
      (let ((ret (cffi:make-shareable-byte-vector (* 4 width height))))
        (cffi:with-pointer-to-vector-data (ret-ptr ret)
          (get-bitmap-data hbitmap width height 32 ret-ptr))
        ret))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun screenshot-region-rgba (x y width height)
    (let ((sh (screenshot :x x :y y :width width :height height)))
      (unwind-protect (bitmap->rgba sh)
        (win32:delete-object sh)))))
