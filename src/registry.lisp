(in-package :winutil)

(defun open-reg-key (hkey subkey &key (sam win32:+key-read+))
  (cffi:with-foreign-object (res-key 'win32:hkey)
    (let ((code (win32:reg-open-key-ex hkey subkey 0 sam res-key)))
      (unless (zerop code)
        (win32-error code)))
    (cffi:mem-ref res-key 'win32:hkey)))

(defun close-reg-key (hkey)
  (let ((code (win32:reg-close-key hkey)))
    (unless (zerop code)
      (win32-error code)))
  (values))

(defun get-reg-string (hkey &key sub-key name
                            &aux
                              (sub-key (or sub-key (cffi:null-pointer)))
                              (name (or name (cffi:null-pointer))))
  (cffi:with-foreign-object (psize 'win32:dword)
    (let ((code (win32:reg-get-value hkey sub-key name (logior win32:+rrf-rt-reg-sz+ win32:+rrf-rt-reg-expand-sz+)
                                     (cffi:null-pointer) (cffi:null-pointer) psize)))
      (unless (zerop code)
        (win32-error code)))

    (cffi:with-foreign-objects ((buffer :uint8 (cffi:mem-ref psize 'win32:dword)))
      (let ((code (win32:reg-get-value hkey sub-key name (logior win32:+rrf-rt-reg-sz+ win32:+rrf-rt-reg-expand-sz+)
                                       (cffi:null-pointer) buffer psize)))
        (unless (zerop code)
          (win32-error code)))
      (values (cffi:foreign-string-to-lisp buffer :encoding win32:+win32-string-encoding+)))))

(defun get-reg-number (hkey &key sub-key name
                            &aux
                              (sub-key (or sub-key (cffi:null-pointer)))
                              (name (or name (cffi:null-pointer))))
  (cffi:with-foreign-objects ((type 'win32:dword)
                              (buffer :uint64)
                              (size 'win32:dword))
    (setf (cffi:mem-ref size 'win32:dword) (cffi:foreign-type-size :uint64))
    (let ((code (win32:reg-get-value hkey sub-key name win32:+rrf-rt-qword+
                                     type buffer size)))
      (unless (zerop code)
        (win32-error code)))

    (ecase (cffi:mem-ref type 'win32:dword)
      ((#.win32:+reg-dword+
        #+little-endian #.win32:+reg-dword-little-endian+
        #+big-endian #.win32:+reg-dword-big-endian+)
       (cffi:mem-ref buffer 'win32:dword))
      ((#.win32:+reg-qword+
        #+little-endian #.win32:+reg-qword-little-endian+)
       (cffi:mem-ref buffer 'win32:qword))
      ((#+little-endian #.win32:+reg-dword-big-endian+
        #+big-endian #.win32:+reg-dword-little-endian+)
       (let ((val (cffi:mem-ref buffer 'win32:dword)))
         (logior (ash (logand #x000000FF val) 24)
                 (ash (logand #x0000FF00 val) 8)
                 (ash (logand #x00FF0000 val) -8)
                 (ash (logand #xFF000000 val) -24))))
      #+big-endian
      (#.win32:+reg-qword-little-endian+
       (let ((val (cffi:mem-ref buffer 'win32:dword)))
         (logior (ash (logand #x00000000000000FF val) 56)
                 (ash (logand #x000000000000FF00 val) 40)
                 (ash (logand #x0000000000FF0000 val) 24)
                 (ash (logand #x00000000FF000000 val) 8)
                 (ash (logand #x000000FF00000000 val) -8)
                 (ash (logand #x0000FF0000000000 val) -24)
                 (ash (logand #x00FF000000000000 val) -40)
                 (ash (logand #xFF00000000000000 val) -56))))
      (#.win32:+reg-binary+
       (ecase (cffi:mem-ref size 'win32:dword)
         (4 (cffi:mem-ref buffer 'win32:dword))
         (8 (cffi:mem-ref buffer 'win32:qword)))))))
