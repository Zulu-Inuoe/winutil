(defsystem #:com.inuoe.winutil
  :version "0.0.1"
  :description "Utility library for Windows programs."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "error" :depends-on ("package" "util"))
   (:file "ui-util" :depends-on ("package"))
   (:file "hicon" :depends-on ("package" "util"))
   (:file "hcursor" :depends-on ("package" "util"))
   (:file "hmenu" :depends-on ("package" "util"))
   (:file "resources" :depends-on ("package" "hicon" "hcursor"))
   (:file "registry" :depends-on ("package" "util"))
   (:file "clipboard" :depends-on ("package" "util"))
   (:file "wndclass" :depends-on ("package"))
   (:file "wndclass-wrapper" :depends-on ("package" "wndclass" "hicon" "hcursor"))
   (:file "hwnd" :depends-on ("package" "util" "ui-util"))
   (:file "hwnd-wrapper" :depends-on ("package" "wndclass" "hwnd"))
   (:file "hmenu-wrapper" :depends-on ("package" "hmenu"))
   (:file "window" :depends-on ("package" "hwnd-wrapper" "wndclass-wrapper" "util"))
   (:file "message-only-window" :depends-on ("package" "window"))
   (:file "tray-icon" :depends-on ("package" "error" "util" "window")))
  :depends-on
  (#:alexandria
   #:bordeaux-threads
   #:cffi
   #:com.inuoe.dispose
   #:com.inuoe.finalizer
   #:win32
   #:uiop))
