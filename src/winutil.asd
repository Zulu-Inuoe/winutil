(defsystem #:winutil
  :version "0.0.1"
  :description "Utility library for Windows programs."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "ui-util" :depends-on ("package"))
   (:file "wndclass" :depends-on ("package"))
   (:file "wndclass-wrapper" :depends-on ("package" "wndclass"))
   (:file "hwnd" :depends-on ("package"))
   (:file "hwnd-wrapper" :depends-on ("package" "wndclass" "hwnd"))
   (:file "hmenu-wrapper" :depends-on ("package"))
   (:file "tray-icon-wrapper" :depends-on ("package"))
   (:file "window" :depends-on ("package" "hwnd")))
  :depends-on
  (#:alexandria
   #:dispose
   #:win32))
