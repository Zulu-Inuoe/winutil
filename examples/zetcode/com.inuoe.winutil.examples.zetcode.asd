(defsystem #:com.inuoe.winutil.examples.zetcode
  :version "0.0.1"
  :description "Implementation of Zetcode GUI tutorial programs"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :components
  ((:file "simple-program")
   (:file "centering-a-window")
   (:file "hot-key")
   (:file "more-windows")
   (:file "the-escape-key")
   (:file "moving-a-window")
   (:file "flashing-a-window")
   (:file "a-simple-menu")
   (:file "a-popup-menu"))
  :depends-on
  (#:cffi
   #:win32
   #:winutil))
