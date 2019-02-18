(defpackage #:winutil
  (:use
   #:alexandria
   #:cl
   #:dispose)
  (:export
   #:lisp-to-tstring
   #:tstring-to-lisp

   #:error-code-string

   #:win32-error
   #:win32-error-code
   #:win32-error-string

   #:exe-pathname
   #:exe-dir-pathname
   #:processor-count

   #:cursor-position
   #:cursor-position*
   #:list-windows
   #:list-child-windows
   #:defwndproc
   #:message-pump

   #:get-clipboard-text
   #:set-clipboard-text
   #:clear-clipboard

   #:wndclass-name
   #:wndclass-instance
   #:wndclass-atom

   #:wndclass-wrapper
   #:wndclass-wrapper-name
   #:wndclass-wrapper-instance
   #:wndclass-wrapper-atom

   #:hwnd
   #:hwnd-x
   #:hwnd-y
   #:hwnd-pos
   #:hwnd-pos*
   #:hwnd-width
   #:hwnd-height
   #:hwnd-dimensions
   #:hwnd-dimensions*
   #:hwnd-text

   #:hwnd-wrapper
   #:hwnd-wrapper-hwnd

   #:hmenu-wrapper
   #:hmenu-wrapper-hmenu

   #:tray-icon-wrapper
   #:tray-icon-wrapper-hwnd
   #:tray-icon-wrapper-message-id
   #:tray-icon-wrapper-id
   #:tray-icon-wrapper-tooltip

   #:window
   #:window-wndclass-name
   #:window-instance
   #:window-hwnd
   #:window-wndproc))
