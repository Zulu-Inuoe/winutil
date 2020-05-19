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

   #:check-win32-error

   #:exe-pathname
   #:exe-dir-pathname
   #:processor-count

   #:clip-cursor
   #:unclip-cursor
   #:cursor-position
   #:cursor-position*
   #:set-cursor-position

   #:list-windows
   #:list-child-windows
   #:defwndproc
   #:message-pump

   #:open-reg-key
   #:close-reg-key
   #:get-reg-string
   #:get-reg-number

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

   #:window
   #:window-wndclass-name
   #:window-instance
   #:window-hwnd
   #:call-wndproc

   #:message-only-window

   #:tray-icon
   #:tray-icon-tooltip))
