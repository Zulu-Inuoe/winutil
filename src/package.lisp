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

   #:check-last-error
   #:not-null-or-error

   #:exe-pathname
   #:exe-dir-pathname
   #:drive-pathnames
   #:processor-count

   #:clip-cursor
   #:unclip-cursor
   #:cursor-position
   #:cursor-position*
   #:set-cursor-position
   #:cursor-x
   #:cursor-y

   #:list-windows
   #:list-child-windows
   #:wparam
   #:lparam
   #:lresult
   #:defwndproc
   #:message-pump

   #:hicon
   #:hcursor
   #:hmenu

   #:icon-resource
   #:make-icon-resource

   #:cursor-resource
   #:make-cursor-resource

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
   #:hwnd-pos
   #:hwnd-pos*
   #:set-hwnd-pos
   #:hwnd-x
   #:hwnd-y
   #:hwnd-size
   #:hwnd-size*
   #:set-hwnd-size
   #:hwnd-width
   #:hwnd-height
   #:hwnd-text

   #:client-to-screen
   #:client-to-screen*
   #:screen-to-client
   #:screen-to-client*

   #:hwnd-wrapper
   #:hwnd-wrapper-hwnd

   #:hmenu-wrapper
   #:hmenu-wrapper-hmenu

   #:window
   #:call-wndproc
   #:defmsg-method

   ;; Restarts
   #:retry-wndproc
   #:use-default-wndproc

   #:message-only-window

   #:tray-icon
   #:tray-icon-tooltip
   #:tray-icon-icon

   #:tray-event))
