;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:automatty
  (:use #:cl)
  (:import-from #:atty
                #:defcommand #:run-command #:*client*
                #:pane-mode #:scroll-mode #:board-mode #:note-mode
                #:configure #:setting #:settings
                #:add-hook #:remove-hook
                #:session-made #:pane-started #:pane-ended #:client-attached
                #:before-save #:server-restored
                #:show-note)
  (:import-from #:atty/mode
                #:define-key #:undefine-key #:global-set-key #:global-unset-key #:define-mode)
  (:export
   #:defserver #:session #:window #:columns #:rows #:pane
   #:defcommand #:run-command #:*client*
   #:define-key #:undefine-key #:global-set-key #:global-unset-key #:define-mode
   #:pane-mode #:scroll-mode #:board-mode #:note-mode
   #:configure #:setting #:settings
   #:add-hook #:remove-hook
   #:session-made #:pane-started #:pane-ended #:client-attached
   #:before-save #:server-restored
   #:show-note))

(defpackage #:automatty/user
  (:use #:cl #:automatty))
