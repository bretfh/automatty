;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:atty
  (:use #:cl)
  (:local-nicknames (#:pty #:atty/pty) (#:tty #:atty/tty) (#:agent #:atty/agent))
  (:export
   #:wire
   #:make-wire
   #:wire-fd
   #:wire-open
   #:wire-close
   #:wire-send
   #:wire-receive
   #:wire-read-message
   #:wire-flush
   #:wire-pending
   #:wire-in-bytes
   #:encode-face
   #:decode-face
   #:encode-runs
   #:decode-runs-into-screen

   #:pane
   #:make-pane
   #:pane-start
   #:pane-started
   #:pane-term
   #:pane-fd
   #:pane-pid
   #:pane-running
   #:pane-dirty
   #:pane-drain
   #:pane-write
   #:pane-resize
   #:pane-command
   #:pane-agent
   #:pane-id
   #:pane-close

   #:*interval*
   #:make-server
   #:server-close
   #:server-running
   #:server-sessions
   #:server-pending-watchers
   #:watcher-wire
   #:watcher-interactive
   #:server-command
   #:session-named
   #:join-session
   #:drop-watcher
   #:server-step
   #:serve
   #:*version*
   #:add-session
   #:session-name
   #:session-layout
   #:session-focus
   #:session-zoomed
   #:session-panes
   #:session-windows
   #:session-window
   #:session-zoomed
   #:window-zoomed
   #:window-panes
   #:window-label
   #:window-focus
   #:window-layout
   #:window-number
   #:window-of
   #:pane-number
   #:pane-address-of
   #:session-add-window
   #:session-select-window
   #:session-cycle-window
   #:session-close-window
   #:session-rename-window
   #:session-nth-window
   #:session-observe
   #:session-socket
   #:agent-rows
   #:split
   #:make-split
   #:split-way
   #:split-parts
   #:layout-panes
   #:layout-insert
   #:layout-remove
   #:layout-tree
   #:session-split
   #:session-close-pane
   #:session-focus-next
   #:session-delete-other-panes
   #:message-types #:define-message-handler #:message-handlers
   #:session-screen
   #:session-rows
   #:session-cols
   #:session-watchers
   #:session-bar-p
   #:session-tree
   #:pane-view
   #:view-pane
   #:views-in
   #:*bar*
   #:default-bar
   #:session-bar

   #:+prefix+
   #:defsetting #:configure #:setting #:settings #:after-setting
   #:defhook #:add-hook #:remove-hook #:run-hook
   #:+wheel-rows+ #:+max-scrollback+ #:+scrollbars-by-default+ #:+bar-by-default+
   #:+restore-command+ #:+saved-scrollback+ #:+save-quiet-after+ #:+save-at-most-every+
   #:bind-prefix-keys
   #:*init-error* #:load-user-init #:user-init-file #:config-dir #:init-help
   #:*state-home* #:state-dir #:encode-pane #:decode-pane #:encode-tree #:restore-state
   #:save-tree #:save-pane #:save-all #:save-due #:move-state-aside #:saved-sessions #:delete-saved-session
   #:write-form-atomically #:read-state-file
   #:+base-message-types+
   #:client-message-types
   #:send-to-server
   #:make-client
   #:client-close
   #:client-step
   #:client-running
   #:client-exit-reason
   #:client-screen
   #:client-wire
   #:client-overlays
   #:client-push-overlay
   #:client-pop-overlay
   #:client-dirty
   #:client-draw
   #:draw-overlay
   #:mode-of
   #:overlay-unbound-key
   #:client-mode
   #:client-rows
   #:client-cols
   #:client-resized
   #:attach

   #:defcommand
   #:*commands*
   #:*unlisted*
   #:command-names
   #:*client*
   #:event-key
   #:pane-mode
   #:run-command

   #:note
   #:make-note
   #:show-note
   #:show-error

   #:prompt
   #:make-prompt
   #:prompt-query
   #:prompt-index
   #:prompt-showing
   #:prompt-chosen
   #:prompt-tree
   #:open-prompt
   #:prompt-command
   #:matches
   #:score

   #:socket-directory
   #:socket-path
   #:log-path
   #:record-agent
   #:verify-corpus
   #:corpus-dirs
   #:server-socket-path
   #:*server-name*
   #:probe-socket
   #:other-servers
   #:request-sessions
   #:request-stop-session
   #:list-sessions
   #:stop-server
   #:request
   #:server-agent-rows
   #:attach-or-create
   #:main))
