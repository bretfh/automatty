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
   #:session-panes
   #:session-windows
   #:window-panes
   #:window-label
   #:window-layout
   #:window-number
   #:window-of
   #:pane-number
   #:pane-address-of
   #:session-add-window
   #:view
   #:make-view
   #:watcher-view
   #:watcher-window
   #:watcher-focus
   #:watcher-zoomed
   #:watcher-layout
   #:watcher-back
   #:watcher-add-window
   #:watcher-split
   #:watcher-select-window
   #:watcher-cycle-window
   #:watcher-focus-next
   #:watcher-fits
   #:on-server
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
   #:session-delete-other-panes
   #:define-message-handler
   #:session-watchers
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
   #:+wheel-rows+ #:+max-scrollback+ #:+scrollback-budget+ #:+scrollbars-by-default+ #:+bar-by-default+ #:+rail-by-default+ #:+rail-width+
   #:+restore-command+ #:+restore-programs+ #:+saved-scrollback+ #:+save-quiet-after+ #:+save-at-most-every+
   #:bind-prefix-keys
   #:*init-error* #:load-user-init #:user-init-file #:config-dir #:init-help
   #:*state-home* #:state-dir #:encode-pane #:decode-pane #:encode-tree #:restore-state
   #:save-tree #:save-pane #:save-all #:save-due #:move-state-aside #:saved-sessions #:delete-saved-session
   #:write-form-atomically #:read-state-file
   #:make-client
   #:client-close
   #:client-step
   #:client-running
   #:client-exit-reason
   #:client-wire
   #:client-draw
   #:client-rows
   #:client-cols
   #:client-resized
   #:draw-overlay
   #:mode-of
   #:overlay-unbound-key
   #:push-overlay
   #:pop-overlay
   #:watcher-overlays
   #:watcher-behind
   #:watcher-mode
   #:handle-input
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
   #:list-sessions
   #:stop-server
   #:request
   #:server-agent-rows
   #:attach-or-create
   #:main))
