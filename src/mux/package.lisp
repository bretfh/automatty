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
   #:wire-fill
   #:wire-take
   #:wire-flush
   #:wire-pending
   #:wire-in-bytes
   #:face-said
   #:said-face
   #:runs-said
   #:said-into-screen

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
   #:pane-say
   #:pane-resize
   #:pane-command
   #:pane-agent
   #:pane-id
   #:pane-close

   #:*interval*
   #:make-server
   #:server-close
   #:server-going
   #:server-sessions
   #:server-knocking
   #:watcher-wire
   #:watcher-here
   #:server-command
   #:session-named
   #:join-session
   #:drop-watcher
   #:server-step
   #:serve
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
   #:add-window
   #:go-to-window
   #:step-window
   #:close-a-window
   #:name-a-window
   #:window-called
   #:session-observe
   #:session-socket
   #:agent-rows
   #:split
   #:make-split
   #:split-way
   #:split-parts
   #:panes-in
   #:put-beside
   #:without-pane
   #:layout-tree
   #:split-the-session
   #:close-the-pane
   #:focus-the-next
   #:only-the-pane
   #:+understood+
   #:session-screen
   #:session-rows
   #:session-cols
   #:session-watchers
   #:session-barp
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
   #:bind-prefixed-keys
   #:*init-problem* #:load-user-init #:user-init-file #:config-dir #:init-help
   #:*state-home* #:state-dir #:pane-said #:said-pane #:tree-said #:restore-state
   #:save-tree #:save-pane #:save-everything #:save-what-is-due #:move-state-aside #:saved-sessions
   #:write-form-atomically #:read-state-file
   #:+was-known+
   #:client-knows
   #:tell-the-server
   #:make-client
   #:client-close
   #:client-step
   #:client-going
   #:client-why
   #:client-screen
   #:client-wire
   #:client-over
   #:client-over-put
   #:client-over-drop
   #:client-dirty
   #:client-show
   #:draw-over
   #:mode-of
   #:unbound
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
   #:key-of
   #:pane-mode
   #:run-command

   #:note
   #:make-note
   #:show-note
   #:show-broke

   #:prompt
   #:make-prompt
   #:prompt-query
   #:prompt-index
   #:prompt-showing
   #:prompt-chosen
   #:prompt-tree
   #:ask
   #:ask-a-command
   #:matches
   #:score

   #:mux-dir
   #:socket-path
   #:log-path
   #:record-agent
   #:verify-corpus
   #:corpus-dirs
   #:where-the-server-is
   #:*server-name*
   #:knocked
   #:other-servers
   #:the-sessions
   #:stop-a-session
   #:list-sessions
   #:stop-a-server
   #:asked
   #:agents-here
   #:run
   #:main))
