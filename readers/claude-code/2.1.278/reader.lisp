(claude-code
  :programs ("claude")
  :title "Claude Code"
  :version (:exec-path "versions/([0-9][0-9.]*)$")
  :versions ("2.1.278")
  :launch ("claude" "--permission-mode" "default")
  :submit :typed
  :scenario
  ((:trust :optional t)
   (:idle :do (:approve))
   (:working :do (:submit "Please use your Bash tool to run: touch atty-record-probe"))
   (:permission)
   (:idle :do (:deny))
   (:transcript :do (:press "C-o"))
   (:idle :do (:press "C-o"))
   (:question :do (:submit "Use your AskUserQuestion tool to ask me whether I prefer red or blue, with those two options."))
   (:idle :do (:choose 1))
   (:edit :do (:submit "Please create a file named atty-record-note.txt containing the word hi."))
   (:idle :do (:deny)))
  :screens
  ((:transcript :widget :footer :match "(?i)showing detailed transcript" :means :skip)
   (:trust :widget :choice :question "(?i)trust this folder|quick safety check" :means :blocked
    :actions (:approve (:option "(?i)^yes") :deny (:option "(?i)^no")))
   (:permission :widget :choice :question "(?i)do you want to proceed\\?" :means :blocked
    :choose :digits
    :actions (:approve (:option "^Yes$")
              :approve-always (:option "(?i)don.t ask again|from this project")
              :deny (:option "(?i)^no\\b")))
   (:edit :widget :choice :question "(?i)do you want to (make this edit|create |overwrite |write |delete )" :means :blocked
    :choose :digits
    :actions (:approve (:option "^Yes$")
              :approve-always (:option "(?i)accept edits|don.t ask again")
              :deny (:option "(?i)^no\\b")))
   (:workflow :widget :choice :question "(?i)run a dynamic workflow\\?" :means :blocked
    :choose :digits
    :actions (:approve (:option "(?i)^yes") :deny (:option "(?i)^no")))
   (:elicitation :widget :choice :question "(?i)mcp server .* requests your input" :means :blocked
    :actions (:approve (:option "(?i)^accept") :deny (:option "(?i)^decline")))
   (:question :widget :choice :match "(?i)enter to (select|confirm)" :means :blocked
    :choose :digits)
   (:working :widget :title :match "^[◐◑◒◓⠀-⣿] " :means :working)
   (:working :widget :spinner :means :working)
   (:working :widget :footer :match "(?i)esc to interrupt|esc to close" :means :working)
   (:working :widget :footer :lines 12
    :match "(?i)^\\s*\\S\\s+waiting for [1-9]\\d* background agents? to finish" :means :working)
   (:idle :widget :prompt-input :means :idle)))
