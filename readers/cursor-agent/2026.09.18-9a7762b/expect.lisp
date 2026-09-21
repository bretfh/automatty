(:CORPUS 1 :AGENT "cursor-agent" :VERSION "2026.09.18-9a7762b" :WIDTH 160 :HEIGHT 50 :ALT-SCREEN
 NIL :LAUNCH ("cursor-agent") :STEPS
 ((:SCREEN :TRUST :CUT :SKIPPED)
  (:SCREEN :IDLE :CUT :OBSERVED :AT 1477 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT "Plan, search, build anything" :GHOST
    NIL))
  (:SCREEN :WORKING :CUT :OBSERVED :AT 7857 :SAW
   (:SCREEN :WORKING :MEANS :WORKING :WIDGET :FOOTER :LINE
    "→ Add a follow-up                                                                                                                             ctrl+c to stop")
   :VIA (:SUBMIT "Please run this shell command: touch atty-record-probe") :SENT
   ("Please run this shell command: touch atty-record-probe" ""))
  (:SCREEN :PERMISSION :CUT :OBSERVED :AT 32797 :SAW
   (:SCREEN :PERMISSION :MEANS :BLOCKED :WIDGET :CHOICE :QUESTION "Run this command?" :SUBJECT
    ("$  touch atty-record-probe in .") :TEXT "$  touch atty-record-probe in .
Run this command?
Not in allowlist: touch"
    :OPTIONS
    ("Run (once) (y)" "Add Shell(touch) to allowlist? (tab)" "Run Everything (shift+tab)"
     "Skip & tell the agent what to do instead (esc or n)")
    :DETAILS (NIL NIL NIL NIL) :SELECTED 0 :NUMBERED NIL :HINT NIL))
  (:SCREEN :REASON :CUT :OBSERVED :AT 35266 :SAW
   (:SCREEN :REASON :MEANS :BLOCKED :WIDGET :FOOTER :LINE
    "→ Tell the agent what to do instead (Enter to send, empty to skip, Esc to cancel)                                                             ctrl+c to stop")
   :VIA (:DENY) :SENT ("n"))
  (:SCREEN :IDLE :CUT :OBSERVED :AT 58716 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT "Add a follow-up" :GHOST NIL) :VIA
   (:SUBMIT "Do not run it. Stop here.") :SENT ("Do not run it. Stop here." ""))))
