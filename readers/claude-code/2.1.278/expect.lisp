(:CORPUS 1 :AGENT "claude-code" :VERSION "2.1.278" :WIDTH 160 :HEIGHT 50 :ALT-SCREEN T :LAUNCH
 ("claude" "--permission-mode" "default") :STEPS
 ((:SCREEN :TRUST :CUT :SKIPPED)
  (:SCREEN :IDLE :CUT :OBSERVED :AT 1895 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT " Try \"edit <filepath> to...\"" :GHOST
    NIL))
  (:SCREEN :WORKING :CUT :OBSERVED :AT 2885 :SAW
   (:SCREEN :WORKING :MEANS :WORKING :WIDGET :TITLE :TEXT "◐ atty-record-probe") :VIA
   (:SUBMIT "Please use your Bash tool to run: touch atty-record-probe") :SENT
   ("Please use your Bash tool to run: touch atty-record-probe" ""))
  (:SCREEN :PERMISSION :CUT :OBSERVED :AT 5041 :SAW
   (:SCREEN :PERMISSION :MEANS :BLOCKED :WIDGET :CHOICE :QUESTION "Do you want to proceed?"
    :SUBJECT
    ("Bash command"
     "Tip: auto mode handles these prompts for you — choose \"switch to auto mode\" below"
     "touch atty-record-probe" "Create empty probe file")
    :TEXT "Bash command
Tip: auto mode handles these prompts for you — choose \"switch to auto mode\" below
touch atty-record-probe
Create empty probe file
Do you want to proceed?"
    :OPTIONS
    ("Yes" "Yes, and always allow access to /private/tmp/atty-record/claude-code from this project"
     "Yes, and switch to auto mode · auto mode handles these prompts for you" "No")
    :DETAILS (NIL NIL NIL NIL) :SELECTED 0 :NUMBERED T :HINT "Esc to cancel · Tab to amend"))
  (:SCREEN :IDLE :CUT :OBSERVED :AT 6732 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT " " :GHOST NIL) :VIA (:DENY) :SENT
   ("4"))
  (:SCREEN :TRANSCRIPT :CUT :OBSERVED :AT 7518 :SAW
   (:SCREEN :TRANSCRIPT :MEANS :SKIP :WIDGET :FOOTER :LINE
    "Showing detailed transcript · ctrl+o to toggle · ↑↓ scroll · v to open in nano · ? for shortcuts                                                      verbose")
   :VIA (:PRESS "C-o") :SENT (""))
  (:SCREEN :IDLE :CUT :OBSERVED :AT 8979 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT " " :GHOST NIL) :VIA (:PRESS "C-o")
   :SENT (""))
  (:SCREEN :QUESTION :CUT :OBSERVED :AT 12367 :SAW
   (:SCREEN :QUESTION :MEANS :BLOCKED :WIDGET :CHOICE :QUESTION "Do you prefer red or blue?"
    :SUBJECT ("☐ Color") :TEXT "☐ Color
Do you prefer red or blue?"
    :OPTIONS ("Red" "Blue" "Type something." "Chat about this") :DETAILS
    (("Choose red") ("Choose blue") NIL NIL) :SELECTED 0 :NUMBERED T :HINT
    "Enter to select · ↑/↓ to navigate · Esc to cancel")
   :VIA
   (:SUBMIT
    "Use your AskUserQuestion tool to ask me whether I prefer red or blue, with those two options.")
   :SENT
   ("Use your AskUserQuestion tool to ask me whether I prefer red or blue, with those two options."
    ""))
  (:SCREEN :IDLE :CUT :OBSERVED :AT 15521 :SAW
   (:SCREEN :IDLE :MEANS :IDLE :WIDGET :PROMPT-INPUT :TEXT " " :GHOST NIL) :VIA (:CHOOSE 1) :SENT
   ("1"))))
