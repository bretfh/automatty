(cursor-agent
  :from "2026.06.19-20-24-33-653a7fb"
  :versions ("2026.09.18-9a7762b")
  :scenario
  ((:trust :optional t)
   (:idle :do (:approve))
   (:working :do (:submit "Please run this shell command: touch atty-record-probe"))
   (:permission)
   (:reason :do (:deny))
   (:idle :do (:submit "Do not run it. Stop here.")))
  :screens
  ((:reason :widget :footer :lines 4 :match "(?i)tell the agent what to do instead" :means :blocked
    :input t)))
