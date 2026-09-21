(cursor-agent
  :programs ("cursor-agent")
  :version (:exec-path "(?:cursor-cli-|cursor-agent/versions/)([0-9][^/]*)/")
  :versions ("2026.06.19-20-24-33-653a7fb")
  :launch ("cursor-agent")
  :submit :typed
  :interrupt ""
  :scenario
  ((:trust :optional t)
   (:idle :do (:approve))
   (:working :do (:submit "Please run this shell command: touch atty-record-probe"))
   (:permission)
   (:reason :do (:deny))
   (:idle :do (:deny)))
  :screens
  ((:trust :widget :choice :question "(?i)trust the contents of this directory" :means :blocked
    :actions (:approve (:option "(?i)trust this workspace") :deny (:option "(?i)quit")))
   (:permission :widget :choice :question "(?i)run this command\\?" :means :blocked
    :actions (:approve (:press "y")
              :approve-always (:press "TAB")
              :deny (:press "n")))
   (:reason :widget :footer :lines 4 :match "(?i)reason for rejection" :means :blocked
    :actions (:deny (:press "ESC")))
   (:working :widget :footer :lines 6 :match "(?i)ctrl\\+c to stop" :means :working)
   (:working :widget :spinner :means :working)
   (:idle :widget :prompt-input :means :idle)))
