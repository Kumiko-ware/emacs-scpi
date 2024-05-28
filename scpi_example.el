(add-to-list 'load-path "~/devel/utils")
(require 'scpi)

(scpi-new 'analyser "192.168.2.30")
(analyser "*IDN?")
(analyser ":HCOP:DEV:LANG1 PNG; *WAI")
(analyser ":MMEM:NAME \'C:\\R_S\\INSTR\\USER\\PRINT1.PNG\'; *WAI")

(while t
  (analyser ":HCOP:IMM1; *WAI")
  (sit-for .8)
  (analyser :cmd-to-file "screen.png" ":MMEM:DATA? 'C:\\R_S\\INSTR\\USER\\PRINT1.PNG'")
  (find-file "screen.png"))

(analyser :terminate)

