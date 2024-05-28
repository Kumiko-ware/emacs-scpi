;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
(defun tcham-set-params (addr &rest args)
    "Sets the parameters of the temperature chamber

ARGS are the following named arguments:

:val1
:val2
:val3
:outs

Usage example:
\(tcham-set-params 01 :val1 23 :val2 50 :val3 80 :outs #xBEBACAFE)
"
    (let ((val1 (plist-get args :val1))
	  (val2 (plist-get args :val2))
	  (val3 (plist-get args :val3))
	  (outs (plist-get args :outs)))
      (concat "$"
	      (format "%02d" addr)
	      "E "
	      (format "%06.1f" val1) " "
	      (format "%06.1f" val2) " "
	      (format "%06.1f" val3) " "
	      (format "%06.1f" 0) " "
	      (format "%06.1f" 0) " "
	      (format "%06.1f" 0) " "
	      (format "%06.1f" 0) " "
	      (let (i str)
		(dotimes (i 3 str)
		  (setq str (concat str (format "%d" (logand 1 (lsh outs (- i 31))))))))
	      " "
	      (let (i str)
		(dotimes (i 29 str)
		  (setq str (concat str (format "%d" (logand 1 (lsh outs(- i 28)))))))) "\r")))
    
(defun tcham-get-params (addr)
  "Gets current parameters

Usage example:
\ (tcham-get-params 01)
"
  (concat "$"
	  (format "%02d" addr)
	  "I\r"))

(defun tcham-set-rates (addr &rest args)
        "Sets the temperature change rates of chamber

ARGS are the following named arguments:

:heat Heating gradient in K/min
:cool Cooling gradient in K/min
:+hum Relative humidity increase in %r.h./min
:-hum Relative humidity decrease in %r.h./min

Usage example:
\ (tcham-set-rates 01 :heat 3 :cool 5 :+hum 2.5 :-hum 2.5)
"
    (let ((heat (plist-get args :heat))
	  (cool (plist-get args :cool))
	  (+hum (plist-get args :+hum))
	  (-hum (plist-get args :-hum)))
      (concat "$"
	      (format "%02d" addr)
	      "U "
	      (format "%06.1f" heat) " "
	      (format "%06.1f" cool) " "
	      (format "%06.1f" +hum) " "
	      (format "%06.1f" -hum) "\r")))

(defun tcham-start-prog (addr prog)
  "Starts the program number PROG

Usage example:
\ (tcham-start-prog 01 123)
"
  (concat "$"
	  (format "%02d" addr)
	  "P"
	  (format "%04d" prog)
	  "F\r"))

(defun tcham-stop-prog (addr)
  "Stops a running programm

Usage example:
\ (tcham-stop-prog 01)
"
  (concat "$"
	  (format "%02d" addr)
	  "P0000\r"))

(defun tcham-get-error (addr)
  "Gets last error

Usage example:
\ (tcham-get-error 01)
"
  (concat "$"
	  (format "%02d" addr)
	  "F\r"))
  
(defun tcham-ack-error (addr)
  "Acknowledges error

Usage example:
\ (tcham-ack-error 01)
"
  (concat "$"
	  (format "%02d" addr)
	  "Q\r"))
