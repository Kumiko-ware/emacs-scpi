;;; scpi.el --- Raw interface for LXI            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:
;;
;; USAGE EXAMPLE:
;;
;; (scpi-new 'ps1 "192.168.1.5")
;; (ps1 "*IDN?")
;; (ps1 "CURR:RANGE %f,(@1:2)" 1.5)
;; (ps1 "CURR:RANGE %f,(@3:4)" 3)
;; (ps1 "VOLT:RANGE %f,(@1:4)" 36)
;; (ps1 "VOLT 5,(@1);CURR 1.5,(@1)")
;; (ps1 "VOLT 6,(@2);CURR 1.5,(@2)")
;; (ps1 "VOLT 14,(@3);CURR 3,(@3)")
;; (ps1 "VOLT 20,(@4);CURR 3,(@4)")
;; (ps1 "MEAS:VOLT? (@1:4)")
;; (ps1 "OUTP ON,(@1:4)")
;; (ps1 "MEAS:VOLT? (@1:4)")
;; (ps1 "OUTP OFF,(@1:4)")
;; (ps1 :cmd-to-file "screen.png" ":MMEM:DATA? 'C:\\R_S\\INSTR\\USER\\PRINT1.PNG'")
;; (ps1 :terminate)
;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<-------
;;; Code:

(defconst scpi-doc-str
  "SCPI command interface.
Usage:
Simple commands: (NAME \"command string\" ARGS)
The command string can contain format specifications according to
the format function. For placing a '%' you have to use a double
sign '%%'.

Other commands:
\(NAME :terminate) stops the process and deletes the object."
  "Documentation string for the created objects")

(defconst scpi-time-out 1
  "Seconds for time out when receiving")

(defun scpi-1-command (ip cmd)
  (scpi-new 'scpi-temp ip)
  (let ((rply (scpi-temp cmd)))
    (scpi-temp :terminate)
    rply))

(defun accept-data (name)
  (unless (accept-process-output (eval name)
				 scpi-time-out)
    (user-error "TIMEOUT in communication with %s" name)))

(defun scpi-new (name addr)
  "Creates a SCPI connection to ip ADDR.
NAME is the name of the object to be created
IP is the ip address of the instrument."
  (unless (and (symbolp name) (stringp addr))
    (user-error "scpi-new: bad arguments"))
  (if (or (functionp name) (boundp name))
      (user-error "%s already exist or defined" name))
  (set name (open-network-stream "scpi"
				 (generate-new-buffer-name " *scpi*")
				 addr 5025))
  (set-process-coding-system (eval name) 'binary 'binary)
  (with-current-buffer (process-buffer (eval name))
    (set-buffer-multibyte nil))
  (or name (user-error "scpi-new: create process failed."))
  (fset name (lambda (method &rest args)
	       (with-current-buffer (process-buffer (eval name))
		 (pcase method
		   ((pred stringp)
		    (erase-buffer)
		    (process-send-string (eval name)
					 (apply 'format
						(concat method "\n")
						args))
		    (if (string-match "?" method)
			(while (not (search-backward "\n" nil t))
			 (accept-data name)))
		    (buffer-string))
		   (:cmd-to-file
		    (with-current-buffer (process-buffer (eval name))
		      (erase-buffer)
		      (process-send-string (eval name)
					   (apply 'format
						  (concat (cadr args) "\n")
						  (cddr args)))
		      (while (< (buffer-size) 2)
		        (accept-data name))
		      (if (not (string= (buffer-substring 1 2) "#"))
			  (user-error "Bad binary format"))
		      ;; Wait until buffer has "#N.{N}"
		      (let ((size-len (string-to-number (buffer-substring 2 3))))
			(while (< (buffer-size) (+ size-len 2))
			  (accept-data name))
			;; I've got the header, go ahead
			(let ((data-len (string-to-number
					 (buffer-substring 3 (+ 3 size-len)))))
			  ;; wait until all data has arrived
			  (while (< (buffer-size) (+ data-len size-len 2))
			    (accept-data name)))
			(delete-region 1 (+ size-len 3)))
		      (with-current-buffer (find-file-noselect (car args))
			(set-buffer-file-coding-system 'binary)
			(setq inhibit-read-only t)
			(erase-buffer)
			(insert-buffer-substring (process-buffer (eval name)))
			(save-buffer)
			(kill-buffer))))
		   (:terminate (let (n(buffer (process-buffer (eval name))))
				 (delete-process (eval name))
				 (kill-buffer buffer))
			       (fmakunbound name)
			       (makunbound name))
		   (_ (user-error "Invalid scpi method"))))))
  (put name 'function-documentation scpi-doc-str))

(provide 'scpi)
;;; scpi.el ends here

