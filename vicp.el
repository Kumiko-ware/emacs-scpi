;;; vicp.el --- VICP interface for LeCroy Oscilloscope -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:
;;
;; USAGE EXAMPLE:
;;
;; (vicp-new 'osc "192.168.1.1")
;; (osc "%s" "*IDN?")
;; (osc :init-default)
;; (osc :snapshot "./test.png")
;; (osc :cmd-to-file "wave-c1.lwf" "c%d:WF? ALL" 1)
;; (osc :cmd-to-file "wave-c2.lwf" "c%d:WF? ALL" 2)
;; (osc :cmd-to-file "wave-c2.lwf" "c%d:WF? ALL" 3)
;; (osc :cmd-to-file "wave-c2.lwf" "c%d:WF? ALL" 4)
;; (osc :terminate)
;;
;; Recommended use: STOP [CONFIG] OPC? [ARM or TRMD SINGLE] WAIT [Read data]
;; Ftom page 36 of LECROY X-STREAM OSCILLOSCOPES - REMOTE CONTROL MANUAL - FEB05
;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(require 'bindat)

(defconst vicp-doc-str
  "VICP command interface.
Usage:
Simple SCPI commands: (NAME \"command string\" ARGS)
The command string can contain format specifications according to
the format function. For placing a '%' you have to use a double
sign '%%'.

Other commands: 
\(NAME :init-default) sets the oscilloscope to factory defaults.
\(NAME :snapshot <file>) takes a snapshot from oscilloscope and
saves it in the named file.
\(NAME :cmd-to-file <file> CMD ARGS) sends the command to the
oscilloscope formatted with ARGS and saves the reply (possibly
binary) into the named file.
\(NAME :param-val CH \"NAME\") get one of the available
parameters from PAVA command from channel CH.
\(NAME :label CH \"STRING\" \"POS\") puts the label STRING in
channel CH at time position POS. If STRING is the empty string,
the label is disabled.
\(NAME :terminate) stops the process and deletes the object."
  "Documentation string for the created objects")

(defconst vicp-record '((op u8)
			(ver u8)
			(seq u8)
			(res u8)
			(len u32))
  "Record structure of VICP protocol")
(defconst vicp-data-flag 128)
(defconst vicp-remote-flag 64)
(defconst vicp-lockout-flag 32)
(defconst vicp-clear-flag 16)
(defconst vicp-srq-flag 8)
(defconst vicp-serialpoll-flag 4)
(defconst vicp-reserved-flag 2)
(defconst vicp-eoi-flag 1)
(defconst vicp-time-out 10
  "Seconds for time out when receiving")
(defconst vicp-snapshot-cmd
  "CHDR OFF;HCSU BCKG,BLACK;HCSU DEV,PNG;HCSU PORT,NET;SCDP;*OPC?"
  "Command to send for snapshot")

(let ((vicp-seq 0))
  (defun vicp-get-seq ()
    (if (< vicp-seq 255)
 	(setq vicp-seq (1+ vicp-seq))
      (setq vicp-seq 1))))

(defun vicp-command (proc cmd)
  "Sends CMD string to the connection NAME, waits for VICP reply,
and returns the reply string."
  (process-send-string
   proc
   (concat (bindat-pack vicp-record `((op . ,(+ vicp-data-flag vicp-eoi-flag))
				 (ver . 1)
				 (seq . ,(vicp-get-seq))
				 (res . 0)
				 (len . ,(length cmd))))
	   cmd))
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (if (string-match "?" cmd)
	(let ((nxt-head 1) (not-ready t))
	  (while not-ready
	    (unless (accept-process-output proc vicp-time-out)
	      (user-error
	       "TIMEOUT in communication with %s" (process-name proc)))
	    (while (>= (buffer-size (current-buffer)) (+ nxt-head 8))
	      (let ((header (bindat-unpack vicp-record
					   (delete-and-extract-region
					    nxt-head (+ nxt-head 8)))))
		(setq nxt-head (+ nxt-head (bindat-get-field header 'len)))
		(setq not-ready (/= 1 (logand
				       vicp-eoi-flag
				       (bindat-get-field header 'op)))))))))
    (buffer-string)))

(defun vicp-init-default (osc)
  "Send initialization commands to the oscilloscope PROC"
  (funcall osc "vbs 'app.SetToDefaultSetup'")
  (funcall osc "vbs 'app.ResetPreferences'")
  (funcall osc "vbs 'app.Display.C1Color = &H00FFFF&'")
  (funcall osc "vbs 'app.Display.C2Color = &H0000FF&'")
  (funcall osc "vbs 'app.Display.C3Color = &HFF0000&'")
  (funcall osc "vbs 'app.Display.C4Color = &H00C000&'")
  (funcall osc "GRID SINGLE")
  (funcall osc
	   (concat "vbs app.Utility.DateTimeSetup.Year="
		   (format-time-string "%Y\n")
		   "app.Utility.DateTimeSetup.Month="
		   (format-time-string "%m\n")
		   "app.Utility.DateTimeSetup.Day="
		   (format-time-string "%d\n")
		   "app.Utility.DateTimeSetup.Hour="
		   (format-time-string "%H\n")
		   "app.Utility.DateTimeSetup.Minute="
		   (format-time-string "%M\n")
		   "app.Utility.DateTimeSetup.Second="
		   (format-time-string "%S\n")
		   "app.Utility.DateTimeSetup.Validate\n")))

(defun vicp-new (name addr)
  "Creates a new function NAME for connecting to an oscilloscope with 
ip address IP."
  (unless (and (symbolp name) (stringp addr))
    (user-error "vicp-open-connection: bad arguments"))
  (if (or (functionp name) (boundp name))
      (user-error "%s already exist or defined" name))
  (set name (open-network-stream "vicp"
				 (generate-new-buffer-name " *vicp*")
				 addr 1861))
  (set-process-coding-system (eval name) 'binary 'binary)
  (with-current-buffer (process-buffer (eval name))
    (set-buffer-multibyte nil))
  (fset name (lambda (method &rest args)
	       (with-current-buffer (process-buffer (eval name))
		 (pcase method
		   ((pred stringp)
		    (vicp-command (eval name)
				  (apply 'format method args)))
		   (:init-default (vicp-init-default name))
		   (:snapshot
		    (with-current-buffer (find-file-noselect (car args))
		      (setq  inhibit-read-only t)
		      (insert (funcall name vicp-snapshot-cmd)) 
		      (save-buffer)
		      (kill-buffer)
		      (car args)))
		   (:cmd-to-file
		    (with-current-buffer (find-file-noselect (car args))
		      (set-buffer-file-coding-system 'binary)
		      (setq  inhibit-read-only t)
		      (insert (apply name (cadr args) (cddr args)))
		      (save-buffer)
		      (kill-buffer)))
		   (:param-val
		    (let ((resp (split-string (funcall name
						       (format "C%d:PAVA? %s"
							       (car args)
							       (cadr args)))
					      ",")))
		      (if (string-match "OK" (nth 2 resp))
			  (string-to-number (nth 1 resp))
			(nth 2 resp))))
		   (:label
		    (let ((chan (nth 0 args))
			  (txt (nth 1 args))
			  (pos (nth 2 args)))
		      (if (string= txt "")
			  (funcall
			   name
			   (format
			    "vbs app.Acquisition.C%d.ViewLabels = False"
			    chan))
			(progn
			  (funcall
			   name
			   (format
			    "vbs app.Acquisition.C%d.ViewLabels = True"
			    chan))
			  (funcall
			   name
			   (format
			    "vbs app.Acquisition.C%d.LabelsText = \"%s\""
			    chan txt))
			  (funcall
			   name
			   (format
			    "vbs app.Acquisition.C%d.LabelsPOsition = \"%s\""
			    chan pos))))))
		   (:terminate
		    (ignore-errors
		      (delete-process (eval name))
		      (kill-buffer))
		    (fmakunbound name)
		    (makunbound name))
		   (_ (user-error "Invalid vicp method"))))))
  (put name 'function-documentation vicp-doc-str))

(provide 'vicp)
;;; vicp.el ends here
