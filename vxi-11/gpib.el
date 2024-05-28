;;; gpib.el --- VXI-11 interface for GPIB gateway -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:

;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(require 'vxi-11)

(defconst gpib-timeout 1000)

(defconst gpib-doc-str
  "GPIB command interface.
Usage:
Simple commands: (NAME \"command string\" ARGS)
The command string can contain format specifications according to
the format function. For placing a '%' you have to use a double
sign '%%'.

Other commands:
\(NAME :terminate) stops the process and deletes the object."
  "Documentation string for the created objects")

(defun gpib-new (name ip gpib)
  "Creates a GPIB connection through gateway IP, addr:GPIB.
NAME is the name of the object to be created IP is the ip address
of the gateway, GPIB is the addess of the instrument in the bus
expressed als gpibX,Y in which X is the address and Y the
subaddress."
  (unless (and (symbolp name) (stringp ip) (stringp gpib))
    (user-error "scpi-new: bad arguments"))
  (if (or (functionp name) (boundp name))
      (user-error "%s already exist or defined" name))
  (let* ((rpc-p (open-network-stream "rpc"
				     " *rpc*"
				     ip 111))
	 (port (bindat-get-field
		(portmap-getport rpc-p #x0607AF 1 PMAP-IPPROTO_TCP 0)
		'port))
	 (vxi-11-p (open-network-stream "vxi-11"
					(generate-new-buffer-name " *vxi-11*")
					ip port))
	 (link (vxi-create-link vxi-11-p gpib)))
    (set name vxi-11-p)
    (let ((buffer (process-buffer rpc-p)))
      (delete-process rpc-p)
      (kill-buffer buffer))
    (let ((errno (bindat-get-field link 'errno))
	  (lid (bindat-get-field link 'lid))
	  (abort-port (bindat-get-field link 'abortPort))
	  (max-size (bindat-get-field link 'maxRecvSize)))
      (when (or (/= 0 errno) (= 0 lid))
	(let ((buffer (process-buffer (eval name))))
	  (delete-process (eval name))
	  (kill-buffer buffer))
	(makunbound name)
	(user-error "gpib-new: create connection failed."))
      (fset name (lambda (method &rest args)
		   (pcase method
		     ((pred stringp)
		      (vxi-device-write (eval name)
					(eval lid)
					gpib-timeout
					gpib-timeout
					8
					(format method args))
		      (if (string-match "\?" method)
			  (let ((not-ready t) buffer)
			    (while not-ready
			      (let ((result (vxi-device-read (eval name)
							     (eval lid)
							     (eval max-size)
							     gpib-timeout
							     gpib-timeout
							     8
							     0)))
				(let ((errno (bindat-get-field result 'errno))
				      (reason (bindat-get-field result 'reason))
				      (data (bindat-get-field result 'data)))
				  (when (/= 0 errno)
				    (user-error "gpib: reading failed."))
				  (when (or (= 4 reason) (= 2 reason))
				    (setq not-ready nil))
				  (setq buffer (concat buffer data)))))
			    buffer)
			""))
		     (:read-only
		      (let ((not-ready t) buffer)
			(while not-ready
			  (let ((result (vxi-device-read (eval name)
							 (eval lid)
							 (eval max-size)
							 gpib-timeout
							 gpib-timeout
							 8
							 0)))
			    (let ((errno (bindat-get-field result 'errno))
				  (reason (bindat-get-field result 'reason))
				  (data (bindat-get-field result 'data)))
			      (when (/= 0 errno)
				(user-error "gpib: reading failed."))
			      (when (or (= 4 reason) (= 2 reason))
				(setq not-ready nil))
			      (setq buffer (concat buffer data)))))
			buffer))
		     (:terminate (let ((buffer (process-buffer (eval name))))
				   (delete-process (eval name))
				   (kill-buffer buffer))
				 (fmakunbound name)
				 (makunbound name))
		     (_ (user-error "Invalid gpib method"))))))
    (put name 'function-documentation gpib-doc-str)))

(provide 'gpib)
;;; gpib.el ends here

;;;; TEST


;; (require 'gpib)

;; (gpib-new 'gen "192.168.17.125" "gpib0,10")

;; (gen "*IDN?")

;; (gen :terminate)
