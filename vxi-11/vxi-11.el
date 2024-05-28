;;; vxi-11.el --- VXI-11 driver for EMACS -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:

;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(require 'onc-rpc)

(load "vxi-11.x")

(defun vxi-create-link (proc device)
  "Implements create-link function of VXI-11."
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (let ((rpc-rply
	   (rpc-call proc 0 #x0607AF 1 vxi-create_link
		     (bindat-pack vxi-Create_LinkParms
		      		  `((clientId . 0)
		      		    (lockDevice . 0)
		      		    (lock_timeout . 0)
		      		    (device-len . ,(length device))
		      		    (device . ,(vconcat device)))))))
      (bindat-unpack
       vxi-Create_LinkResp
       (or
	(and (= RPC-REPLY (bindat-get-field-n rpc-rply 'mtype))
	     (= RPC-MSG_ACCEPTED (bindat-get-field-n rpc-rply 'rbody 'stat))
	     (= RPC-SUCCESS (bindat-get-field-n rpc-rply 'rbody 'areply 'stat))
	     (bindat-get-field-n rpc-rply 'rbody 'areply 'results 'data))
  	(user-error "RPC error, returned: %s" rpc-rply))))))

(defun vxi-device-write (proc lid tout lock-tout flags data)
  "Implements device-write function of VXI-11."
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (let ((rpc-rply
	   (rpc-call proc 0 #x0607AF 1 vxi-device_write
		     (bindat-pack vxi-Device_WriteParms
		      		  `((lid . ,lid)
		      		    (io_timeout . ,tout)
		      		    (lock_timeout . ,lock-tout)
				    (flags . ,flags)
		      		    (data-len . ,(length data))
		      		    (data . ,(vconcat data)))))))
      (bindat-unpack
       vxi-Device_WriteResp
       (or
	(and (= RPC-REPLY (bindat-get-field-n rpc-rply 'mtype))
	     (= RPC-MSG_ACCEPTED (bindat-get-field-n rpc-rply 'rbody 'stat))
	     (= RPC-SUCCESS (bindat-get-field-n rpc-rply 'rbody 'areply 'stat))
	     (bindat-get-field-n rpc-rply 'rbody 'areply 'results 'data))
	(user-error "RPC error, returned: %s" rpc-rply))))))

(defun vxi-device-read (proc lid size tout lock-tout flags term)
  "Implements device-read function of VXI-11."
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (let ((rpc-rply
	   (rpc-call proc 0 #x0607AF 1 vxi-device_read
		     (bindat-pack vxi-Device_ReadParms
		      		  `((lid . ,lid)
				    (requestSize . , size)
		      		    (io_timeout . ,tout)
		      		    (lock_timeout . ,lock-tout)
				    (flags . ,flags)
		      		    (termChar . ,term))))))
      (bindat-unpack
       vxi-Device_ReadResp
       (or
	(and (= RPC-REPLY (bindat-get-field-n rpc-rply 'mtype))
	     (= RPC-MSG_ACCEPTED (bindat-get-field-n rpc-rply 'rbody 'stat))
	     (= RPC-SUCCESS (bindat-get-field-n rpc-rply 'rbody 'areply 'stat))
	     (bindat-get-field-n rpc-rply 'rbody 'areply 'results 'data))
	(user-error "RPC error, returned: %s" rpc-rply))))))

(provide 'vxi-11)
;;; vxi-11.el ends here

;;;; TEST

;; (setq rpc-p (open-network-stream "rpc" " *rpc*" "192.168.17.125" 111))

;; (let ((port (bindat-get-field
;; 	     (portmap-getport rpc-p #x0607AF 1 PMAP-IPPROTO_TCP 0)
;; 	     'port)))
;;   (setq vxi-11-p (open-network-stream "vxi-11" " *vxi-11*" "192.168.17.125" port)))

;; (vxi-create-link vxi-11-p "gpib0,10")
;; (vxi-create-link vxi-11-p "gpib0,23")

;; (vxi-device-write vxi-11-p 404056 1000 1000 8 "CONF:RES")
;; (concat (bindat-get-field 
;; 	 (vxi-device-read vxi-11-p 404056 1000 1000 1000 8 0)
;; 	 'data))

;; (string-to-number (concat (bindat-get-field 
;; 			   (vxi-device-read vxi-11-p 404056 1000 1000 1000 8 0)
;; 			   'data)))

;; (vxi-device-write vxi-11-p 403072 1000 1000 8 "*IDN?")
;; (concat (bindat-get-field 
;; 	 (vxi-device-read vxi-11-p 403072 1000 1000 1000 8 0)
;; 	 'data))

