;;; onc-rpc.el --- ONC RPC for EMACS -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:

;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(require 'bindat)

(defun bindat-get-field-n (struct &rest fields)
  (if (and (listp struct) fields)
      (apply 'bindat-get-field-n (alist-get (car fields) struct)
	     (cdr fields))
    (if fields nil struct)))

(load "rpc.x")
(load "portmap.x")
(load "rpcbind.x")

(defconst rpc-max-pdu-size #xFFF0
  "Maximum record size, total package is this +4.")

(defconst rpc-timeout 1
  "Maximum wait time for incomplete data.")

(defconst rpc-rm-record '((mark u32)
			  (final eval (> (logand last #x80000000) 0))
			  (len eval (logand last #x7FFFFFFF))
			  (data vec (eval (logand last #x7FFFFFFF)))))

(defconst rpc-rm-head '((mark u32)
			(final eval (> (logand last #x80000000) 0))
			(len eval (logand last #x7FFFFFFF))))

(defun rpc-send-rm (proc data)
  "Send data to process splitted with RM in rpc-max-pdu-size chunks"
  (if (> (length data) rpc-max-pdu-size)
      (let ((chunk (substring data 0 rpc-max-pdu-size)))
	(process-send-string proc
			     (bindat-pack rpc-rm-record
					  `((mark . ,rpc-max-pdu-size)
					    (data . ,chunk))))
	(rpc-send-rm proc (substring data rpc-max-pdu-size)))
    (process-send-string proc
			 (bindat-pack rpc-rm-record
				      `((mark . ,(logior (length data)
							 #x80000000))
					(data . ,data))))))

(defun rpc-recv-rm (proc &optional buff)
  (with-current-buffer (process-buffer proc)
    (while (< (buffer-size) 4)
      (unless (accept-process-output proc rpc-timeout 0 t)
	(user-error "Time out in RPC communication")))
    (let* ((head (bindat-unpack rpc-rm-head
				(string-to-unibyte
				 (delete-and-extract-region 1 5))))
	   (len (bindat-get-field head 'len))
	   (final (bindat-get-field head 'final)))
      (while (> len (buffer-size))
	(unless (accept-process-output proc rpc-timeout 0 t)
	  (user-error "Time out in RPC communication")))
      (if final
	  (string-to-unibyte (concat buff (delete-and-extract-region 1 (1+ len))))
	(rpc-recv-rm proc (delete-and-extract-region 1 (1+ len)))))))
;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; RPC CORE
(defun rpc-call (process xid prog vers proc args)
  (rpc-send-rm process
	       (concat (bindat-pack rpc-rpc_msg
				    `((xid . ,xid)
				      (mtype . ,RPC-CALL)
				      (cbody . ((rpcvers . 2)
						(prog . ,prog)
						(vers . ,vers)
						(proc . ,proc)
						(cred . ((flavor . 0)
							 (body-len . 0)
							 (body . [])))
						(verf . ((flavor . 0)
							 (body-len . 0)
							 (body . [])))))))
		       args))
  (bindat-unpack rpc-rpc_msg (rpc-recv-rm process)))
;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; PORTMAP
;; Stil very incomplete...

(defun portmap-getport (proc prog vers prot port)
  "Implements getport function of PORTMAP protocol."
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (let ((rpc-rply
	   (rpc-call proc 1 100000 2 PMAP-PMAPPROC_GETPORT
		     (bindat-pack pmap-mapping
				  `((prog . ,prog)
				    (vers . ,vers)
				    (prot . ,prot)
				    (port . ,port)))))) 
      (bindat-unpack
       '((port u32))
       (or
	(and (= RPC-REPLY (bindat-get-field-n rpc-rply 'mtype))
	     (= RPC-MSG_ACCEPTED (bindat-get-field-n rpc-rply 'rbody 'stat))
	     (= RPC-SUCCESS (bindat-get-field-n rpc-rply 'rbody 'areply 'stat))
	     (bindat-get-field-n rpc-rply 'rbody 'areply 'results 'data))
	(user-error "RPC error, returned: %s" rpc-rply))))))

(defun portmap-dump (proc)
  "Implements dump function of PORTMAP protocol."
  (with-current-buffer (process-buffer proc)
    (erase-buffer)
    (let ((rpc-rply
	   (rpc-call proc 1 100000 2 PMAP-PMAPPROC_DUMP "")))
      (bindat-unpack
       pmap-pmaplist
       (or
	(and (= RPC-REPLY (bindat-get-field-n rpc-rply 'mtype))
	     (= RPC-MSG_ACCEPTED (bindat-get-field-n rpc-rply 'rbody 'stat))
	     (= RPC-SUCCESS (bindat-get-field-n rpc-rply 'rbody 'areply 'stat))
	     (bindat-get-field-n rpc-rply 'rbody 'areply 'results 'data))
	(user-error "RPC error, returned: %s" rpc-rply))))))
		    
;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; BINDP
;; To be done...

(provide 'onc-rpc)
;;; onc-rpc.el ends here

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------

;;;; TEST

;; (setq rpc-p (open-network-stream "rpc" " *rpc*" "192.168.17.125" 111))

;; (portmap-dump rpc-p)

;; (portmap-getport rpc-p 100000 1 PMAP-IPPROTO_TCP 0)

;; (let ((port (bindat-get-field
;; 	     (portmap-getport rpc-p #x0607AF 1 PMAP-IPPROTO_TCP 0)
;; 	     'port)))
;;   (setq vxi-11-p (open-network-stream "vxi-11" " *vxi-11*" "192.168.17.125" port)))

;; (delete-process "rpc")
;; (delete-process "vxi-11")
