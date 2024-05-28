;; enum msg_type {
;;   CALL  = 0,
;;   REPLY = 1
;; };

(defconst RPC-CALL 0)
(defconst RPC-REPLY 1)

;; enum reply_stat {
;;   MSG_ACCEPTED = 0,
;;   MSG_DENIED   = 1
;; };

(defconst RPC-MSG_ACCEPTED 0)
(defconst RPC-MSG_DENIED 1)

;; enum accept_stat {
;;   SUCCESS       = 0, /* RPC executed successfully       */
;;   PROG_UNAVAIL  = 1, /* remote hasn't exported program  */
;;   PROG_MISMATCH = 2, /* remote can't support version #  */
;;   PROC_UNAVAIL  = 3, /* program can't support procedure */
;;   GARBAGE_ARGS  = 4, /* procedure can't decode params   */
;;   SYSTEM_ERR    = 5  /* e.g. memory allocation failure  */
;; };

(defconst RPC-SUCCESS 0)
(defconst RPC-PROG_UNAVAIL 1)
(defconst RPC-PROG_MISMATCH 2)
(defconst RPC-PROC_UNAVAIL 3)
(defconst RPC-GARBAGE_ARGS 4)
(defconst RPC-SYSTEM_ERR 5)

;; enum reject_stat {
;;   RPC_MISMATCH = 0, /* RPC version number != 2          */
;;   AUTH_ERROR = 1    /* remote can't authenticate caller */
;; };

(defconst RPC-RPC_MISMATCH 0)
(defconst RPC-AUTH_ERROR 1)

;; enum auth_flavor {
;;   AUTH_NONE       = 0,
;;   AUTH_SYS        = 1,
;;   AUTH_SHORT      = 2,
;;   AUTH_DH         = 3,
;;   RPCSEC_GSS      = 6
;;   /* and more to be defined */
;; };

(defconst RPC-AUTH_NONE 0)
(defconst RPC-AUTH_SYS 1)
(defconst RPC-AUTH_SHORT 2)
(defconst RPC-AUTH_DH 3)
(defconst RPC-RPCSEC_GSS 6)

;; struct opaque_auth {
;;   auth_flavor flavor;
;;   opaque body<400>;
;; };

(defconst rpc-opaque_auth '((flavor u32)
			    (body-len u32)
			    (body vec (body-len))))

;; enum auth_stat {
;;   AUTH_OK           = 0,  /* success                        */
;;   /*
;;    * failed at remote end
;;    */
;;   AUTH_BADCRED      = 1,  /* bad credential (seal broken)   */
;;   AUTH_REJECTEDCRED = 2,  /* client must begin new session  */
;;   AUTH_BADVERF      = 3,  /* bad verifier (seal broken)     */
;;   AUTH_REJECTEDVERF = 4,  /* verifier expired or replayed   */
;;   AUTH_TOOWEAK      = 5,  /* rejected for security reasons  */
;;   /*
;;    * failed locally
;;    */
;;   AUTH_INVALIDRESP  = 6,  /* bogus response verifier        */
;;   AUTH_FAILED       = 7,  /* reason unknown                 */
;;   /*
;;    * AUTH_KERB errors; deprecated.  See [RFC2695]
;;    */
;;   AUTH_KERB_GENERIC = 8,  /* kerberos generic error */
;;   AUTH_TIMEEXPIRE = 9,    /* time of credential expired */
;;   AUTH_TKT_FILE = 10,     /* problem with ticket file */
;;   AUTH_DECODE = 11,       /* can't decode authenticator */
;;   AUTH_NET_ADDR = 12,     /* wrong net address in ticket */
;;   /*
;;    * RPCSEC_GSS GSS related errors
;;    */
;;   RPCSEC_GSS_CREDPROBLEM = 13, /* no credentials for user */
;;   RPCSEC_GSS_CTXPROBLEM = 14   /* problem with context */
;; };

(defconst RPC-AUTH_OK 0)
(defconst RPC-AUTH_BADCRED 1)
(defconst RPC-AUTH_REJECTEDCRED 2)
(defconst RPC-AUTH_BADVERF 3)
(defconst RPC-AUTH_REJECTEDVERF 4)
(defconst RPC-AUTH_TOOWEAK 5)
(defconst RPC-AUTH_INVALIDRESP 6)
(defconst RPC-AUTH_FAILED 7)
(defconst RPC-AUTH_KERB_GENERIC 8)
(defconst RPC-AUTH_TIMEEXPIRE 9)
(defconst RPC-AUTH_TKT_FILE 10)
(defconst RPC-AUTH_DECODE 11)
(defconst RPC-AUTH_NET_ADDR 12)
(defconst RPC-RPCSEC_GSS_CREDPROBLEM 13)
(defconst RPC-RPCSEC_GSS_CTXPROBLEM 14)

;; struct rpc_msg {
;;   unsigned int xid;
;;   union switch (msg_type mtype) {
;;   case CALL:
;;     call_body cbody;
;;   case REPLY:
;;     reply_body rbody;
;;   } body;
;; };

(defconst rpc-rpc_msg `((xid u32)
			(mtype u32)
			(union (mtype)
			       (,RPC-CALL (cbody struct rpc-call_body))
			       (,RPC-REPLY (rbody struct rpc-reply_body)))))

;; struct call_body {
;;   unsigned int rpcvers;       /* must be equal to two (2) */
;;   unsigned int prog;
;;   unsigned int vers;
;;   unsigned int proc;
;;   opaque_auth cred;
;;   opaque_auth verf;
;;   /* procedure-specific parameters start here */
;; };

(defconst rpc-call_body '((rpcvers u32)
			  (prog u32)
			  (vers u32)
			  (proc u32)
			  (cred struct rpc-opaque_auth)
			  (verf struct rpc-opaque_auth)))

;; union reply_body switch (reply_stat stat) {
;;  case MSG_ACCEPTED:
;;    accepted_reply areply;
;;  case MSG_DENIED:
;;    rejected_reply rreply;
;;  } reply;

(defconst rpc-reply_body `((stat u32)
			   (union (stat)
				  (,RPC-MSG_ACCEPTED (areply struct rpc-accepted_reply))
				  (,RPC-MSG_DENIED (rreply struct rpc-rejected_reply)))))

;; struct accepted_reply {
;;   opaque_auth verf;
;;   union switch (accept_stat stat) {
;;   case SUCCESS:
;;     opaque results[0];
;;     /*
;;      * procedure-specific results start here
;;      */
;;   case PROG_MISMATCH:
;;     struct {
;;       unsigned int low;
;;       unsigned int high;
;;     } mismatch_info;
;;   default:
;;     /*
;;      * Void.  Cases include PROG_UNAVAIL, PROC_UNAVAIL,
;;      * GARBAGE_ARGS, and SYSTEM_ERR.
;;      */
;;     void;
;;   } reply_data;
;; };

(defconst rpc-mismatch_info_struct '((low u32)
				     (high u32)))

(defconst rpc-results '((len eval (- (length bindat-raw) bindat-idx))
		       (data vec (len))))

(defconst rpc-accepted_reply `((verf struct rpc-opaque_auth)
			       (stat u32)
			       (union (stat)
				      (,RPC-SUCCESS (results struct rpc-results))
				      (,RPC-PROG_MISMATCH (mismatch_info struct rpc-mismatch_info_struct))
				      (- ))))

;; union rejected_reply switch (reject_stat stat) {
;;  case RPC_MISMATCH:
;;    struct {
;;      unsigned int low;
;;      unsigned int high;
;;    } mismatch_info;
;;  case AUTH_ERROR:
;;    auth_stat stat;
;; };

(defconst rpc-rejected_reply `((stat u32)
			       (union (stat)
				      (,RPC-RPC_MISMATCH (mismatch_info struct rpc-mismatch_info_struct))
				      (,RPC-AUTH_ERROR (auth_stat u32)))))


;;;; USAGE EXAMPLES
;; (require 'bindat)

;; ;;call
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,CALL)
;; 			      (cbody . ((rpcvers . 2)
;; 					(prog . 100000)
;; 					(vers . 1)
;; 					(proc . 10)
;; 					(cred . ((flavor . 0)
;; 						 (body-len . 0)
;; 						 (body . "")))
;; 					(verf . ((flavor . 0)
;; 						 (body-len . 0)
;; 						 (body . ""))))))) )

;; ;;accepted-reply success
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,REPLY)
;; 			      (rbody . ((stat . ,MSG_ACCEPTED)
;; 					(areply . ((verf . ((flavor . 0)
;; 							    (body-len . 0)
;; 							    (body . "")))
;; 						   (stat . ,SUCCESS)
;; 						   (results . []))))))))

;; (setq recv-data (concat (bindat-pack rpc_msg
;; 				`((xid . #xBEBACAFE)
;; 				  (mtype . ,REPLY)
;; 				  (rbody . ((stat . ,MSG_ACCEPTED)
;; 					    (areply . ((verf . ((flavor . 0)
;; 								(body-len . 0)
;; 								(body . "")))
;; 						       (stat . ,SUCCESS)
;; 						       (results . [])))))))
;; 		   "DATA DATA DATA"))

;; (setq paylod (substring recv-data (length (bindat-pack rpc_msg (bindat-unpack rpc_msg recv-data)))))

;; ;;accepted-reply prog_mismatch
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,REPLY)
;; 			      (rbody . ((stat . ,MSG_ACCEPTED)
;; 					(areply . ((verf . ((flavor . 0)
;; 							    (body-len . 0)
;; 							    (body . "")))
;; 						   (stat . ,PROG_MISMATCH)
;; 						   (mismatch_info . ((low . 100)
;; 								     (high . 200))))))))))
;; ;;accepted-reply prog_unavailable
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,REPLY)
;; 			      (rbody . ((stat . ,MSG_ACCEPTED)
;; 					(areply . ((verf . ((flavor . 0)
;; 							    (body-len . 0)
;; 							    (body . "")))
;; 						   (stat . ,PROG_UNAVAIL))))))))

;; ;;rejected-reply rpc_msimatch
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,REPLY)
;; 			      (rbody . ((stat . ,MSG_DENIED)
;; 					(rreply . ((stat . ,RPC_MISMATCH)
;; 						   (mismatch_info . ((low . 10)
;; 								     (high . 20))))))))))

;; ;;rejected-reply auth_error
;; (bindat-unpack rpc_msg
;; 	       (bindat-pack rpc_msg
;; 			    `((xid . #xBEBACAFE)
;; 			      (mtype . ,REPLY)
;; 			      (rbody . ((stat . ,MSG_DENIED)
;; 					(rreply . ((stat . ,AUTH_ERROR)
;; 						   (auth_stat . 88))))))))
