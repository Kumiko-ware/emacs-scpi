;; const PMAP_PORT = 111;      /* portmapper port number */	

(defconst PMAP-PMAP_PORT 111)

;; struct mapping {
;;   unsigned int prog;
;;   unsigned int vers;
;;   unsigned int prot;
;;   unsigned int port;
;; };

(defconst pmap-mapping '((prog u32)
			 (vers u32)
			 (prot u32)
			 (port u32)))

;; const IPPROTO_TCP = 6;      /* protocol number for TCP/IP */
;; const IPPROTO_UDP = 17;     /* protocol number for UDP/IP */

(defconst PMAP-IPPROTO_TCP 6)
(defconst PMAP-IPPROTO_UDP 17)

;; struct *pmaplist {
;;   mapping map;
;;   pmaplist next;
;; };

;; so should it be:
;;(defconst pmaplist '((map struct map)
;;		     (next u32)))

;; but turns out to be:
(defconst pmap-pmaplist '((next u32)
			  (union (next)
				 (1 (map struct pmap-mapping)
				    (ptr  struct pmap-pmaplist))
				 (0 ))))

;; struct call_args {
;;   unsigned int prog;
;;   unsigned int vers;
;;   unsigned int proc;
;;   opaque args<>;
;; };

(defconst pmap-call_args '((prog u32)
			   (vers u32)
			   (proc u32)
			   (args-len u32)
			   (args vec (args-len))))

;; struct call_result {
;;   unsigned int port;
;;   opaque res<>;
;; };

(defconst pmap-call_result '((port u32)
			     (res-len u32)
			     (res vec (res-len))))

;; program PMAP_PROG {
;;   version PMAP_VERS {
;;     void
;;       PMAPPROC_NULL(void)         = 0;

;;     bool
;;       PMAPPROC_SET(mapping)       = 1;

;;     bool
;;       PMAPPROC_UNSET(mapping)     = 2;

;;     unsigned int
;;       PMAPPROC_GETPORT(mapping)   = 3;

;;     pmaplist
;;             PMAPPROC_DUMP(void)         = 4;

;;     call_result

;;       PMAPPROC_CALLIT(call_args)  = 5;
;;   } = 2;
;; } = 100000;

(defconst PMAP-PMAP_PROG 100000)
(defconst PMAP-PMAP_VERS 2)
(defconst PMAP-PMAPPROC_NULL 0)
(defconst PMAP-PMAPPROC_SET 1)
(defconst PMAP-PMAPPROC_UNSET 2)
(defconst PMAP-PMAPPROC_GETPORT 3)
(defconst PMAP-PMAPPROC_DUMP 4)
(defconst PMAP-PMAPPROC_CALLIT 5)
