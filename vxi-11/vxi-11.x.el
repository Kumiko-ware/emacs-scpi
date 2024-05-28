;; typedef long Device_Link;

;; enum Device_AddrFamily { 		/* used by interrupts */
;;   DEVICE_TCP,
;;   DEVICE_UDP
;; };
;; typedef long Device_Flags;

(defconst VXI-DEVICE_TCP 0)
(defconst VXI-DEVICE_UDP 1)

;; typedef long Device_ErrorCode;
;; struct Device_Error {
;;   Device_ErrorCode error;
;; };

(defconst vxi-device-error '((errno u32)))

;; struct Create_LinkParms {
;;   long clientId; 			/* implementation specific value */
;;   bool lockDevice; 			/* attempt to lock the device */
;;   unsigned long lock_timeout; 		/* time to wait on a lock */
;;   string device<>; 			/* name of device */
;; };

(defconst vxi-Create_LinkParms '((clientId u32)
				 (lockDevice u32)
				 (lock_timeout u32)
				 (device-len u32)
				 (device vec (device-len))
				 (align 4)))

;; struct Create_LinkResp {
;;   Device_ErrorCode error;
;;   Device_Link lid;
;;   unsigned short abortPort; 		/* for the abort RPC */
;;   unsigned long maxRecvSize; 		/* specifies max data size in bytes
;; 					device will accept on a write */
;; };

(defconst vxi-Create_LinkResp '((errno u32)
				(lid u32)
				(abortPort u32)
				(maxRecvSize u32)))

;; struct Device_WriteParms {
;;   Device_Link lid; 			/* link id from create_link */
;;   unsigned long io_timeout; 		/* time to wait for I/O */
;;   unsigned long lock_timeout; 		/* time to wait for lock */
;;   Device_Flags flags;
;;   opaque data<>; 			/* the data length and the data itself */
;; };

(defconst vxi-Device_WriteParms '((lid u32)
				  (io_timeout u32)
				  (lock_timeout u32)
				  (flags u32)
				  (data-len u32)
				  (data vec (data-len))
				  (align 4)))


;; struct Device_WriteResp {
;;   Device_ErrorCode error;
;;   unsigned long size; 			/* Number of bytes written */
;; };

(defconst vxi-Device_WriteResp '((errno u32)
				 (size u32)))

;; struct Device_ReadParms {
;;   Device_Link lid; 			/* link id from create_link */
;;   unsigned long requestSize; 		/* Bytes requested */
;;   unsigned long io_timeout; 		/* time to wait for I/O */
;;   unsigned long lock_timeout; 		/* time to wait for lock */
;;   Device_Flags flags;
;;   char termChar; 			/* valid if flags & termchrset */
;; };

(defconst vxi-Device_ReadParms '((lid u32)
				 (requestSize u32)
				 (io_timeout u32)
				 (lock_timeout u32)
				 (flags u32)
				 (termChar u32)))

;; struct Device_ReadResp {
;;   Device_ErrorCode error;
;;   long reason; 				/* Reason(s) read completed */
;;   opaque data<>; 			/* data.len and data.val */
;; };

(defconst vxi-Device_ReadResp '((errno u32)
				(reason u32)
				(data-len u32)
				(data vec (data-len))
				(align 4)))

;; struct Device_ReadStbResp {
;;   Device_ErrorCode error; 		/* error code */
;;   unsigned char stb; 			/* the returned status byte */
;; };

(defconst vxi-Device_ReadStbResp '((errno u32)
				   (stb u32)))

;; struct Device_GenericParms {
;;   Device_Link lid; 			/* Device_Link id from connect call */
;;   Device_Flags flags; 			/* flags with options */
;;   unsigned long lock_timeout; 		/* time to wait for lock */
;;   unsigned long io_timeout; 		/* time to wait for I/O */
;; };

(defconst vxi-Device_GenericParms '((lid u32)
				    (flags u32)
				    (lock_timeout u32)
				    (io_timeout u32)))

;; struct Device_RemoteFunc {
;;   unsigned long hostAddr; 		/* Host servicing Interrupt */
;;   unsigned short hostPort; 		/* valid port # on client */
;;   unsigned long progNum; 		/* DEVICE_INTR */
;;   unsigned long progVers; 		/* DEVICE_INTR_VERSION */
;;   Device_AddrFamily progFamily; 	/* DEVICE_UDP | DEVICE_TCP */
;; };

(defconst vxi-Device_RemoteFunc '((hostAddr u32)
				  (hostPort u32)
				  (progNum u32)
				  (progVers u32)
				  (progFamily u32)))

;; struct Device_EnableSrqParms {
;;   Device_Link lid;
;;   bool enable; 				/* Enable or disable interrupts */
;;   opaque handle<40>; 			/* Host specific data */
;; };

(defconst vxi-Device_EnableSrqParms '((lid u32)
				      (enable u32)
				      (handle-len u32)
				      (handle vec (handle-len))
				      (align 4)))

;; struct Device_LockParms {
;;   Device_Link lid; 			/* link id from create_link */
;;   Device_Flags flags; 			/* Contains the waitlock flag */
;;   unsigned long lock_timeout; 		/* time to wait to acquire lock */
;; };

(defconst vxi-Device_LockParms '((lid u32)
				 (flags u32)
				 (lock_timeout u32)))

;; struct Device_DocmdParms {
;;   Device_Link lid; 			/* link id from create_link */
;;   Device_Flags flags; 			/* flags specifying various options */
;;   unsigned long io_timeout; 		/* time to wait for I/O to complete */
;;   unsigned long lock_timeout; 		/* time to wait on a lock */
;;   long cmd; 				/* which command to execute */
;;   bool network_order; 			/* client's byte order */
;;   long datasize; 			/* size of individual data elements */
;;   opaque data_in<>; 			/* docmd data parameters */
;; };

(defconst vxi-Device_DocmdParms '((lid u32)
				  (flags u32)
				  (io_timeout u32)
				  (lock_timeout u32)
				  (cmd u32)
				  (network_order u32)
				  (datasize u32)
				  (data-len u32)
				  (data vec (data-len))
				  (align 4)))

;; struct Device_DocmdResp {
;;   Device_ErrorCode error; 		/* returned status */
;;   opaque data_out<>; 			/* returned data parameter */
;; };

(defconst vxi-Device_DocmdResp '((errno u32)
				 (data-len u32)
				 (data vec (data-len))
				 (align 4)))

;; program DEVICE_ASYNC{
;;   version DEVICE_ASYNC_VERSION {
;;     Device_Error device_abort (Device_Link) = 1;
;;   } = 1;
;; } = 0x0607B0;

(defconst vxi-device_abort 1)

;; program DEVICE_CORE {
;;   version DEVICE_CORE_VERSION {
;;     Create_LinkResp create_link (Create_LinkParms) = 10;
;;     Device_WriteResp device_write (Device_WriteParms) = 11;
;;     Device_ReadResp device_read (Device_ReadParms) = 12;
;;     Device_ReadStbResp device_readstb (Device_GenericParms) = 13;
;;     Device_Error device_trigger (Device_GenericParms) = 14;
;;     Device_Error device_clear (Device_GenericParms) = 15;
;;     Device_Error device_remote (Device_GenericParms) = 16;
;;     Device_Error device_local (Device_GenericParms) = 17;
;;     Device_Error device_lock (Device_LockParms) = 18;
;;     Device_Error device_unlock (Device_Link) = 19;
;;     Device_Error device_enable_srq (Device_EnableSrqParms) = 20;
;;     Device_DocmdResp device_docmd (Device_DocmdParms) = 22;
;;     Device_Error destroy_link (Device_Link) = 23;
;;     Device_Error create_intr_chan (Device_RemoteFunc) = 25;
;;     Device_Error destroy_intr_chan (void) = 26;
;;   } = 1;
;; } = 0x0607AF;

(defconst vxi-create_link 10)
(defconst vxi-device_write 11)
(defconst vxi-device_read 12)
(defconst vxi-device_readstb 13)
(defconst vxi-device_trigger 14)
(defconst vxi-device_clear 15)
(defconst vxi-device_remote 16)
(defconst vxi-device_local 17)
(defconst vxi-device_lock 18)
(defconst vxi-device_unlock 19)
(defconst vxi-device_enable_srq 20)
(defconst vxi-device_docmd 22)
(defconst vxi-destroy_link 23)
(defconst vxi-create_intr_chan 25)
(defconst vxi-destroy_intr_chan 26)

;; struct Device_SrqParms {
;;   opaque handle<>;
;; };

(defconst vxi-Device_SrqParms '((handle-len u32)
				(handle vec (handle-len))
				(align 4)))

;; program DEVICE_INTR {
;;   version DEVICE_INTR_VERSION {
;;     void device_intr_srq (Device_SrqParms) = 30;
;;   }=1;
;; }= 0x0607B1;

(defconst vxi-device_intr_srq 30)
