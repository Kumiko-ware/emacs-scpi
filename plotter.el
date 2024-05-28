;;; plotter.el --- Interface to GNUplot for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:

;; 

;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(defconst plotter-doc-str
  "Plotter command interface.
Usage:
Simple commands: (NAME \"command string\" ARGS)

The command string can contain format specifications according to
the format function. For placing a '%' you have to use a double
sign '%%'.

Other commands:

\(NAME :wait) waits for pending commands to be done.
\(NAME :terminate) stops the process and deletes the object."
  "Documentation string for the created objects")


(defun plotter-new (name gnu-plot-path)
  "Creates a plotter process based on GNU-PLOT.
NAME is the name of the objetc to be created
GNU-PLOT-PATH must be the path to the GNU-PLOT executable."
  (unless (and (symbolp name) (stringp gnu-plot-path))
    (user-error "plotter-new: bad arguments"))
  (if (or (functionp name) (boundp name))
      (user-error "%s already exist or defined" name))
  (set name (start-process "plotter"
			   (generate-new-buffer-name " *plotter*")
			   gnu-plot-path))
  (set-process-coding-system (eval name) 'binary 'binary)
  (with-current-buffer (process-buffer (eval name))
    (set-buffer-multibyte nil))
  (or name (user-error "plotter-new: create process failed."))
  (fset name (lambda (method &rest args)
	       (pcase method
		 ((pred stringp) (process-send-string
				  (eval name)
				  (apply 'format
					 (concat method "\n") args)))
		 (:wait (with-current-buffer (process-buffer (eval name))
			  (erase-buffer)
			  (funcall name "print 'ready'")
			  (while (not (re-search-backward "ready" nil t))
			    (accept-process-output (eval name) nil))))
		 (:terminate (let ((buffer (process-buffer (eval name))))
			       (delete-process (eval name))
			       (kill-buffer buffer)
			       (fmakunbound name)
			       (makunbound name)))
		 (_ (user-error "Invalid plotter method")))))
  (put name 'function-documentation plotter-doc-str))

(provide 'plotter)
;;; plotter.el ends here
