;;; report.el --- Report automation for latex with org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author: Sicpier
;; Keywords: convenience

;;; Commentary:

;;


;;--------:<--------:<--------:<--------:<--------:<------->:<-----|--:<--------
;;; Code:

(defconst report-doc-str
  "Report command interface.
Usage:

Simple commands: (NAME \"command string\" ARGS)

The command string can contain format specifications according to
the format function. For placing a '%' you have to use a double
sign '%%'. For markup explanations refer to:
http://orgmode.org/manual/Markup.html

Other commands:

\(NAME :clear) deletes all file contents.
\(NAME :title) defines the title of the report.
\(NAME :picture :file<file> [:caption <caption>] [:label <label>]
[:latex-ops <options>] [:html-ops <options>]) inserts a picture
from file with optional caption, label (for LaTeX references) and
options.  For the possible latex-ops and html-ops see the ORG
MODE documentation for '#+ATTR_LATEX:' and '#+ATTR_HTML:'
respectively.
\(NAME :save) saves ORG file contents.
\(NAME :export-pdf) converts the ORG file to PDF.
\(NAME :export-html) converts the ORG file to HTML.
\(NAME :terminate) stops the process and deletes the object."
  "Documentation string for the created objects")

(defun report-new (name path)
  "Creates a report buffer based on org-mode.
NAME is the name of the object to be created
PATH must be the path to the associated file for the buffer."
  (unless (and (symbolp name) (stringp path))
    (user-error "report-new: bad arguments"))
  (if (or (functionp name) (boundp name))
      (user-error "%s already exist or defined" name))
  (set name (find-file-noselect path))
  (or (eval name) (user-error "report-new: open file failed."))
  (fset name (lambda (method &rest args)
	       (with-current-buffer (eval name)
		 (pcase method
		   ((pred stringp)
		    (insert (apply 'format (concat method "\n") args)))
		   (:init
		    (insert "#+LaTeX_CLASS: koma-article\n")
		    (insert "#+LATEX_HEADER: \\usepackage{xcolor}\n")
		    (insert "#+LATEX_HEADER: \\usepackage[maxfloats=1000]{morefloats}\n")
		    (insert "#+LATEX_HEADER: \\maxdeadcycles=1000\\relax\n"))
		   (:clear (erase-buffer)
			   (funcall name :init))
		   (:title
		    (insert (apply 'format (concat "#+TITLE: "
						   (car args)
						   "\n")
				   args)))
		   (:picture (let ((file (plist-get args :file))
				   (caption (plist-get args :caption))
				   (label (plist-get args :label))
				   (latex-ops  (plist-get args :latex-ops))
				   (html-ops (plist-get args :html-ops)))
			       (insert "#+attr_latex: :placement [!h]\n")
			       (if caption
				   (insert (format "#+CAPTION: %s\n" caption)))
			       (if label
				   (insert (format "#+NAME: %s\n" label)))
			       (if latex-ops
				   (insert (format "#+ATTR_LATEX: %s\n"
						   latex-ops)))
			       (if html-ops
				   (insert (format "#+ATTR_HTML: %s\n"
						   html-ops)))
			       (insert (format "[[./%s]]\n\n"
					       (file-relative-name
						file (file-name-directory
						      path))))))
		   (:error
		    (insert (apply
			     'format
			     (concat
			      "#+LATEX: \\colorbox{red}{\\color{white}"
			      (car args) "}\n")
			     args))
		    (insert (apply
			     'format
			     (concat
			      "#+HTML: <p style=\"color: #ffffff; background-color: #ff0000\">"
			      (car args) "</p>\n")
			     args)))
		   (:save (save-buffer))
		   (:export-pdf (org-latex-export-to-pdf))
		   (:export-html (org-html-export-to-html))
		   (:terminate (set-buffer-modified-p nil)
			       (kill-buffer (eval name))
			       (fmakunbound name)
			       (makunbound name))
		   (_ (user-error "Invalid report method"))))))
  (funcall name :init)
  (put name 'function-documentation report-doc-str))

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'report)
;;; report.el ends here
