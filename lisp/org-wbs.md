;;; org-wbs.el &#x2014; Convert Org-mode outline to PlantUML WBS and preview -**- lexical-binding: t; -**-

;; Author: ChatGPT ;; Version: 1.2 ;; License: MIT ;; Dependencies: org-mode, plantuml-mode

;;; Commentary: ;; This package converts an Org-mode outline into a PlantUML Work Breakdown Structure (WBS) ;; and renders the image preview inside Emacs using \`plantuml-mode\`.

;;; Code:

(require 'org) (require 'plantuml-mode)

(defvar org-wbs-plantuml-file "/tmp/org-wbs.puml" "Temporary file to store the generated PlantUML WBS.")

(defun org-wbs-generate (output-file) "Convert the current Org buffer's outline into a PlantUML WBS and save it to OUTPUT-FILE." (interactive "FOutput PlantUML file: ") (let ((wbs-structure '()) (root-title (file-name-base (or (buffer-file-name) "OrgDocument")))) ;; Extract headings (org-map-entries (lambda () (let ((level (org-outline-level)) (title (org-get-heading t t t t))) (push (cons level title) wbs-structure)))) ;; Sort and structure the outline (setq wbs-structure (reverse wbs-structure)) ;; Write to PlantUML file (with-temp-file output-file (insert "@startwbs\n") (insert (format "\* %s\n" root-title)) (dolist (entry wbs-structure) (let ((level (car entry)) (title (cdr entry))) (insert (format "%s\*\* %s\n" (make-string (1- level) ?\*) title)))) (insert "@endwbs\n")) (message "PlantUML WBS file generated: %s" output-file)))

(defun org-wbs-preview () "Generate and preview the Org-mode outline as a WBS diagram using \`plantuml-mode\`." (interactive) (org-wbs-generate org-wbs-plantuml-file) (with-current-buffer (get-buffer-create "**Org WBS Preview**") (erase-buffer) (insert-file-contents org-wbs-plantuml-file) (plantuml-mode) (plantuml-preview)) (message "Previewing Org WBS in **Org WBS Preview** buffer."))

(provide 'org-wbs)

;;; org-wbs.el ends here
