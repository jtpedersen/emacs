(setq debug-on-error t)

;; Ensure Org-mode and Babel are loaded
(require 'org)

;; Setup Org-Babel for code execution
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)    
   (shell . t)         
   (plantuml . t)
   (python . t)
   ))

;; Disable confirmation prompts for code block execution
(setq org-confirm-babel-evaluate nil)

;; Enable syntax highlighting for code blocks
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

;; Load the main configuration 
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))


(my-use-package wrap-region
		:config
		(wrap-region-global-mode t)
		(wrap-region-add-wrapper "#+BEGIN_SRC emacs-lisp\n" "#+END_SRC" "#" 'org-mode))


