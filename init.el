(setq debug-on-error `nil)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
