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





;; ;;a clock
;; (setq display-time-day-and-date t)
;; (defvar display-time-24hr-format t)
;; (display-time)








(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3693403316f0127326fa08067c2e3013eda29216829e1478e1656ea4fbbc6560" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-push-arguments '("--set-upstream"))
 '(org-agenda-files
   '((concat org-directory "/timer.org")
     (concat org-directory "/todo.org")))
 '(package-selected-packages
   '(eldoc-box rust-mode chatgpt-shell yaml-mode flatbuffers-mode yasnippet-snippets dap-cpptools which-key helm-xref ox-reveal ox-gfm helm-projectile dumb-jump ob-async git-timemachine smart-mode-line-powerline-theme esup helm-swoop zenburn-theme htmlize company-lsp company lsp-mode highlight-symbol yasnippet-classic-snippets all-the-icons-dired all-the-icons langtool plantuml-mode lua-mode helm-ag flx-ido flx helm-gtags use-package bury-successful-compilation el-get yasnippet ack helm-projetcile projectile cmake-mode keyfreq diff-hl highlight-current-line discover-my-major window-numbering clang-format helm multiple-cursors magit org company-irony-c-headers company-irony python-mode req-package))
 '(safe-local-variable-values
   '((epa-file-cache-passphrase-for-symmetric-encryption . t)
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (org-confirm-babel-evaluate))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#4ccc4ccc4ccc"))) t)
 '(company-scrollbar-fg ((t (:background "#3fff3fff3fff"))) t)
 '(company-tooltip ((t (:inherit default :background "#385138513851"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(highlight-current-line-face ((t (:background "gray22")))))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)




;;;;;;;;; UTF-8 ENCODING



;;;;;;;;; CUSTOM KEYBINDINGS

;;;;;;;;; FUNCTIONS


;;;;;;;;; IDO
(ido-mode t)
(defvar ido-enable-flex-matching t)

;;;;;;;;; SPELLING



;;;;;;;;; CSS-mode

(autoload 'css-mode "css-mode" "CSS mode." t)
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))





;;; Programming


;;;;;;;;; C & C++


;; 


;; Elisp

;; ;;
;; ;;org mode
(use-package ox-reveal
  :ensure t)


(defun export-tangle ()
  "Shortcut for exporting and tangling the current org-mode buffer."
  (interactive)
  (org-gfm-export-to-markdown)
  (org-babel-tangle))

;;
(use-package org
  :ensure t
  :config
  (setq org-log-done t)
  ;; Yes it's long... but more is better ;)
  (defvar org-clock-history-length 35)
  ;; Resume clocking task on clock-in if the clock is open
  (defvar org-clock-in-resume t)
  ;;Change task state to STARTED when clocking in
  (defvar org-clock-in-switch-to-state "STARTED")
  ;; Doing
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "|" "ABORTED(a@)" "DONE(d@/!)")))
  ;; writing hooks
  (add-hook 'org-mode-hook 'auto-fill-mode t)
  (add-hook 'org-mode-hook 'flyspell-mode t)
  ;; Capture notes
  (defvar jtp-inbox (concat org-directory "/todo.org"))
  (defvar org-default-notes-file jtp-inbox)
  ;; Templates
  (defvar org-capture-templates
    '(("t" "Todo" entry (file+headline jtp-inbox "Inbox")
       "* TODO %?\n  %i\n  %a")
      ("f" "Follow up" entry (file+headline jtp-inbox "Tasks")
       "* TODO Follow up on: %?\n  DEADLINE: %^t")
      ("m" "Meeting" entry (file+headline jtp-inbox "Meetings")
       "* TODO %?\n  SCHEDULED: %^T")))
  ;; Export to confluence
  (require 'ox-confluence)
  (require 'ox-reveal)
  ;; Export to jira
  (load-file (concat user-emacs-directory "lisp/ox-jira.el"))
  ;; load plantuml
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages '(
                                 (plantuml . t)
                                 (shell . t)
                                 (python . t)
                                 ))
    )
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "~/plantuml.jar")

  :bind
  (("\C-ca" . org-agenda)
   ("\C-ce" . export-tangle)
   ("\C-cc" . org-capture)))

(use-package htmlize
  :ensure t
  )

;;magit
(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  ;; Set defaults used by specific operations.
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-pull-arguments '("--rebase"))
  (setq magit-cherry-pick-arguments '("-x"))
  :bind (
	 ( "C-x g" . magit-status)
	 ( "C-c h" . magit-log-buffer-file)
	 ))


;;;;;;; Mulitple cursors
(use-package multiple-cursors
  :ensure t
  :config
  :bind(( "C->"     . mc/mark-next-like-this)
        ( "C-<"     . mc/mark-previous-like-this)
        ( "C-c C-<" . mc/mark-all-like-this)
        ( "C-;" . mc/mark-all-dwim)))

;; recentf


;; arduino files
(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))
;; ala tail -f for log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; (use-package uniquify
;;   :config
;;   (setq uniquify-buffer-name-style 'forward))





(use-package keyfreq
  :ensure t
  :config (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;; CMake
(use-package cmake-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))


(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-mode-line "ρ")
  (setq projectile-enable-caching 'native)
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (projectile-mode +1)
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )


(use-package yasnippet
  :ensure t
  :config
  (defconst yas-dir (concat user-emacs-directory "snippets"))
  ;; Add local snippets to override some of the defaults in elpa folder.
  (add-to-list 'yas-snippet-dirs yas-dir)
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :ensure t)

;; Turn on smerge-mode when opening a file with the markers in them.
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))


(add-hook 'find-file-hook 'sm-try-smerge t)
(add-hook 'smerge-mode-hook
      (lambda ()
        (local-set-key (kbd "M-RET") #'smerge-keep-current)
        (local-set-key (kbd "M-a") #'smerge-keep-all)
        (local-set-key (kbd "M-n") #'smerge-next)
        (local-set-key (kbd "M-p") #'smerge-prev)))

(use-package plantuml-mode
  :ensure t
  :config
  (autoload 'plantuml-mode "plantuml-mode" "A mode for editing plantuml code." t)
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-output-type "png")
  (setq plantuml-default-exec-mode 'jar)
  (add-hook 'plantuml-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'plantuml-preview))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1)
  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))


;; Should help speed loading of files
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Speed up loading of recent-f list
(setq recentf-keep '(file-remote-p file-readable-p))


;;;;;;;;;;;;; HELM

(use-package helm-xref
  :ensure t)

(use-package helm
  :ensure t
  :config
  (defvar helm-recentf-fuzzy-match t)
  (defvar helm-ff-file-name-history-use-recentf t)
  :bind (
         ("M-x"   . helm-M-x)
         ("M-y"   . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x r" . helm-recentf)))


(use-package helm-gtags
  :ensure t
  :config
  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (setenv "GTAGSFORCECPP" "1")
  ;; Set key bindings
  (eval-after-load "helm-gtags"
    '(progn
       (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
       (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
       (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
       (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
       (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
       (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
       (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))))

(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")
  (setq helm-ag-insert-at-point 'symbol)
  :bind (("C-c j"   . helm-do-ag)))


(use-package helm-swoop
  :ensure t
  :config (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (setq helm-multi-swoop-edit-save t)
  ;;If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direction. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)

  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t))


;; default in emacs-30
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))


	       ;; ((rust-ts-mode rust-mode) .
	       ;; 	("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))


(setq rust-format-on-save t)


;; (use-package smart-mode-line-powerline-theme
;;   :ensure t
;;   :config
;;   (setq sml/theme 'respectful))


;; (use-package chatgpt-shell
;;   :ensure t
;;   :config
;;   (setq chatgpt-shell-openai-key gpt-key))

;This library implements a Markdown back-end (github flavor) for Org exporter, based on the `md'
;back-end.
(use-package ox-gfm
  :ensure t)

;;; init.el ends here
