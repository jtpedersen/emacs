;;; jtp-init --- COMMON CONFIGURATIONS
;;; Commentary:
;; my Emacs setup
;(setq debug-on-error t)
;;; Code:
(show-paren-mode t)			;; show matching parenthesis
(column-number-mode t)			;; show current column
(menu-bar-mode -1)			;; don't show menu-bar
(tool-bar-mode -1)			;; same for the toolbar
(scroll-bar-mode -1)			;; .. and for the scrollbar
(put 'scroll-left 'disabled nil)        ;scroll left
(setq inhibit-startup-message t)	;;dont show the GNU splash screen
(transient-mark-mode t)			;; show selection from mark
(mouse-avoidance-mode 'jump)            ;; jump mouse away when typing
(setq visible-bell 1)                   ;; turn off bip warnings
(auto-compression-mode t)               ;; browse tar archives
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.
(size-indication-mode t)             ;; Show current buffer size


;; set a default font
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height (pcase system-type
                                                                        ('gnu/linux 110)
                                                                        ('darwin 130)) :weight 'normal)

;; Garbage collect at every 20 MB allocated instead of the default 8 MB. This
;; speeds up various things.
(setq gc-cons-threshold 20000000)
(setq load-prefer-newer t)

;;a clock
(setq display-time-day-and-date t)
(defvar display-time-24hr-format t)
(display-time)

(add-to-list 'load-path "~/.emacs.d/lisp")

(if (string-equal system-type "windows-nt")
  (let (
        (mypaths
         '(
           "C:/Users/jtp/AppData/Local/Programs/Python/Python36/"
           "C:/Program Files/Git/bin"
           "D:/tools/bin"
           "D:/Tools/emacs/bin"
           )))
    (setenv "PATH" (mapconcat 'identity mypaths ";") )
    (setq exec-path (append mypaths (list "." exec-directory))))

  ;; Set exec path to be the same as the one from the shell
  (defun set-exec-path-from-shell-path()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (set-exec-path-from-shell-path)
    )

(setq temporary-file-directory "~/.emacs.d/tmp/")
;; Save all backups and auto-saves to a temporary directory. And clean it for all files older than a
;; week.
(defvar backup-dir "~/.emacs.d/backups")
(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

(message "Deleting backup files older than a week...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files backup-dir t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))

(defun dont-kill-emacs(bool)
  "Disable C-x C-c binding execute 'kill-emacs'."
  (interactive
   (list (y-or-n-p "Do you want to kill Emacs? ")))
  (if bool
      (save-buffers-kill-terminal)
    (message "phew")))
(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(defun sudo-find-file (file)
  "Find FILE with sudo/tramp."
  (interactive
   (list
    (read-file-name "Sudo find file: ")))
  (find-file (format "/sudo::%s" file)))

(defun sudo-find-current ()
  "Find current buffer file with sudo/tramp."
  (interactive)
  (sudo-find-file (buffer-file-name)))

(global-set-key (kbd "C-x w") 'sudo-find-current)


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
   '("~/orgs/RokuTvReady/Spring22.org" "~/orgs/todo.org" "~/orgs/inbox.org"))
 '(package-selected-packages
   '(ox-reveal ox-gfm helm-projectile dumb-jump ob-async git-timemachine smart-mode-line-powerline-theme esup helm-swoop zenburn-theme htmlize company-lsp company lsp-mode highlight-symbol yasnippet-classic-snippets all-the-icons-dired all-the-icons langtool plantuml-mode lua-mode helm-ag flx-ido flx helm-gtags use-package bury-successful-compilation el-get yasnippet ack helm-projetcile projectile cmake-mode keyfreq diff-hl highlight-current-line discover-my-major window-numbering clang-format helm multiple-cursors magit org  company-irony-c-headers company-irony python-mode req-package))
 '(safe-local-variable-values '((org-confirm-babel-evaluate))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#4ccc4ccc4ccc"))))
 '(company-scrollbar-fg ((t (:background "#3fff3fff3fff"))))
 '(company-tooltip ((t (:inherit default :background "#385138513851"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(highlight-current-line-face ((t (:background "gray22")))))



(defvar global-fill-column 100)
(setq-default fill-column global-fill-column)
(dolist (hook '(auto-fill-mode-hook
                prog-mode-hook))
  (add-hook hook (lambda () (setq fill-column global-fill-column))))

;; Sensible window splitting should follow the fill column.
(when window-system
  (setq split-height-threshold global-fill-column
        split-width-threshold (* 2 global-fill-column)))


;; Auto-revert buffers when files change on disk.
(defvar auto-revert-verbose t)            ; announce when buffer is reverted.
(global-auto-revert-mode t)

;;;;;;;;; MAC OS X SPECIFIC

(if (or (eq window-system 'ns) (eq window-system 'mac))
    (progn
      ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
                                        ;(setq mac-pass-command-to-system nil)

      ;; Let command be meta and alt be alt.
      ;; (setq mac-option-key-is-meta nil)
      ;; (setq mac-command-key-is-meta t)
      ;; (setq mac-command-modifier 'meta)
      ;; (setq mac-option-modifier nil)
      (x-focus-frame nil)
      (defvar ns-use-native-fullscreen nil)
      ))
(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key [(M-f10)] 'toggle-fullscreen)

;;;;;;;;; UTF-8 ENCODING

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;; CUSTOM COLORS & FONTS


(setq frame-title-format (list (format "%%S %%j ")
                               '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;;;;;;;;; CUSTOM KEYBINDINGS

;; Do not jump to headers
(global-set-key (kbd "C-c o")
                '(lambda () (interactive) (ff-find-other-file nil)))
(defvar cc-search-directories '("." "../Source/" "../../Source" "../Include" "../../Include"
                              "../../src/*" "../../../src/&" "../include/" "../../include/*"
                              "/usr/include/c++/*"))


(setq compilation-window-height 30)

(defun next-error-skip-warnings ()
  "Jumps to next error skipping warnings."
  (interactive)
  (let (threshold compilation-skip-threshold)
    (setq compilation-skip-threshold 2)
    (next-error)
    (setq compilation-skip-threshold threshold)))


(global-set-key [(C-f5)] 'compile)
(global-set-key [(f5)] 'recompile)
(global-set-key [(f6)] 'next-error)
(global-set-key [(C-f6)] 'flycheck-next-error)

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'recompile)
            (local-set-key "\C-c\C-f" 'next-error)))

;; Turn off adaptive process buffering when using compilation mode because it speeds up immensely
;; when there is a lot of output in the buffer.
(add-hook 'compilation-mode-hook
          (lambda () (setq process-adaptive-read-buffering nil)))

;; Turn it back on again when finished.
(add-hook 'compilation-finish-functions
          (lambda (buffer string)
            (setq process-adaptive-read-buffering t)))

;; Compilation output
(defvar compilation-scroll-output t)

;; windows endlines
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'prog-mode-hook 'remove-dos-eol)


;; Using hippie-expand instead of dabbrev-expand.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Put dabbrev expansions first because it's most often what's expected.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-expand-whole-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line))

;;;;;;;;; FUNCTIONS
;; convert current buffer to unix EOLs
(defun to-unix-eol ()
  "Change current buffer's line ending to unix convention."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)))


(defun clang-format-dwim ()
  "Perform clang-format on region or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (clang-format-region (region-beginning) (region-end))
      (clang-format-buffer))))

;; Bindings

(add-to-list 'auto-mode-alist '(".qss" . javascript-mode))

;;;;;;;;; EMAIL
(setq user-mail-address "jpedersen@roku.com")

;;;;;;;;; IDO

(ido-mode t)
(defvar ido-enable-flex-matching t)

;;;;;;;;; SPELLING

;; Set aspell as spell program
(defvar ispell-program-name "aspell")

;; Speed up aspell: ultra | fast | normal
(defvar ispell-extra-args '("--sug-mode=normal"))

;; Flyspell activation for text mode
(add-hook 'text-mode-hook
          (lambda () (flyspell-mode t)))

;; Change to danish dict
(defun da-spell ()
  "Set Ippell to use Danish dictionary."
  (interactive)
  (ispell-change-dictionary "dansk"))

;; Change to english dict
(defun en-spell ()
  "Set Ispell to use English dictionary."
  (interactive)
  (ispell-change-dictionary "english"))

;; Open .h/.cc files in c++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

;;;;;;;;; Text

;; set auto-fill-mode
(add-hook 'text-mode-hook
          (lambda () (auto-fill-mode t)))

;;;;;;;;; (La)TeX

;; set auto-fill-mode
(add-hook 'latex-mode-hook
          (lambda () (auto-fill-mode t)))
(setenv "TEXINPUTS" ".:~/latex/:")

;;;;;;;;; CSS-mode

(autoload 'css-mode "css-mode" "CSS mode." t)
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))


(require 'package)
                                        ;packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


(defun require-package (package)
  "Refresh package archives, check PACKAGE presence and install if it's not installed."
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

(require-package 'use-package)
(require 'use-package)

;;Closes *compilation* buffer after successful compilation, and otherwise when the failure was fixed
;; to compile, it restores the original window configuration.
;; (use-package bury-successful-compilation
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'bury-successful-compilation))

;; magic return to where you left from
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "saveplace.txt" )))
;; Saves mini buffer history including search and kill ring values, and compile history.
(use-package savehist
  :ensure t
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring compile-history))
  (setq savehist-autosave-interval 60)
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode t))


;;;;;;;;; Python-mode
(use-package python-mode
  :ensure t
  :config
  (autoload 'python-mode "python-mode" "Mode for editing Python source files")
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   )

;;;;;;;;; JavaScript
(add-hook 'js-mode-hook (lambda () (defvar js-indent-level 2)))

;;;;;;;;; C & C++

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (flyspell-prog-mode)
            (setq tab-width 4)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)
            (linum-mode)))


;; Create include guards
(defun my-c-header-ifdef ()
  "Create a header guard with random suffix on the define name."
  (interactive)
  (save-excursion
    (let* ((guard (replace-regexp-in-string "[^0-9a-zA-Z]" "_"
                                           (buffer-name)))
           (guard (replace-regexp-in-string "h\\'" "" guard))
           (guard (concat guard  (shell-command-to-string "openssl rand -hex 8"))))
      (goto-char (point-min))
      (insert (concat "#ifndef " guard))
      (insert (concat "#define " guard))
      (newline 2)
      (goto-char (point-max))
      (newline)
      (insert (concat "#endif // " guard ))
      (newline))))

;; cppcheck --template='{file}:{line}:{severity}:{message}' --quiet <filename>


;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))


;; ;;
;; ;;org mode
;;
(use-package org
  :ensure t
  :config
  (setq org-directory "~/orgs")
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
   ("\C-cc" . org-capture)))

(use-package htmlize
  :ensure t
  )

(use-package ob-async
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
  :bind (( "C-x g" . magit-status)))


;;;;;;; Mulitple cursors
(use-package multiple-cursors
  :ensure t
  :config
  :bind(( "C->"     . mc/mark-next-like-this)
        ( "C-<"     . mc/mark-previous-like-this)
        ( "C-c C-<" . mc/mark-all-like-this)
        ( "C-;" . mc/mark-all-dwim)))

;; recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              "/itsalltext/"  ; It's all text temp files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".emacs.d/saveplace.txt"
                              ".*-autoloads\\.el\\'"))
  (recentf-mode))


;;;;;;;;;;;;; HELM


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


;; ;; maya mel - mode
(add-to-list 'auto-mode-alist '("\\.mel$" . c++-mode))
;; arduino files
(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))
;; ala tail -f for log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
;; BitBake files
(add-to-list 'auto-mode-alist '("\\.bb$" . sh-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; load the clang-format module
(require 'clang-format)
(add-hook 'c++-mode-hook (lambda ()
                           (define-key c++-mode-map (kbd "C-M-<tab>") 'clang-format-dwim)))


(use-package  window-numbering
  :ensure t
  :config
  (window-numbering-mode 't))

(use-package discover-my-major
  :ensure t
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major)
  (global-set-key (kbd "C-h M-m") 'discover-my-mode))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "C++17"))))


;;;;; COMPILATION
(setq compilation-scroll-output t)
(setq compilation-window-height 30
      compilation-scroll-output 'first-error
      compilation-skip-threshold 2 ; skip accros warnings
      compilation-always-kill t) ;; Don't ask, just start new compilation.

;; terminal colors
(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


(use-package diff-hl
  :ensure t
  :requires magit
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode))


;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(defvar whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(defvar whitespace-line-column global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)


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

;; Tries to automatically detect the language of the buffer and setting the dictionary accordingly.
 ;; (req-package auto-dictionary
;;   :require ispell
;;   :config
;;   (add-hook 'text-mode-hook 'auto-dictionary-mode))

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t
  :requires flx
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; Disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-mode-line "ρ")
  (setq projectile-enable-caching nil)
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  ;; Want projects defined by a few markers and we always want to take the top-most marker.
  ;; Reorder so other cases are secondary
  ;; (setq projectile-project-root-files-functions #'(projectile-root-top-down
  ;;                                                     projectile-root-top-down-recurring
  ;;                                                     projectile-root-bottom-up
  ;;                                                     projectile-root-local))
  (projectile-global-mode)
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


(use-package yasnippet-classic-snippets
  :ensure t)

;; Turn on smerge-mode when opening a file with the markers in them.
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

;(add-hook 'find-file-hook 'sm-try-smerge)

(use-package helm-ag
  :ensure t
  :requires helm
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")
  (setq helm-ag-insert-at-point 'symbol))


(use-package plantuml-mode
  :ensure t
  :config
  (autoload 'plantuml-mode "plantuml-mode" "A mode for editing plantuml code." t)
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  ;(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

(use-package langtool
  :ensure t
  :config
  (add-hook 'org-mode-hook(lambda ()
                            (progn
                              (setq langtool-language-tool-jar "/home/jacob/3pp/LanguageTool-4.4/languagetool-commandline.jar")
                              (setq langtool-default-language "en-US")
                              (setq langtool-user-arguments '("--languagemodel" "/home/jacob/3pp/LanguageTool-4.4"))
                              (require 'langtool)))))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode 'highlight-symbol-mode)
  :bind
  (( "C-<f3>" . highlight-symbol)
   ( "<f3>"   . highlight-symbol-next)
   ( "S-<f3>" . highlight-symbol-prev)
   ( "M-<f3>" . highlight-symbol-query-replace)))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1)
  (global-set-key (kbd "C-<tab>") 'company-complete))

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (require 'lsp-clients)
;;   :hook ((c-mode c++-mode-hook) . lsp)
;;   :custom
;;   (lsp-prefer-flymake nil))

;; (use-package lsp-ui
;;   :ensure t)

;; (use-package company-lsp
;;   :ensure t
;;   )

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))


;; Should help speed loading of files
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Speed up loading of recent-f list
(setq recentf-keep '(file-remote-p file-readable-p))

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


(use-package smart-mode-line-powerline-theme
  :ensure t
  :config
  (setq sml/theme 'respectful))

(use-package git-timemachine
  :ensure t)


(use-package dumb-jump
  :ensure t
  :config
  (global-set-key (kbd "M-j") 'dumb-jump-go))



;;; init.el ends here
