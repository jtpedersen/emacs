;;COMMON CONFIGURATIONS
;(setq debug-on-error t)
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
(put 'upcase-region 'disabled nil)      ;; enable ``upcase-region''
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.
(size-indication-mode t)             ;; Show current buffer size


(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))
;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Garbage collect at every 20 MB allocated instead of the default 8 MB. This
;; speeds up various things.
(setq gc-cons-threshold 20000000)
(setq load-prefer-newer t)

;;a clock
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
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

(defun show-elapsed-time (msg start end)
  (let ((elapsed (float-time (time-subtract end start))))
    (message "%s %.3fs" msg elapsed)))

;; Save all backups and auto-saves to a temporary directory. And clean it for all files older than a
;; week.
(setq backup-dir "~/.emacs.d/backups")
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
  "Disable C-x C-c binding execute kill-emacs."
  (interactive
   (list (y-or-n-p "Do you want to kill emacs? ")))
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
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)


(defun sudo-find-file (file)
  "Find file with sudo/tramp."
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
   (quote
    ("3693403316f0127326fa08067c2e3013eda29216829e1478e1656ea4fbbc6560" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(eclim-eclipse-dirs (quote ("d:/Tools/eclipse-installation")))
 '(eclim-executable "d:/Tools/eclipse-installation/eclim.bat")
 '(magit-branch-arguments nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-push-arguments (quote ("--set-upstream")))
 '(org-agenda-files (quote ("d:/BGProjects/orgs/PN.org")))
 '(package-selected-packages
   (quote
    (lua-mode ac-geiser dumb-jump geiser eclim yasnippet ack helm-projectile projectile helm-flx flx-ido cmake-mode keyfreq diff-hl highlight-current-line highlight-thing vlf discover-my-major window-numbering clang-format smart-mode-line fic-mode helm-gtags helm multiple-cursors magit org flycheck-irony company-irony-c-headers company-irony python-mode req-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-current-line-face ((t (:background "gray22")))))


(setq global-fill-column 100)
(setq-default fill-column global-fill-column)
(dolist (hook '(auto-fill-mode-hook
                prog-mode-hook))
  (add-hook hook (lambda () (setq fill-column global-fill-column))))

;; Sensible window splitting should follow the fill column.
(when window-system
  (setq split-height-threshold global-fill-column
        split-width-threshold (* 2 global-fill-column)))


;; Auto-revert buffers when files change on disk.
(setq auto-revert-verbose t)            ; announce when buffer is reverted.
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
      (setq ns-use-native-fullscreen nil)
      ))
(defun toggle-fullscreen ()
  "Toggle full screen"
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

(load-theme 'tsdh-dark)

(setq frame-title-format (list (format "%%S %%j ")
                               '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defface hi-yellow
  '((((min-colors 88) (background dark))
     (:background "DarkGoldenrod3" :foreground "black"))
    (((background dark)) (:background "DarkGoldenrod3" :foreground "black"))
    (((min-colors 88)) (:background "DarkGoldenrod3"))
    (t (:background "DarkGoldenrod3")))
  "Default face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)



;;;;;;;;; CUSTOM KEYBINDINGS

;; Do not jump to headers
(global-set-key (kbd "C-c o")
                '(lambda () (interactive) (ff-find-other-file nil t)))


(setq compilation-window-height 30)

(defun next-error-skip-warnings ()
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
(setq compilation-scroll-output t)

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
(setq user-mail-address "jacob.toft.pedersen@beumergroup.com")

;;;;;;;;; IDO

(ido-mode t)
(setq ido-enable-flex-matching t)

;;;;;;;;; SPELLING

;; Set aspell as spell program
(setq ispell-program-name "aspell")

;; Speed up aspell: ultra | fast | normal
(setq ispell-extra-args '("--sug-mode=normal"))

;; Flyspell activation for text mode
(add-hook 'text-mode-hook
          (lambda () (flyspell-mode t)))

;; Remove Flyspell from some sub modes of text mode
                                        ;(dolist (hook '(change-log-mode-hook 
                                        ;                log-edit-mode-hook))

;; Change to danish dict
(defun da-spell ()
  "Set ispell to use Danish dictionary"
  (interactive)
  (ispell-change-dictionary "dansk"))

;; Change to english dict
(defun en-spell ()
  "Set ispell to use English dictionary"
  (interactive)
  (ispell-change-dictionary "english"))

;; indent on CR
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

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

;;;;;;;;; Diff-mode

;; (autoload 'diff-mode "diff-mode" "Diff major mode" t)
;; (add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;;;;;;;;; Apache-mode

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))



;;;packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
;       ("melpa-stable" . "http://stable.melpa.org/packages/")
       ("org" . "http://orgmode.org/elpa/")
       ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(require 'req-package)

;; Closes *compilation* buffer after successful compilation, and otherwise when the failure was
;; fixed to compile, it restores the original window configuration.
;; (req-package bury-successful-compilation
;;   :config
;;   (add-hook 'prog-mode-hook 'bury-successful-compilation))

;; magic return to where you left from
(req-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "saveplace.txt" )))
;; Saves mini buffer history including search and kill ring values, and compile history.
(req-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring compile-history))
  (setq savehist-autosave-interval 60)
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode t))


;;;;;;;;; Python-mode
(req-package python-mode
  :config
  (autoload 'python-mode "python-mode" "Mode for editing Python source files")
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))


;;;;;;;;; JavaScript

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

;;; java


(req-package eclim
  :require (ac-emacs-eclim)
  :config
;  (setq eclimd-autostart t)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  ;; regular auto-complete initialization
  (require 'auto-complete-config)
  (ac-config-default)

  ;; add the emacs-eclim source
  ;; (require 'ac-emacs-eclim-source)
  ;; (ac-emacs-eclim-config)
  (ac-emacs-eclim-config)
  (add-hook 'java-mode-hook (lambda ()
                              (eclim-mode t)))
  (add-hook 'eclim-mode-hook (lambda ()
                             (local-set-key (kbd "C-,") 'eclim-problems-correct)
                             (local-set-key (kbd "C-.") 'eclim-problems-next-same-file))))





;;;;;;;;; C & C++

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (flyspell-prog-mode)
            (setq tab-width 2)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)))


;; crate include guards
(defun my-c-header-ifdef ()
  (interactive)
  (save-excursion
    (let* ((file (replace-regexp-in-string "[^0-9a-zA-z]" "_" 
                                           (buffer-name)))
           (file (upcase file))
           (file (concat file "_")))
      (beginning-of-buffer)
      (newline)
      (insert (concat "#ifndef " file)) 
      (newline)
      (insert (concat "#define " file))
      (newline 2)
      (end-of-buffer)
      (newline)
      (insert (concat "#endif /* !" file " */"))
      (newline))))

;; cppcheck --template='{file}:{line}:{severity}:{message}' --quiet <filename>


;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  ;; Only run auto-setup first time to be faster. However, it's not perfect because if opening a
  ;; file in a different project root it will not auto-setup for the new CDB. But then use
  ;; `irony-cdb-json-select'.
  (when (not my-irony-cdb-loaded-time)
    (setq my-irony-cdb-loaded-time (current-time))
    (message "Irony CDB auto-setup...")
    (irony-cdb-autosetup-compile-options)
    (show-elapsed-time "Irony CDB auto-setup done in" my-irony-cdb-loaded-time (current-time))))


(req-package   company-irony
  :require (company company-irony-c-headers)
  :config
                                        ;(add-hook 'c++-mode-hook 'irony-mode)
                                        ;(add-hook 'c-mode-hook 'irony-mode)
                                        ;(add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  ;; (optional) adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  ;;  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (defun irony-auto-kill ()
    (interactive)
    (run-at-time "50 sec" nil
                 (lambda ()
                   (irony-server-kill)
                   (irony-auto-kill))))
  ;;  (add-hook 'irony-mode-hook 'irony-auto-kill)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'objc-mode-hook 'company-mode))

(req-package flycheck-irony
  :require irony flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; (req-package irony-eldoc
;;  :require irony
;;  :config
;;  (add-hook 'irony-mode-hook 'irony-eldoc))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))


;; ;;
;; ;;org mode
;;
(req-package org
  :config
  :require org-babel
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  ;;
  ;; Yes it's long... but more is better ;)
  (setq org-clock-history-length 35)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;;Change task state to STARTED when clocking in
  ;(setq org-clock-in-switch-to-state "STARTED")
  ;; writing hooks
  (add-hook 'org-mode-hook 'auto-fill-mode t)
  (add-hook 'org-mode-hook 'flyspell-mode t)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     (emacs-lisp . t)
     )))


;;magit
(req-package magit
  :config
  (global-set-key "\C-xg" 'magit-status)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  ;; Set defaults used by specific operations.
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-pull-arguments '("--rebase"))
  (setq magit-cherry-pick-arguments '("-x")))

;;;;;;; Mulitple cursors
(req-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-;") 'mc/mark-all-dwim)
  )

;;;;;;;;;;;;; HELM

(req-package helm
  :require recentf
  :config
  (setq helm-recentf-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key "\C-xr" 'helm-recentf))

;; recentf
(req-package recentf
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

(req-package helm-gtags
  :config
  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

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

;; fixme in comments
(req-package fic-mode
  :config
  (add-to-list 'fic-highlighted-words '"XXX")
  (add-hook 'c++-mode-hook 'fic-mode))

;; ala tail -f for log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))


(req-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; (req-package smart-mode-line
;;   :config
;;   (sml/setup)
;;   (sml/apply-theme 'dark)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/keyshot_network" ":KS-NET:") t)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/" ":KS:") t)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/luxion" ":Luxion:") t)
;;   (add-to-list 'sml/replacer-regexp-list '(".*/src/keyshot_network" ":ksnr:") t)
;;   (add-to-list 'sml/replacer-regexp-list '(".*/test/keyshot_network" ":ksnr-test:") t)
;;   )

(req-package clang-format
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (define-key c++-mode-map (kbd "C-M-<tab>") 'clang-format-dwim))))


(req-package  window-numbering
  :config
  (window-numbering-mode 't))

(req-package discover-my-major
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major)
  (global-set-key (kbd "C-h M-m") 'discover-my-mode))

(req-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"))))

;; (require 'devdocs-lookup)
;; (devdocs-setup)
;; (global-set-key "\C-cd" 'devdocs-lookup)

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; (flyspell-prog-mode)
;;             (local-set-key "\C-cd" #'devdocs-lookup-cpp)))

;; (add-hook 'python-mode-common-hook
;;           (lambda ()
;;             ;; (flyspell-prog-mode)
;;             (local-set-key "\C-cd" #'devdocs-lookup-python)))



(req-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

;;;;; COMPILATION
(setq compilation-scroll-output t)
(setq compilation-window-height 30
      compilation-scroll-output 'first-error
                                        ;      compilation-skip-threshold 2 ; skip accros warnings
      compilation-always-kill t) ;; Don't ask, just start new compilation.

;; terminal colors
(req-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


(req-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds 0.8)
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

(req-package highlight-current-line
  :config
  (setq highlight-current-line-globally t))

(req-package diff-hl
  :require magit
  :config
                                        ;  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'prog-mode-hook 'diff-hl-mode))


;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(setq whitespace-line-column global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)


(req-package keyfreq
  :config (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;; CMake
(req-package cmake-mode
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

(req-package flx-ido
  :require flx
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; Disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(req-package helm-flx
  :require (helm flx)
  :config
  ;; Use flx for better search results.
  (helm-flx-mode +1))

(req-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-mode-line "ρ")
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(req-package helm-projectile
  :require (projectile ack)
  :config
  (setq helm-projectile-fuzzy-match t)
  (setq projectile-switch-project-action 'helm-projectile-find-file)

  ;; Use helm-projectile alternatives.
  (defun msk/projectile-define-prefix-key (key func)
    (define-key projectile-mode-map
      (kbd (concat projectile-keymap-prefix key)) func))
  (msk/projectile-define-prefix-key "f" 'helm-projectile-find-file)
  (msk/projectile-define-prefix-key "d" 'helm-projectile-find-dir)
  (msk/projectile-define-prefix-key "o" 'helm-projectile-find-other-file)
  (msk/projectile-define-prefix-key "a" 'helm-projectile-ag)
  (msk/projectile-define-prefix-key "p" 'helm-projectile-switch-project)
  (msk/projectile-define-prefix-key "b" 'helm-projectile-switch-to-buffer)
  (msk/projectile-define-prefix-key "r" 'helm-projectile-recentf)

  (defun msk/helm-update-gtags (arg)
    "Update gtags for all files or create if they don't already
exist. When given the prefix argument present gtags will be
removed and then recreated."
    (interactive "P")
    (let ((gtags-file (concat (projectile-project-root) "GTAGS"))
          (grtags-file (concat (projectile-project-root) "GRTAGS"))
          (gpath-file (concat (projectile-project-root) "GPATH")))
      (progn
        (when arg
          (message "Removing gtags..")
          (delete-file gtags-file)
          (delete-file grtags-file)
          (delete-file gpath-file))
        (if (file-exists-p gtags-file)
            (progn
              (message "Updating gtags..")
              (universal-argument)
              (helm-gtags-update-tags))
          (progn
            (message "Creating gtags..")
            (helm-gtags-create-tags (projectile-project-root) "default"))))))
  (msk/projectile-define-prefix-key "R" 'msk/helm-update-gtags))


(defconst yas-dir (concat user-emacs-directory "snippets"))
(req-package yasnippet
  :config
  ;; Add local snippets to override some of the defaults in elpa folder.
  (add-to-list 'yas-snippet-dirs yas-dir)
  (add-to-list 'yas-snippet-dirs "~/srcs/yasnippet-snippets")
  (yas-global-mode 1))


;; Turn on smerge-mode when opening a file with the markers in them.
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge)

(req-package helm-ag
  :require helm
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats"))


(req-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(req-package copy-as-format
  :config (global-set-key (kbd "C-c w s") 'copy-as-format-slack))

(req-package ace-isearch
  :require helm-swoop avy ace-jump-mode
  :config
  (setq ace-isearch-input-idle-jump-delay 0.5
        ace-isearch-function 'avy-goto-word-1
        ace-isearch-input-length 6 ; Invoke helm-swoop when >= 6.
        ace-isearch-function-from-isearch 'ace-isearch-helm-swoop-from-isearch
        ace-isearch-use-jump 'printing-char)
  (global-ace-isearch-mode +1)

  ;; (define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
  ;; (define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)
  )

(req-package highlight-escape-sequences
  :config
  ;; Has its own `hes-mode-alist' that specifies which modes it supports.
  (hes-mode))

(req-package scad-mode
  :require 'scad-preview
  :config
  (autoload 'scad-mode "scad-mode" "A major mode for editing OpenSCAD code." t)
  (add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode)))

(req-package geiser  )
(req-package ac-geiser  )

;; Jump to definition for multiple languages without configuration.
(req-package dumb-jump
  :require helm
  :bind (("M-g j" . dumb-jump-go)
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g e" . dumb-jump-go-prefer-external)
         ("M-g x" . dumb-jump-go-prefer-external-other-window)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g b" . dumb-jump-back))
  :config
  (setq dumb-jump-selector 'helm
        dumb-jump-max-find-time 5))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (progn
;;               (setq dumb-jump-mode t)
;;               (define-key dumb-jump-mode-map (kbd "M-g j") 'dumb-jump-go))))

(req-package lua-mode
  :config
  (autoload 'lua-mode "lua-mode" "A mode for editing lua code." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))

(req-package-finish)
