;;COMMON CONFIGURATIONS
(setq debug-on-error t)
(show-paren-mode t)			;; show matching parenthesis
(column-number-mode t)			;; show current column
(menu-bar-mode -1)			;; don't show menu-bar
(tool-bar-mode -1)			;; same for the toolbar
(scroll-bar-mode -1)			;; .. and for the scrollbar
(put 'scroll-left 'disabled nil)        ;scroll left
(setq inhibit-startup-message t)	;;dont show the GNU splash screen
(transient-mark-mode t)			;; show selection from mark
(mouse-avoidance-mode 'jump)            ;; jump mouse away when typing
(setq visible-bell nil)                 ;; turn off bip warnings
(auto-compression-mode t)               ;; browse tar archives
(put 'upcase-region 'disabled nil)      ;; enable ``upcase-region''
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.
(size-indication-mode t)             ;; Show current buffer size


;; Garbage collect at every 20 MB allocated instead of the default 8 MB. This
;; speeds up various things.
(setq gc-cons-threshold 20000000)

;;a clock
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)


(add-to-list 'load-path "~/.emacs.d/lisp")

;;;packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(require 'req-package)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-push-arguments (quote ("--set-upstream")))
 '(org-agenda-files (quote ("~/orgs/luxdo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-yellow ((t (:background "yellow3" :foreground "black"))))
 '(highlight-current-line-face ((t (:background "gray22")))))


(setq global-fill-column 100)
(setq-default fill-column global-fill-column)
(dolist (hook '(auto-fill-mode-hook
               prog-mode-hook))
 (add-hook hook (lambda () (setq fill-column global-fill-column))))

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
;; under X11
(if (eq window-system 'x)
    (set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-15"))

(setq frame-title-format (list (format "%%S %%j ")
                               '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;;;;;;;; CUSTOM KEYBINDINGS

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "M-n") 'scroll-up-one-line)
;; (global-set-key (kbd "M-p") 'scroll-down-one-line)
;; (global-set-key (kbd "M-P") 'previous-user-buffer) 
;; (global-set-key (kbd "M-N") 'next-user-buffer) 
;;                                         ;(global-set-key (kbd "M-ƒ") 'open-with-finder) ; Command+Option+f

(global-set-key (kbd "C-c o") 'ff-find-other-file)


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
(global-set-key [(C-f6)] 'next-error-skip-warnings)
;; Compilation output
(setq compilation-scroll-output t)

;; windows endlines
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'prog-mode-hook 'remove-dos-eol)


;;;;;;;;; FUNCTIONS

;; convert current buffer to unix EOLs
(defun to-unix-eol ()
  "Change current buffer's line ending to unix convention."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)))

;; Set exec path to be the same as the one from the shell
(defun set-exec-path-from-shell-path()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-path)

;;;;;;;;; Luxion related

;; Wraps a function with // ***.. before and after (the region selected). Both
;; inserted lines with have a length fo 80 characters.
(defun lux-wrap-function (start end)
  "Put comments around Luxion function."
  (interactive "r")
  (let ((str (concat "// " (make-string (- 100 3) ?*) "\n")))
    (save-excursion
      (goto-char end)
      (insert str)
      (goto-char start)
      (insert str))))

(defun lux-fix-function-comments ()
  "Fix all functions with an incorrect number of '// ***..' (or '=' or '-') around them."
  (interactive)
  (let* ((regexp "[ ]*\/\/[ ]*[\*\=\-]+")
         (line-width global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (old-line)
         (line-end))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at regexp)
          ;; Replace with correct line.
          (beginning-of-line)
          (kill-line)
          (insert str)

          ;; Indent to fit with sorrounded scopes, if any.
          (indent-for-tab-command)

          ;; If the line exceeds `line-width` then kill the rest of line.
          (end-of-line)
          (setq line-end (current-column))
          (beginning-of-line)
          (setq old-line (line-number-at-pos))
          (forward-char line-width)
          (when (and (< (current-column) line-end)
                     (= old-line (line-number-at-pos)))
            (kill-line))
          (goto-line old-line))
        (forward-line))))) ;; Search next line.

(defun lux-fix-function-curls ()
  "Fix all functions with '// ***..' around it to have it's '{' be put after the second '//***..'."
  (interactive)
  (let* ((regexp-line "[ ]*\/\/[ ]*[\*]+")
         (regexp-curl "{")
         (line-width global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (line-one)
         (line-two)
         (flag nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not flag))
        ;; Find line one.
        (while (and (not (eobp))
                    (not flag))
          (when (looking-at regexp-line)
            (setq line-one (line-number-at-pos))
            (setq flag t))
          (forward-line))

        (unless (eobp)
          (setq flag nil)
          (forward-line)

          ;; Find line two.
          (while (and (not (eobp))
                      (not flag))
            (when (looking-at regexp-line)
              (setq line-two (line-number-at-pos))
              (setq flag t))
            (forward-line))

          (unless (eobp)
            (setq flag nil)
            (goto-line line-one)

            ;; Find and remove the '{'.
            (while (and (<= (line-number-at-pos) line-two)
                        (not flag))
              (when (looking-at regexp-curl)
                (delete-char 1)
                (cycle-spacing 0) ;; Remove any whitespace
                (setq flag t))
              (forward-char 1))

            (unless (eobp)
              (goto-line line-two)

              ;; If deleted '{' then insert on new line after `line-two`.
              (when flag
                (end-of-line)
                (insert "\n")
                (insert regexp-curl))

              (setq flag nil)
              (forward-line))))))))

(defun lux-fix-buffer ()
  (interactive)
  (lux-fix-function-curls)
;;  (cleanup-region-or-buffer)
  (lux-fix-function-comments)
  (clang-format-buffer))


;; Bindings
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") 'lux-wrap-function)
            (local-set-key (kbd "C-c l") 'lux-fix-buffer)))

;; Regressor tst
(add-to-list 'auto-mode-alist '(".tst" . conf-mode))

;;;;;;;;; EMAIL
(setq user-mail-address "jacob@luxion.com")

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
                                        ;  (add-hook hook (lambda () (flyspell-mode -1))))

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

;; Open .h files in c++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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

;;;;;;;;; Python-mode
(req-package python-mode
  :config
  (autoload 'python-mode "python-mode" "Mode for editing Python source files")
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))


;;;;;;;;; JavaScript

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))



;;;;;;;;; C & C++

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (flyspell-prog-mode)
            (local-set-key (kbd "C-;") 'iedit-mode) ;; default is overwritten by flyspell-correct-word-before-point
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
      ;;XXX todo insert ifdef CPP extern c stuff
      (insert "// (c) Copyright 2003-2014 Luxion ApS - All Rights Reserved")
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
    'irony-completion-at-point-async))

(req-package   company-irony
  :require (company company-irony-c-headers)
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  ;; (optional) adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'objc-mode-hook 'company-mode))


;;;; irony install server 
;; -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON   -DCMAKE_PREFIX_PATH=/opt/local/libexec/llvm-3.8 CXX=clang-mp-3.8 cmake -DCMAKE_INSTALL_PREFIX\=/Users/jtp/.emacs.d/irony/ /Users/jtp/.emacs.d/elpa/irony-20160317.1527/server && cmake --build . --use-stderr --config Release --target install

;; ;;
;; ;;org mode
;;
(req-package org-mode
  :config
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  ;;
  ;; Yes it's long... but more is better ;)
  (setq org-clock-history-length 35)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change task state to STARTED when clocking in
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
  (setq magit-cherry-pick-arguments '("-x"))
  (setq magit-auto-revert-mode nil))

;;;;;;; Mulitple cursors
(req-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

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

(req-package smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'dark)
  (add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/keyshot_network" ":KS-NET:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/" ":KS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/luxion" ":Luxion:") t)
  (add-to-list 'sml/replacer-regexp-list '(".*repo/src/keyshot_network" ":ksnr:") t)
  )

(req-package clang-format
  :config
  (global-set-key [C-M-tab] 'clang-format-region))


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



; Save all backups and auto-saves to a temporary directory. And clean it for all files older than a
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
;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)



(req-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

;;;;; COMPILATION
(setq compilation-scroll-output t)
(setq compilation-window-height 30
      compilation-scroll-output 'first-error
      compilation-skip-threshold 2 ; skip accros warnings
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
(req-package auto-dictionary
  :require ispell
  :config
  (add-hook 'text-mode-hook 'auto-dictionary-mode))

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
  :require projectile
  :config
  (setq helm-projectile-fuzzy-match t)
  (setq projectile-switch-project-action 'helm-projectile-find-file)

  ;; Use helm-projectile alternatives.
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "f")) 'helm-projectile-find-file)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "d")) 'helm-projectile-find-dir)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "o")) 'helm-projectile-find-other-file)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "a")) 'helm-projectile-ag)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "p")) 'helm-projectile-switch-project)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "b")) 'helm-projectile-switch-to-buffer))


(defconst yas-dir (concat user-emacs-directory "snippets"))

(req-package yasnippet
  :config
  ;; Add local snippets to override some of the defaults in elpa folder.
  (add-to-list 'yas-snippet-dirs yas-dir)
  (yas-global-mode 1))

(req-package-finish)
