;; ------------------------
;;   Emacs Configurations
;; ------------------------

;; inspired by Slot Kristensen
;;
;; I got inspiration to some of the tweaks elsewhere. One of those are
;; Ian Zerny. If I have wrongfully forgotten to give credit to the
;; original source of something please do inform me of the mistake.
;;
;; This file is free software. You may redistribute it and/or modify
;; it under the terms of the GNU General Public License, version 2 or
;; later as published by the Free Software Foundation.
;;
;; The file is distributed AS IS and WITHOUT ANY WARRANTY. I hope you
;; will find it useful and I welcome feedback and
;; modifications/improvements.

;;;;;;;;; COMMON CONFIGURATIONS

(show-paren-mode t)                     ;; show matching parenthesis
(column-number-mode t)                  ;; show current column
(menu-bar-mode -1)                      ;; don't show menu-bar
(tool-bar-mode -1)                      ;; same for the toolbar
(scroll-bar-mode -1)                    ;; .. and for the scrollbar
(put 'scroll-left 'disabled nil)        ; scroll left
(setq inhibit-startup-message t)        ;; dont show the GNU splash screen
(transient-mark-mode t)                 ;; show selection from mark
(mouse-avoidance-mode 'jump)            ;; jump mouse away when typing
(setq visible-bell nil)                 ;; turn off bip warnings
(auto-compression-mode t)               ;; browse tar archives
(put 'upcase-region 'disabled nil)      ;; enable ``upcase-region''
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.

;;a clock
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;; recentf
(recentf-mode 1)
(add-to-list 'recentf-exclude "ido.last")
(add-to-list 'recentf-exclude ".*COMMIT_EDITMSG.*")
(add-to-list 'recentf-exclude ".*ede-projects.el")
(add-to-list 'recentf-exclude ".emacs.d/saveplace.txt")
(setq recentf-max-saved-items 100)

;;(global-set-key "\C-xr" 'recentf-open-files)
(global-set-key "\C-xr" 'helm-recentf)

;; magic return to where you left from 
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saveplace.txt" ))

;; Saves mini buffer history including search and kill ring values, and compile history.
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring compile-history))
(setq savehist-autosave-interval 60)
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)

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

;;;packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

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

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-n") 'scroll-up-one-line)
(global-set-key (kbd "M-p") 'scroll-down-one-line)
(global-set-key (kbd "M-P") 'previous-user-buffer) 
(global-set-key (kbd "M-N") 'next-user-buffer) 
                                        ;(global-set-key (kbd "M-ƒ") 'open-with-finder) ; Command+Option+f

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

(setq compile-command "source ~/.bashrc &&  cd ~/luxion/build/debug && ninja && ctest -L fast -j 8 --output-on-failure")



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

(defun open-with-finder ()
  "Show current buffer-file, or directory if in Dired-mode, in
  Finder (OSX specific)."
  (interactive)
  (if (eq 'dired-mode major-mode)
      (shell-command "open .")
    (shell-command (concat "open -R '" (concat buffer-file-name "'")))))

;; goto next user buffer (no *Messages*, *eshell* etc.)
(defun next-user-buffer ()
  "Switch to next buffer in cyclic order. User buffers are those
  not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (next-buffer))))

;; goto previous user buffer (no *Messages*, *eshell* etc.)
(defun previous-user-buffer ()
  "Switch to previous buffer in cyclic order. User buffers are
  those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (previous-buffer))))

;; Reload the .emacs conf-file
(defun reload-conf ()
  "Reloads ~/.emacs"
  (interactive)
  (load-file "~/.emacs"))

;; Opens (finds) .emacs in another buffer
(defun open-conf () 
  "Opens ~/.emacs for editing"
  (interactive)
  (find-file-other-window "~/.emacs"))

;; Scroll one line up / down
(defun scroll-down-one-line ()
  "Scrolls down one line"
  (interactive)
  (scroll-down 2))

(defun scroll-up-one-line ()
  "Scrolls up one line"
  (interactive)
  (scroll-up 2))

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

;;;;;;;;; XSL mode

(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.fo" . xsl-mode)
        '("\\.xsl" . xsl-mode))
       auto-mode-alist))

;;;;;;;;; DTD mode

;; (autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
;; (autoload 'dtd-etags "tdtd"
;;   "Execute etags on FILESPEC and match on DTD-specific regular expressions."
;;   t)
;; (autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; ;; Turn on font lock when in DTD mode
;; (add-hook 'dtd-mode-hooks
;;           'turn-on-font-lock)

;; (setq auto-mode-alist
;;       (append
;;        (list
;;         '("\\.dcl$" . dtd-mode)
;;         '("\\.dec$" . dtd-mode)
;;         '("\\.dtd$" . dtd-mode)
;;         '("\\.ele$" . dtd-mode)
;;         '("\\.ent$" . dtd-mode)
;;         '("\\.mod$" . dtd-mode))
;;        auto-mode-alist))

;;;;;;;;; PHP-mode

(autoload 'php-mode "php-mode" "Mode for editing PHP source files")
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))

;;;;;;;;; Python-mode

(autoload 'python-mode "python-mode" "Mode for editing Python source files")
(add-to-list 'auto-mode-alist '("\\.py" . python-mode))

;;;;;;;;; svn-mode

;; (require 'psvn)


;;;;;;;;; MIX / MIXAL

;; (autoload 'mixal-mode "mixal-mode" t)
;; (add-to-list 'auto-mode-alist '("\\.mixal\\'" . mixal-mode))
;; (autoload 'mixvm "mixvm" "mixvm/gud interaction" t)

;;;;;;;;; CMake

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

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
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))



(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
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
(add-hook 'objc-mode-hook 'company-mode)



;; ;;
;; ;;org mode
;;

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;TODO(t) STARTED(s) WAITING(w) APPT(a) | DONE(d) CANCELLED(c) DEFERRED(f)
(setq org-todo-keywords (quote (
				(sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)"))))

;; Resume clocking tasks when emacs is restarted
                                        ;(org-clock-persistence-insinuate)
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
   ))


;;magit
;(require 'magit-svn)
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(add-hook 'magit-mode-hook 'magit-load-config-extensions)


;;;;;;; Mulitple cursors

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; terminal colors
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Ace-jump mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (require 'ace-jump-buffer)
;; (global-set-key (kbd "C-x b") 'ace-jump-buffer)
(require 'ace-isearch)
(global-ace-isearch-mode +1)

;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; ;; multiple replace
;; (require 'iedit)

;; rainbows
;(rainbow-mode 't)

;; ;; maya mel - mode
(add-to-list 'auto-mode-alist '("\\.mel$" . c++-mode))

;; ;;;  (setq mel-mode-document-base "/home/narazaki/Lib/Maya/html/ja_JP/")

;; fixme in comments
(require 'fic-mode)
(add-to-list 'fic-highlighted-words '("XXX"))
(add-hook 'c++-mode-hook 'turn-on-fic-mode)

;; ala tail -f for log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; smart mode-line
(sml/setup)
(sml/apply-theme 'dark)
(add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/keyshot_network" ":KS-NET:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/luxion/keyshot/" ":KS:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/luxion" ":Luxion:") t)


;;yas/text
(require 'yasnippet)
(defconst yas-dir (concat user-emacs-directory "snippets"))
(add-to-list 'yas-snippet-dirs yas-dir)

(yas-global-mode 1)



(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-auto-revert-mode nil) ;; Do not auto-revert!

(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;; Regressor tst
(add-to-list 'auto-mode-alist '(".tst" . conf-mode))


;; Window numbering
(setq window-numbering-assign-func
      (lambda () (when (equal (buffer-name) "*compilation*") 9)))
(window-numbering-mode 't)
;; Rainbows
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(global-cwarn-mode 1)


(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))


;; Set defaults used by specific operations.
(setq magit-merge-arguments '("--no-ff"))
(setq magit-pull-arguments '("--rebase"))
(setq magit-cherry-pick-arguments '("-x"))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'devdocs-lookup)
(devdocs-setup)
(global-set-key "\C-cd" 'devdocs-lookup)

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


(setq compilation-scroll-output t)

(require 'vlf-setup)
(setq vlf-application 'dont-ask)

(setq compilation-window-height 30
      compilation-scroll-output 'first-error
      compilation-always-kill t) ;; Don't ask, just start new compilation.


(setq highlight-thing-delay-seconds 0.8)
;(setq highlight-thing-limit-to-defun t) ;; Limit to current function.
(setq highlight-thing-what-thing 'symbol)
(setq highlight-thing-case-sensitive-p t)

(add-hook 'prog-mode-hook 'highlight-thing-mode)

(require 'highlight-current-line)
(setq highlight-current-line-globally t)


(require 'diff-hl)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook 'diff-hl-mode)

;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(setq whitespace-line-column global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)
