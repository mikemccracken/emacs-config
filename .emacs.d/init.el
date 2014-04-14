(require 'cask "~/.cask/cask.el")
(cask-initialize)

(global-set-key [down-mouse-3] 'imenu)
(global-font-lock-mode 1)

(xterm-mouse-mode)

(setq x-super-keysym 'meta)

(global-set-key [mouse-4] 
                (function (lambda () 
                            (interactive) 
                            (scroll-down 1))))
(global-set-key [mouse-5] 
                (function (lambda () 
                            (interactive) 
                            (scroll-up 1)))) 

;; C++ stuff
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

(defun insert-date ()
  "Insert the current date according to the variable
\"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string "%m/%d %H:%M: "
                              (current-time))))
(defun insert-time ()
  "Insert the current time according to the variable
\"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string "%H:%M: "
                              (current-time))))

(defun tag-as-debug ()
  "mark lines in region as debug, so it's easy to find them later"
  (interactive "*")
  (end-of-line)
  (insert comment-start)
  (insert " MMCC DEBUG, ")
  (insert-date))

;; Function keys and handy bindings
(global-set-key [f3] 'insert-date)
(global-set-key [f4] 'insert-time)
(global-set-key [f5] 'tag-as-debug)
(global-set-key [f8] 'compile)

(global-set-key "\C-co" 'occur)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)


(defun kill-whole-line ()
  "Kill an entire line including trailing newline. Handy to avoid
    C-aa when using screen..."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(global-set-key "\C-ck" 'kill-whole-line)
(global-set-key "\C-c\C-k" 'kill-whole-line)

(setq load-path (cons "~/elisp/" load-path))


;; PYTHON

;; python keys
;; maybe these aren't so useful:
;; (eval-after-load 'python
;;   '(define-key python-mode-map (kbd "M-n") 'python-end-of-block))
;; (eval-after-load 'python
;;   '(define-key python-mode-map (kbd "M-p") 'python-beginning-of-block))

;; Python Jedi mode




;; NOTE: make sure you set setup-keys before (el-get 'sync) below:
(setq jedi:setup-keys t)
(defun jedi-goto-in-other-window ()
  (interactive)
  (jedi:goto-definition t))

(global-set-key (kbd "C->") 'jedi-goto-in-other-window)


(add-hook 'python-mode-hook 'jedi:setup)
;; ;; or just autocompletion:
;; ; (add-hook 'python-mode-hook 'jedi:ac-setup)



;; POS-TIP tooltip stuff, after el-get
(require 'pos-tip)

;;; flyspell in latex
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                nxml-mode-hook
                crontab-mode-hook
                perl-mode-hook
                javascript-mode-hook
                LaTeX-mode-hook
                c-mode-hook
                c++-mode-hook
                qml-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;;; linum for line numbers (not in emacs 22)
(when (>= emacs-major-version 24) 
      (global-linum-mode 1)
      (setq linum-format "%3d ")
      )

;;; ANYTHING.EL

;; (require 'anything-config)
;; (global-set-key "\C-xa" 'anything)
;; (require 'anything-match-plugin)

;; YASNIPPET
(add-to-list 'load-path
	     "~/elisp/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)


;; handy text manip stuff

(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))


;; WIN SWITCH FOR THE WIN SWITCHING
(require 'win-switch)
(global-set-key "\C-xo" 'win-switch-dispatch)

;; winner mode for window config undo:
(winner-mode 1)

;; replaces iswitchb:
(require 'ido)
(ido-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "Anonymous Pro"))))
 '(autoface-default ((((type ns)) (:height 90 :family "Monaco"))))
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 90 :family "Monaco"))))
 '(erc-current-nick-face ((t (:background "yellow" :foreground "black"))))
 '(erc-direct-msg-face ((t (:foreground "dark blue"))))
 '(erc-input-face ((t (:foreground "#666666"))))
 '(erc-keyword-face ((t (:background "gray70" :foreground "black"))))
 '(erc-nick-msg-face ((t (:foreground "IndianRed" :weight bold))))
 '(erc-notice-face ((t (:foreground "gray86"))))
 '(erc-timestamp-face ((t (:foreground "gray44"))))
 '(font-lock-keyword-face ((((class color) (min-colors 8)) (:foreground "black" :weight bold))))
 '(linum ((t (:inherit (shadow default) :background "#efefef" :height 1.0))))
 '(notes-bold-face ((t (:weight bold))) t)
 '(notes-mode-default ((t (:inherit indented-text-mode-default :height 90 :family "Monaco"))) t)
 '(popup-tip-face ((t (:background "khaki1" :foreground "black" :box (:line-width 3 :color "grey75" :style released-button)))))
 '(python-mode-default ((t (:inherit autoface-default :height 90 :family "Monaco"))) t)
 '(term-mode-default ((t (:inherit autoface-default :height 90 :family "Monaco"))) t)
 '(text-mode-default ((((type ns)) (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 90 :width normal :family "Monaco"))))
 '(whitespace-space ((t (:foreground "gray90")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.4)
 '(ack-and-a-half-arguments (quote ("--follow")))
 '(ack-and-a-half-executable "ack")
 '(ack-and-a-half-prompt-for-directory t)
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(c-default-style (quote ((objc-mode . "cc-mode") (java-mode . "java") (awk-mode . "awk") (other . "cc-mode"))))
 '(c-mode-common-hook (quote ((lambda nil (dolist (k (quote (":" ">" ";" "<" "{" "}"))) (define-key (symbol-value (make-local-variable (quote yas-keymap))) k (quote self-insert-command)))) elide-head)))
 '(c-offsets-alist (quote ((defun-open . --) (inline-open . 0) (substatement-open . 0) (innamespace . [0]))))
 '(column-number-mode t)
 '(compilation-error-regexp-alist (quote (("Loc: \\[\\(.*.qml\\)(\\([0-9]+\\))" 1 2) absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint)))
 '(compilation-read-command nil)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-visited t)
 '(compilation-window-height nil)
 '(desktop-restore-eager 7)
 '(desktop-save-mode t)
 '(diary-file "~/Dropbox/jen-mike-shared/hadleydiary.txt")
 '(diff-switches "-u")
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(electric-pair-mode nil)
 '(elide-head-headers-to-hide (quote (("Canonical Ltd." . "delete it here.") ("is free software[:;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.") ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.") ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\."))))
 '(erc-current-nick-highlight-type (quote all))
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT" "MODE")))
 '(erc-keyword-highlight-type (quote all))
 '(erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-match-mode t)
 '(erc-modules (quote (autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands notify readonly ring stamp highlight-nicknames)))
 '(erc-move-to-prompt-mode t)
 '(erc-nick "mmcc")
 '(erc-server-send-ping-timeout 240)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t)
 '(erc-track-shorten-start 4)
 '(erc-track-when-inactive t)
 '(face-font-family-alternatives (quote (("monaco" "courier" "fixed") ("helv" "helvetica" "arial" "fixed"))))
 '(flymake-allowed-file-name-masks (quote (("\\.py\\'" flymake-pycheckers-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-gui-warnings-enabled nil)
 '(flymake-log-level 0)
 '(font-use-system-font nil)
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-complete-mode t)
 '(global-linum-mode t)
 '(global-whitespace-mode t)
 '(haskell-stylish-on-save t)
 '(indent-tabs-mode nil)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "/usr/bin/aspell")
 '(jedi:tooltip-method (quote (pos-tip)))
 '(ns-command-modifier (quote meta))
 '(nyan-bar-length 16)
 '(nyan-mode t)
 '(package-archives (quote (("marmalade" . "http://marmalade-repo.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/") ("org" . "http://orgmode.org/elpa/"))))
 '(pastebin-default-subdomain "paste.ubuntu.com")
 '(python-python-command "/usr/bin/python")
 '(safe-local-variable-values (quote ((test-case-name . twisted\.names\.test) (test-case-name . twisted\.names\.test\.test_names) (encoding . utf-8))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-track-mouse-flag t)
 '(speedbar-use-images nil)
 '(split-height-threshold 100)
 '(split-width-threshold 100)
 '(timeclock-modeline-display t nil (timeclock))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-bzr-program "/usr/bin/bzr")
 '(vc-hg-program "/usr/local/bin/hg")
 '(whitespace-global-modes (quote (python)))
 '(whitespace-style (quote (face tabs trailing space-before-tab empty space-after-tab)))
 '(win-switch-feedback-foreground-color "grey")
 '(win-switch-other-window-first nil)
 '(win-switch-window-threshold 0)
 '(window-combination-resize t)
 '(window-min-width 80))

; so we pick up ack and others:
(setenv "PATH" (concat (getenv "PATH")
		       ":/Users/mmccrack/bin"))
		       

(put 'narrow-to-region 'disabled nil)


;;; FLYMAKE MODE:

(require 'flymake)

(defvar pycheckers-cmd "/home/mmccrack/bin/pycheckers")

(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycheckers-cmd (list local-file)))
    )
  (add-to-list 'flymake-allowed-file-name-masks 
 	       '("\\.py\\'" flymake-pycheckers-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(load-library "flymake-cursor-error")


;;     (add-to-list 'load-path "/Users/mmccrack/elisp/ack-and-a-half.el")
     (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
     (autoload 'ack-and-a-half "ack-and-a-half" nil t)
     (autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
     (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
     (defalias 'ack 'ack-and-a-half)
     (defalias 'ack-same 'ack-and-a-half-same)
     (defalias 'ack-find-file 'ack-and-a-half-find-file)
     (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; ioccur
(require 'ioccur)

