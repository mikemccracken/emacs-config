;(toggle-debug-on-error)
(require 'package)
(package-initialize)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )
; was in config:'(package-archives (quote (("marmalade" . "http://marmalade-repo.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/") ("org" . "http://orgmode.org/elpa/"))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile
	     :ensure t
	     :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(require 'org-table)

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda()
                        (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
;; workaround virtualbox and trusty removing my cmd-space
; overrides newline-and-indent which I don't use...
(global-set-key "\C-j" 'set-mark-command)

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


(use-package magit
  :ensure t
  :bind (("<f9>" . magit-status))
  :pin melpa
  )


;; PYTHON

;; (add-to-list 'load-path "~/.emacs.d/flymake-pycheckers")
;; (require 'flymake-pycheckers)
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.py\\'" flymake-pycheckers-init))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;(add-to-list 'load-path "~/.emacs.d/")
;;;;;;; slow???(require 'flymake-cursor)

;; Python Jedi mode
(use-package jedi
  :ensure t
  :init
  (jedi:install-server)
  (setq jedi:setup-keys t)
  (defun jedi-goto-in-other-window ()
    (interactive)
    (jedi:goto-definition t))
  (add-hook 'python-mode-hook 'jedi:setup)
  
  ;; ;; or just autocompletion:
  ;; ; (add-hook 'python-mode-hook 'jedi:ac-setup)


:bind (("C->" . jedi-goto-in-other-window))
)

(use-package pos-tip
  :ensure t)

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

;; YASNIPPET
;(require 'yasnippet)
;(yas/global-mode 1)


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
(use-package win-switch
  :ensure t
  :bind (("C-x o" . win-switch-dispatch))
  )

;; HELM
(use-package helm
	     :ensure t
	     :diminish helm-mode
	     :init
	     ;; must set before helm-config,  otherwise helm use default
	     ;; prefix "C-x c", which is inconvenient because you can
	     ;; accidentially pressed "C-x C-c"
	     (setq helm-command-prefix-key "C-c h")
	     (require 'helm-config)
	     (require 'helm-eshell)
	     (require 'helm-files)
	     (require 'helm-grep)
	     (setq
	      helm-google-suggest-use-curl-p t
	      helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
	      helm-quick-update t ; do not display invisible candidates
	      helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
	      helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
	      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

	      helm-split-window-default-side 'other ;; open helm buffer in another window
	      helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
	      helm-buffers-favorite-modes (append helm-buffers-favorite-modes
						  '(picture-mode artist-mode))
	      helm-candidate-number-limit 200 ; limit the number of displayed canidates
	      helm-M-x-requires-pattern 0     ; show all candidates when set to 0
	      helm-boring-file-regexp-list
	      '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
	      helm-ff-file-name-history-use-recentf t
	      helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
	      ido-use-virtual-buffers t      ; Needed in helm-buffers-list
	      helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
	      )
	     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
	     (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
	     (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
	     
	     (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
	     (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
	     (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
	     
	     (helm-mode)

	     :bind (("C-c o" . helm-occur)
		    ("C-x b" . helm-mini)
                    ("C-x C-f" . helm-find-files))
	     )

(use-package helm-descbinds
	     :defer t
	     :ensure t
	     :bind (("C-h b" . helm-descbinds)
		             ("C-h w" . helm-descbinds)))


;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)


;;; END HELM


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "Anonymous Pro"))))
 '(autoface-default ((((type ns)) (:height 90 :family "Monaco"))))
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 90 :family "Monaco"))))
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
 '(ack-and-a-half-arguments (quote ("--nopager")))
 '(ack-and-a-half-executable "ack")
 '(ack-and-a-half-prompt-for-directory t)
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(c-default-style
   (quote
    ((objc-mode . "cc-mode")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "cc-mode"))))
 '(c-mode-common-hook
   (quote
    ((lambda nil
       (dolist
           (k
            (quote
             (":" ">" ";" "<" "{" "}")))
         (define-key
           (symbol-value
            (make-local-variable
             (quote yas-keymap)))
           k
           (quote self-insert-command))))
     elide-head)))
 '(c-offsets-alist
   (quote
    ((defun-open . --)
     (inline-open . 0)
     (substatement-open . 0)
     (innamespace .
                  [0]))))
 '(column-number-mode t)
 '(compilation-error-regexp-alist
   (quote
    (("Loc: \\[\\(.*.qml\\)(\\([0-9]+\\))" 1 2)
     absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint)))
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
 '(elide-head-headers-to-hide
   (quote
    (("Canonical Ltd." . "delete it here.")
     ("is free software[:;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
     ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.")
     ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\."))))
 '(face-font-family-alternatives
   (quote
    (("monaco" "courier" "fixed")
     ("helv" "helvetica" "arial" "fixed"))))
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.py\\'" flymake-pycheckers-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.html?\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init))))
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
 '(pastebin-default-subdomain "paste.ubuntu.com")
 '(python-python-command "/usr/bin/python")
 '(safe-local-variable-values
   (quote
    ((test-case-name . twisted\.names\.test)
     (test-case-name . twisted\.names\.test\.test_names)
     (encoding . utf-8))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 40)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
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
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab empty space-after-tab)))
 '(win-switch-feedback-foreground-color "grey")
 '(win-switch-other-window-first nil)
 '(win-switch-window-threshold 0)
 '(window-combination-resize t)
 '(window-min-width 80))

(put 'narrow-to-region 'disabled nil)


;;; FLYMAKE MODE:
  (defvar pycheckers-cmd "/home/mmccrack/bin/pycheckers")
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycheckers-cmd (list local-file)))
    )

(use-package flymake
  :config
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pycheckers-init))
  (add-hook 'find-file-hook 'flymake-find-file-hook))

(use-package flymake-cursor
  :ensure t)


(use-package ack-and-a-half
  :ensure t
  :init
  (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
  :config
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
  )

;; GO MODE STUFF

(eval-after-load "go-mode"
  '(require 'flymake-go))

(message "CONFIG DONE")
;;(profiler-start 'cpu)
