;;; Package --- Summary
;; Emacs setup for Linux, OSX, (and Windows OS testing pending)
;;; Commentary:  The main package configuration for Emacs using
;;; straight.el
;;; Code:

;;; Startup optimizations

;;; Load built-in utility libraries
(require 'cl-lib)


;; Adjust garbage collection thresholds
(setq gc-cons-threshold (* 100 1024 1024))

(setq package-check-signature nil)

;;; ---------------------------------------------------------------------------
(setq tls-checktrust t)

;;; This snippet is ready to work in both UNIX-like and Windows OS
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat "python3 -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))
(defun check-tls-config ()
  "Check for correctness in the TLS configuration for Emacs."
  (interactive)
  (let ((bad-hosts
         (cl-loop for bad
                  in `("https://wrong.host.badssl.com/"
                       "https://self-signed.badssl.com/")
                  if (condition-case e
                         (url-retrieve
                          bad (lambda (retrieved) t))
                       (error nil))
                  collect bad)))
    (if bad-hosts
        (error (format "TLS misconfigured; retrieved %s ok" bad-hosts))
      (url-retrieve "https://badssl.com"
                    (lambda (retrieved) t)))))
;; Added by Package.el. Placed before configurations of installed packages.
(custom-set-variables
 ;; custom-set-variables was added by Custom.[<64;44;20M]
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (el-autoyas el-fly-indent-mode el-init-viewer el-init ac-c-headers gnu-elpa-keyring-update python-docstring sphinx-doc encourage-mode package-build package+ dired-sidebar all-the-icons-dired dash-functional cl-lib-highlight iedit smartparens-mode use-package cheat-sh pdf-tools yasnippet-snippets flymd flycheck-clang-analyzer flycheck-clang-tidy company-shell company-irony company-irony-c-headers company-jedi company-ctags auto-complete cl-format cl-generic cl-lib cl-libify auto-complete-auctex auto-complete-c-headers auto-complete-chunk auto-complete-clang elpy auctex company-auctex flycheck-indicator latex-extra latex-math-preview latex-pretty-symbols latex-preview-pane markdown-mode markdown-mode+ markdown-preview-mode org-ac org-babel-eval-in-repl company-c-headers counsel-tramp docker docker-tramp dockerfile-mode flycheck flycheck-pycheckers flycheck-relint flycheck-xcode flyspell-correct ivy-yasnippet org python python-info python-mode rainbow-delimiters smartparens json-mode jsonrpc which-key yasnippet yasnippet-classic-snippets company ivy))))
(custom-set-faces

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap the package manager, `straight.el`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package `use-package`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'use-package)

;; Load features lazily unless otherwise determined by if `demand' is
;; is present for eager loading
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq straight-use-package-by-default t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package `blackout'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package `no-littering'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package no-littering
  :demand t)

;;; Prevent Emacs-provided Org-mode from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use my mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking Radian CI.
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Add functions to determine system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Load theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'tango-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse integration
;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (display-graphic-p)

  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode 1))

;; Enable scrolling with the mouse wheel.
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;;;  whitespace indicator
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;;;  line and column numbering
(setq column-number-mode t)
(setq global-linum-mode t)
(global-linum-mode t)
(setq linum-format "%d ")

;;;  Disable yes-or-no messages and set to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable startup message
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;;; Scroll inside the compilation buffer
(setq compilation-scroll-output t)

;;; Disable the warning when killing a buffer with process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

;;; Disable the bell
(setq ring-bell-function 'ignore)

;;; cheat-sh package to reference commands
(use-package cheat-sh
	     :ensure t
	     :bind (("C-c ?" . cheat-sh)))

;;; Package `which-key`
(use-package which-key
  :demand t
  :config
  (which-key-mode +1))


;;; Set the backup folder with rudimentary version control instead of ~ files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;;; smartparens auto-close parenthesis etc
;; (add-to-list 'load-path "/Users/amilcararmmand/smartparens")
(use-package smartparens
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens in all buffers.
  (smartparens-global-mode +1)
	:ensure t
	:config
	(sp-use-paredit-bindings)
	(add-hook 'prog-mode-hook #'smartparens-global-mode)
	(sp-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(show-paren-mode 1)

;;; Package `encourage mode' because everyone needs encouragement
(use-package encourage-mode
  :ensure t
  :config
;; Activate encourage-mode
  (encourage-mode t))
(setq encourage-encouragements
      (nconc encourage-encouragements
	     '("Excellent!"
	       "Hot sandwich!"
	       "Be the change!"
	       "Nice!"
	       "Outstanding!"
	       "Ossum!"
	       "Pointers!"
	       "Quit it!"
	       "Scwhanky!"
	       "Spanakopita!"
	       "SPHINX!"
	       "Supergood!"
	       "Sweet!"
	       "I am TYT!"
	       "That is so Batman!"
	       "Well done, you!"
	       "Whoa!")))

;;; May consider company competion method as and alt to autocomplete.
;; Selectrum, Ido, Hekm, Ivy, Icompletem...
;; Package `selectrum' is an incremental completion and narrowing
;; framework, which provides a user interface for choosing from a list
;; of options by typing a query to narrow the list, and then selecting
;; one of the remaining candidates.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; Increase default settings to 1000 entries.
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

;; Package `ctrlf' provides a replacement for `isearch' that is
;; similar to the default macOS text search interfaces in web
;; browsers and other programs.
(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :init

  (ctrlf-mode +1))

;;; yasnippets


;;; iedit
(use-package iedit
  :ensure t
  :bind (("C-c i" . iedit-mode)))

;;; Flycheck package

;;; Betty-style and Flycheck coding linter
;; (load "~/.emacs.d/private/Betty/betty-style")
;; (add-to-list 'flycheck-checkers 'betty-style)
;; (straight-use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))
;;(setq flycheck-gcc-pedantic t)
;;(setq Eflycheck-gcc-warnings '("all" "extra" "error"))

;;; Major mode for C
(setq c-default-style "bsd"
      c-basic-offset 8
      tab-width 8
      indent-tabs-mode t)

;;;  Python
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

;;;  Package for Python docstrings, features automatic docstring creation and
;;; highlighting in them.
(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

(use-package python-docstring
  :ensure t
  :config (setq python-docstring-sentence-end-double-space nil)
  :hook (python-mode . python-docstring-mode))

;;; .emacs.el ends here
