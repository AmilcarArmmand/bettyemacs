;;; Package --- Summary
;; Emacs setup for Linux, OSX, (and Windows OS testing pending)
;;; Commentary:  The main package repository for Emacs
;;; Code: Add melpa (https only) and gnu elpa (https only)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)
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
 ;; custom-set-variables was added by Custom.
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
;;;  Add functions to determine system
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;;;  Load theme
(load-theme 'tango-dark)

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

;;; which-key
(require 'which-key)
(which-key-mode)

;;; dired - directory edit mode
(use-package dired
  :config
  (setq dired-dwin-target t)
  :hook (dired-mode . dired-hide-details-mode))
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-sidebar
  :ensure t
  :bind (("C-c s" . dired-sidebar-toggle-sidebar)))

;;; Set the backup folder with rudimentary version control instead of ~ files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;;; smartparens auto-close parenthesis etc
(add-to-list 'load-path "/Users/amilcararmmand/smartparens")
;(require 'smartparens-mode)
(smartparens-global-mode 1)
(use-package smartparens
	:ensure t
	:config
	(sp-use-paredit-bindings)
	(add-hook 'prog-mode-hook #'smartparens-global-mode)
	(sp-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;;; encourage mode
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
;;; Autocomplete mode
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)
; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.3/include")
  )
; use 'gcc -xc++ -E -v -' to find where c header files are stored locally, results below
; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.3/include
; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include
; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
; now let's call the my:ac-c-header-init function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;; iedit
(use-package iedit
  :ensure t
  :bind (("C-c i" . iedit-mode)))

;;; Betty-style and Flycheck coding linter
(load "~/.emacs.d/private/Betty/betty-style")
(add-to-list 'flycheck-checkers 'betty-style)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq flycheck-gcc-pedantic t)
(setq flycheck-gcc-warnings '("all" "extra" "error"))

;;; Major modes ???
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
