;; -*- lexical-binding: t -*-
;; Commentary:
;; This file bootstraps the configuration, which is divided into a number of
;; files.  Emacs setup for Linux, OSX, (and Windows NT testing pending)

;;; Code:

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Darius is powering up... Be patient, %s." current-user)

;; Produce backtrace help messages when errors occur on startup to help fix.
(setq debug-on-error t) ;; turn on debug-on-error

(let ((minver "24.5"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t, disable with nil
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windows* (eq system-type 'windows-nt))
(setq debug-on-error nil) ;; turn off debug-on-error

;;-----------------------------------------------------------------------------
;; Startup optimizations
;;-----------------------------------------------------------------------------
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; Adjust garbage collection thresholds. Values are in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

;;; Networking and security
;; Use `with-eval-after-load' instead of `straight-use-feature' because we have
;; yet to set up package management.

;; Support for SSL/TLS connections, using the GnuTLS library.
(with-eval-after-load 'gnutls

  ;; `use-package' does this for us normally.
  (eval-when-compile
    (require 'gnutls))

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;;; ---------------------------------------------------------------------------
(setq tls-checktrust t)

;;=============================================================================
;; Bootstrap the package manager, `straight.el`
;;=============================================================================
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

;;=============================================================================
;;; Package `use-package`
;;=============================================================================
(straight-use-package 'use-package)

;; Load features lazily unless otherwise determined by if `demand' is
;; is present for eager loading
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq straight-use-package-by-default t)

;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;=============================================================================
;; mac keybindings
;;=============================================================================
(setq mac-command-modifier 'meta) ; set cmd key to Meta
(setq mac-option-modifier 'super) ; set option key to Super
(setq mac-control-modifier 'control) ; set control key to Control
(setq ns-function-modifier 'hyper) ; set function key to Hyper

;;; Disable the bell
(setq ring-bell-function 'ignore)
(use-package no-littering
  :demand t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load configurations for features and modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'wgrep)
(straight-use-package 'scratch)

;;;  line and column numbering
(setq column-number-mode t)
(setq global-linum-mode t)
(global-linum-mode t)
(setq linum-format "%d ")

;; `Selectrum' is an incremental completion and narrowing framework.
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

;; Increases the default setting
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
 
(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :init

  (ctrlf-mode +1))

;;;; Mouse integration
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(xterm-mouse-mode t)

;;=============================================================================
;;; encourage mode
;;=============================================================================
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

;;=============================================================================
;; Load theme last
;;=============================================================================

(use-package zerodark-theme
  :straight (:host github :repo "NicolasPetton/zerodark-theme")
(require 'zerodark-theme)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'zerodark
   `(selectrum-current-candidate
     ((,class (:background "#48384c"
                           :weight bold
                           :foreground "#c678dd"))))
   `(selectrum-primary-highlight ((,class (:foreground "#da8548"))))
   `(selectrum-secondary-highlight ((,class (:foreground "#98be65"))))))

(enable-theme 'zerodark)


(provide 'init)\n


;; coding: utf-8
;; no-byte-compile: t
;;; init.el ends here
