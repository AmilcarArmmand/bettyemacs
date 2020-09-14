;;; init.el -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; files.  Emacs setup for Linux, OSX, (and Windows NT testing pending)

;;; Code:

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

;; Adjust garbage collection thresholds. Values are in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

;;; Networking and security

;; Use `with-eval-after-load' instead of `straight-use-feature' because we have
;; yet to set up package management.

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
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


;;; Disable the bell
(setq ring-bell-function 'ignore)

;;; .emacs.el ends here
