;;; package --- Jennifer Kowalsky's .emacs file
;;; Commentary:
;;; Code:

;; MELPA and Marmalade support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "https://marmalade-repo.org/packages/" ))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

 
;;;
;;; Make sure all needed packages are installed.
;;;

(setq package-selected-packages
   (quote
    (clang-format column-marker company company-c-headers company-irony company-irony-c-headers flycheck flycheck-color-mode-line flycheck-rtags rtags cmake-ide origami visual-regexp-steroids dired-rainbow rainbow-delimiters powerline cmake-mode smex nyan-mode multi-term magit)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;;
;;; THEME
;;;
;;(load-theme 'ample-flat t t)
;;(enable-theme 'ample-flat)

;;;
;;; TERMINAL
;;;

;; Force multi-term to use bash, the superior shell.
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

;; Make sure we can send ESC to terminal while in Emacs
(add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape))


;;;
;;; NAVIGATION
;;;

;; Origami mode keybindings
(global-set-key (kbd "C-c o") 'origami-open-node)
(global-set-key (kbd "C-c c") 'origami-close-node)
(global-set-key (kbd "C-c a") 'origami-close-all-nodes)
(global-set-key (kbd "C-c n") 'origami-next-fold)
(global-set-key (kbd "C-c p") 'origami-previous-node)

;; Smex for better M-x
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Bind Hippie Expand to M-<spc>
(global-set-key "\M- " 'hippie-expand)

;; Visual Regex Steriods
;; Use python regular expressions, instead of emacs ones, and highlight matches in buffer.
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-s") 'vr/isearch-forward)

;; Movement
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-p") 'other-frame)

;;;
;;; PROGRAMMING
;;;

;; Rainbow delimiters in most programming modes
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Git tools
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;
;;; PYTHON
;;;

;; Anaconda mode.
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
;; Scons support
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;;;
;;; MISC
;;;

;; hide scroll and tool bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; consistent prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; I like column numbers
(column-number-mode t)

;; No Startup screen
(setq inhibit-startup-screen t)

;; save backups in a backups directory instead of cluttering the working dir
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Always spaces
(setq-default indent-tabs-mode nil)

;; turn off audible bell
(setq ring-bell-function 'ignore)

;; Kill auto-fill mode, for it is evil.
(auto-fill-mode -1)

;;;
;;; FUN
;;;

;; Nyan mode
(nyan-mode 1)
(nyan-start-animation)


;;;
;;; Final Startup
;;;

;; Start emacs in server mode if it isn't that way already.
(server-start)

(provide '.emacs)
;;; .emacs ends here
