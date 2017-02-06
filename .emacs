;;; package --- Jennifer Kowalsky's .emacs file
;;; Commentary:
;;; This is the Linux version.  It points to the Global configs in .emacs_kowalsky
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/elpa/")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; MELPA support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;;
;;; THEME
;;;
(load-theme 'ample-flat t t)
(enable-theme 'ample-flat)

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

;; Rainbow delimiters in most programming modes
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;;
;;; NAVIGATION
;;;

;; Which-key -- help learn keybindings.
(require 'which-key)
(which-key-mode)

;; Origami m ode keybindings
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
;; See what commit inspired the change
(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

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

(require 'cheatsheet)
(load-file "~/.emacs_cheatsheet")

;;(require 'template)
(global-set-key (kbd "C-c C-t") 'template-new-file)

;; hide scroll and tool bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; save backups in a backups directory instead of cluttering the working dir
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq-default indent-tabs-mode nil)

;; turn off audible bell
(setq ring-bell-function 'ignore)

;; Kill auto-fill mode, for it is evil.
(auto-fill-mode -1)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)


;;;
;;; FUN
;;;

;; Nyan mode
(nyan-mode 1)
(nyan-start-animation)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(desktop-restore-eager 5)
 '(desktop-save t)
 '(fringe-mode 10 nil (fringe))
 '(global-auto-revert-mode nil)
 '(global-reveal-mode t)
 '(inhibit-startup-screen t)
 '(linum-format "%3i")
 '(package-selected-packages
   (quote
    (origami cheatsheet ample-theme moe-theme which-key visual-regexp-steroids dired-rainbow rainbow-delimiters powerline fish-mode cmake-mode smex nyan-mode nsis-mode multi-term magit-gitflow git-messenger)))
 '(vc-annotate-background nil)
 '(vc-annotate-very-old-color nil)
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0))))))


;; Start emacs in server mode if it isn't that way already.
(server-start)

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
