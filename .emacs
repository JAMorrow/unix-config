;;; package --- Jennifer Kowalsky's .emacs file
;;; Commentary:
;;; This is the Linux version.  It points to the Global configs in .emacs_kowalsky
;;; Code:

(setq warning-minimum-level :emergency)


;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/elpa/")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; IGDEV C++ coding style
(load-file "~/.emacs.d/elpa/cmode.el")

;; Abbrev Mode Goodness
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;; Load global configurations
(load-file "~/.emacs_kowalsky")

;; Force multi-term to use bash, the superior shell.
(require 'multi-term)
(setq multi-term-program "/bin/bash")

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape))

;;(require 'org)
;; Tell emacs where the agenda files are
;; Set Org Agenda files
;; I keep my org files in ~/org, so everything under that is to be included in the agenda.
(setq org-agenda-files (list "~/" "~/org/" "~/org/projects/"))

;; for org mode, always openin visual-line-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; for org mode, always openin visual-line-mode
;;(add-to-list 'auto-mode-alist '("\\.org$" . visual-line-mode))

;; Turn on org-bullets, which makes titles look nicer
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

;; Nyan mode
(nyan-mode 1)
(nyan-start-animation)

;; Very simple, I just keep track of when tasks enter each phase.
(setq org-todo-keywords
      '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)")))

;; org flags for Context
;; Tags with fast selection keys
(setq org-tag-alist (quote (("SEDG" . ?s)
                            ("dev" . ?d)
                            ("meeting" . ?m)
                            ("waiting" . ?w)
			    ("question" . ?q)
                            ("PERSONAL" . ?P))))

;; Color coding org mode tags
(add-hook 'org-finalize-agenda-hook
	  (lambda ()
	    (save-excursion
	      (color-org-header "Work:"  "orange")
	      (color-org-header "REPEAT:"  "pink")
	      (color-org-header "Muse:"  "maroon")
	      (color-org-header "IoT:"      "MediumPurple")
              (color-org-header "Mystique:"  "MediumPurple")
	      (color-org-header "task:"  "Gold")
	      (color-org-header "meeting:"  "LightPink")
	      (color-org-header "question:"  "LightGreen")
              )))

(defun color-org-header (tag col)
  ""
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
    (add-text-properties (match-beginning 0) (point-at-eol)
                         `(face (:foreground ,col)))))

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Add diary to Agenda
'(org-agenda-include-diary t)
'(org-agenda-span (quote week))
(setq org-support-shift-select t)

;; Kill auto-fill mode, for it is evil.
(auto-fill-mode -1)

;; Anaconda mode.
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

;; Git tools
(global-set-key (kbd "C-x g") 'magit-status)

(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;; Movement
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-p") 'other-frame)

;; For Java development
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" default)))
 '(desktop-restore-eager 5)
 '(desktop-save t)
 '(eclim-eclipse-dirs (quote ("/godzilla/usr2/jkowalsk/eclipse")))
 '(eclim-executable "/godzilla/usr2/jkowalsk/eclipse/eclim")
 '(fringe-mode 10 nil (fringe))
 '(global-auto-revert-mode nil)
 '(global-reveal-mode t)
 '(inhibit-startup-screen t)
 '(linum-format "%3i")
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

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)


(setq-default indent-tabs-mode nil)

;; Start emacs in server mode if it isn't that way already.
(server-start)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(provide '.emacs)
;;; .emacs ends here
