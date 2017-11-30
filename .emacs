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

;; set tabs to indent as white spaces and set default tab width to 4 white spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; cpputils-cmake 
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
                '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))


;;Highlight column 80 in foo mode.
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

(setq cppcm-get-executable-full-path-callback
      (lambda (path type tgt-name)
        ;; path is the supposed-to-be target's full path
        ;; type is either add_executabe or add_library
        ;; tgt-name is the target to built. The target's file extension is stripped
        (message "cppcm-get-executable-full-path-callback called => %s %s %s" path type tgt-name)
        (let* ((dir (file-name-directory path))
               (file (file-name-nondirectory path)))
          (cond
           ((string= type "add_executable")
            (setq path (concat dir "bin/" file)))
           ;; for add_library
           (t (setq path (concat dir "lib/" file)))
           ))
        ;; return the new path
        (message "cppcm-get-executable-full-path-callback called => path=%s" path)
        path))

;; Rainbow delimiters in most programming modes
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Paredit everywhere
(require 'paredit)
(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;;
;;; C++
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake ide & rtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)

;; set path to project build directory
(setq cmake-ide-build-dir
      (expand-file-name "/usr0/igdev/sw/working/jkowalsk/starsky/build/linux-desktop-debug"))
;; CURRENTLY: hardcode to build dir of default project
;; TODO: fix via .dir-locals.el

;; set path to rtag executables
(setq rtags-path
      (expand-file-name "/godzilla/usr1/jkowalsk/rtags/rtags/build"))

;; invoke cmake-ide setup
(require 'cmake-ide)
(cmake-ide-setup)

;; start the rdm process unless the process is already running.
;; (I prefer to launch rdm externally and prior to Emacs)
;;(rtags-start-process-unless-running)

;; Enable rtags-diagnostics.
;;(setq rtags-autostart-diagnostics t)
;;(rtags-diagnostics)


;; Timeout for reparse on onsaved buffers
(rtags-set-periodic-reparse-timeout 0.5)

;; Rtags standard keybindings ([M-. on symbol to go to bindings])
(rtags-enable-standard-keybindings)

;; Custom keybindings
(global-set-key (kbd "<home>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<prior>") 'rtags-location-stack-back)
(global-set-key (kbd "<next>") 'rtags-location-stack-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(global-flycheck-mode)

;; color model line
(require 'flycheck-color-mode-line)
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; rtags with Flycheck (syntax checking)
(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irony (C/C++ minor mode powered by libclang) and company for completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Enable company mode
(require 'company)
(global-company-mode)

;; company-irony
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-M-tab
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)
;; If the repo does not have a .clang-format files, one can
;; be created using google style:
;; clang-format -style=google -dump-config > .clang-format
;; In this, default indent is 2 (see 'IndentWidth' key in generated file).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Protobuf
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; **************************************************************************
;; IGDEV Emacs Coding Style mode files
;; ***************************************************************************
(load-file "~/unix-config/cmode.el") 

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
