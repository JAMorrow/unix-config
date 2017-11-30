;; ***************************************************************
;; Standard Configurations for major modes are placed in this file
;; ***************************************************************

;; ***************************
;; Customizations for CC Mode.
;; ****************************

;; Create IGDEV C++ coding style. This will take precedence over any setting
;; of the syntactic symbol made by a style.
;; Described in:
;; http://cc-mode.sourceforge.net/html-manual/Style-Variables.html#Style-Variables
(defconst igdev-c++-style
  '(
    ;; Indentation
    ;;; c-indent-comment-alist
    ;;; (c-indent-comments-syntactically-p . nil) ;; default

    ;; Doc Comments
    ;;; c-doc-comment-style

    ;; Custom Filling and Breaking
    ;;; c-block-comment-prefix
    ;;; c-comment-prefix-regexp

    ;; Hanging Braces
    (c-hanging-braces-alist . (
        ;; Format and description defined in:
	;; http://cc-mode.sourceforge.net/html-manual/Hanging-Braces.html#Hanging-Braces
	;;;(defun-open)
	;;;(defun-close)
	;;;(class-open)
	;;;(class-close)
	;;;(inline-open)
	;;;(inline-close)
	;;;(block-open)
	(block-close . c-snug-do-while)
	;;;(substatement-open)
	;;;(statement-case-open)
	(statement before after)
	;;;(Statement-cont)
	;;;(extern-lang-open)
	;;;(extern-lang-close)
	;;;(brace-list-open)
	;;;(brace-list-close)
	;;;(brace-list-intro)
	;;;(brace-entry-open)
	;;;(namespace-open)
	;;;(namespace-close)
    ))

    ;; Hanging Colons
    ;;;(c-hanging-colons-alist . (
        ;; Format and description defined in:
	;; http://cc-mode.sourceforge.net/html-manual/Hanging-Colons.html#Hanging-Colons
        ;;;(case-label)
        ;;;(label)
        ;;;(access-label)
        ;;;(member-init-intro)
        ;;;(inher-intro)
    ;;;))

    ;; Hanging Semicolons and Commas
    ;;; c-hanging-semi&comma-criteria

    ;; Clean-ups
    ;; Format and description defined in:
    ;; http://cc-mode.sourceforge.net/html-manual/Clean_002dups.html#Clean_002dups
    (c-cleanup-list . (list
        ;;;brace-else-brace
        ;;;brace-elseif-brace
        ;;;(brace-catch-brac)
        empty-defun-braces
        defun-close-semi
        list-close-comma
        scope-operator
        ;;;one-liner-defun
        ;;;space-before-funcall
        ;;;compact-empty-funcall
        ;;;comment-close-slash ;; I think is implemented in cc mode v5.31
    ))

    ;; Customizing Indentation
    ;;; (c-basic-offset . 4) ;; default

    ;; c-offsets-alist
    (c-offsets-alist . (
	;; Format defined in: 
        ;; http://cc-mode.sourceforge.net/html-manual/c_002doffsets_002dalist.html#c_002doffsets_002dalist
        ;; Inside a multiline string. Literal Symbols.
        ;;;(string . x)

	;; Inside a multiline C style block comment. Literal Symbols.
	;;(c . x)

	;; Brace that opens a top-level function definition. Function Symbols.
	;;;(defun-open . x)

	;; Brace that closes a top-level function definition. Function Symbols.
	;;;(defun-close . x)

        ;; The first line in a top-level defun. Function Symbols.
        ;;;(defun-block-intro . x)

        ;; Brace that opens a class definition. Class Symbols.
        ;;;(class-open . x)

        ;; Brace that closes a class definition. Class Symbols.
        ;;;(class-close . x)

        ;; Brace that opens an in-class inline method. Class Symbols.
        (inline-open . 0)

        ;; Brace that closes an in-class inline method. Class Symbols.
        ;;;(inline-close . x)

        ;; The region between a function definition's argument list and the
        ;; function opening brace (excluding K&R argument declarations). In C,
        ;; you cannot put anything but whitespace and comments in this region,
        ;; however in C++ and Java, throws declarations and other things can
        ;; appear here. Literal Symbols.
        (func-decl-cont . 0)

        ;; First line of a K&R C argument declaration. K&R Symbols.
        ;;;(knr-argdecl-intro . x)

        ;; Subsequent lines in a K&R C argument declaration. K&R Symbols.
        ;;;(knr-argdecl . x)

        ;; The first line in a topmost definition. Function Symbols.
        ;;;(topmost-intro . x)

        ;; Topmost definition continuation lines. This is only used in the
        ;; parts that aren't covered by other symbols such as func-decl-cont
        ;; and knr-argdecl. Function Symbols.
        ;;;(member-init-intro . x)

        ;; First line in a member initialization list. Class Symbols.
        ;;;(topmost-intro-cont . x) . x)

        ;; Subsequent member initialization list lines. Class Symbols.
        ;;;(member-init-cont . x)

        ;; First line of a multiple inheritance list. Class Symbols.
        ;;;(inher-intro . x)

        ;; Subsequent multiple inheritance lines. Class Symbols.
        ;;;(inher-cont . x)

        ;; Statement block open brace. Literal Symbols.
        ;;;(block-open . x)

        ;; Statement block close brace. Conditional Construct Symbols.
        ;;;(block-close . x)

        ;; Open brace of an enum or static array list. Brace List Symbols.
        ;;;(brace-list-open . x)

        ;; Close brace of an enum or static array list. Brace List Symbols.
        ;;;(brace-list-close . x)

        ;; First line in an enum or static array list. Brace List Symbols.
        ;;;(brace-list-intro . x)

        ;; Subsequent lines in an enum or static array list. Brace List
        ;; Symbols.
        ;;;(brace-list-entry . x)

        ;; Subsequent lines in an enum or static array list where the line
        ;; begins with an open brace. Brace List Symbols.
        ;;;(brace-entry-open . x)

        ;; A statement. Function Symbols.
        ;;;(statement . x) A continuation of a statement. Function Symbols.
        (statement-cont . 0)

        ;; The first line in a new statement block. Conditional Construct
        ;; Symbols.
        ;;;(statement-block-intro . x)

        ;; The first line in a case block. Switch Statement Symbols.
        ;;;(statement-case-intro . x)

        ;; The first line in a case block that starts with a brace. Switch
        ;; Statement Symbols.
        ;;;(statement-case-open . x)

        ;; The first line after a conditional or loop construct. Conditional
        ;; Construct Symbols.
        ;;;(substatement . x)

        ;; The brace that opens a substatement block. Conditional Construct
        ;; Symbols.
        (substatement-open . 0)

        ;; The first line after a conditional or loop construct if it's a
        ;; label. Conditional Construct Symbols.
        ;;;(substatement-label . x)

        ;; A label in a switch block. Switch Statement Symbols.
        (case-label . +)

        ;; C++ access control label. Class Symbols.
        ;;;(access-label . x)

        ;; Any other label. Literal Symbols.
        ;;;(label . x)

        ;; The while line that ends a do-while construct. Conditional Construct
        ;; Symbols.
        ;;;(do-while-closure . x)

        ;; The else line of an if-else construct. Conditional Construct
        ;; Symbols.
        ;;;(else-clause . x)

        ;; The catch or finally (in Java) line of a try-catch
        ;; construct. Conditional Construct Symbols.
        ;;;(catch-clause . x)

        ;; A line containing only a comment introduction. Literal Symbols.
        ;;;(comment-intro . x)

        ;; The first line in an argument list. Paren List Symbols.
        ;;;(arglist-intro . x)

        ;; Subsequent argument list lines when no arguments follow on the same
        ;; line as the arglist opening paren. Paren List Symbols.
        ;;;(arglist-cont . x)

        ;; Subsequent argument list lines when at least one argument follows on
        ;; the same line as the arglist opening paren. Paren List Symbols.
        (arglist-cont-nonempty . +)

        ;; The solo close paren of an argument list. Paren List Symbols.
        (arglist-close . 0)

        ;; Lines continuing a stream operator (C++ only). Literal Symbols.
        (stream-op . +)

        ;; The line is nested inside a class definition. Class Symbols.
        ;;;(inclass . x)

        ;; The start of a preprocessor macro definition. Literal Symbols.
        (cpp-macro . c-lineup-dont-change)

        ;; The first line inside a multiline preproprocessor macro if
        ;; c-syntactic-indentation-in-macros is set. Multiline Macro Symbols.
        ;;;(cpp-define-intro . x)

        ;; All lines inside multiline preprocessor macros if
        ;; c-syntactic-indentation-in-macros is nil. Multiline Macro Symbols.
        ;;;(cpp-macro-cont . x)

        ;; A C++ friend declaration. Class Symbols.
        ;;;(friend . x)

        ;; Lines continuing an Objective-C method call. Objective-C Method
        ;; Symbols.
        ;;;(objc-method-call-cont . x)

        ;; Brace that opens an extern block (e.g. extern "C" {...}). External
        ;; Scope Symbols.
        ;;;(extern-lang-open . x)

        ;; Brace that closes an extern block. External Scope Symbols.
        ;;;(extern-lang-close . x)

        ;; Analogous to inclass syntactic symbol, but used inside extern
        ;; blocks. External Scope Symbols.
        ;;;(inextern-lang . x)

        ;; These are analogous to the three extern-lang symbols above, but are
        ;; returned for C++ namespace blocks. External Scope Symbols.
        ;;;(namespace-close . x)
        ;;;(namespace-open . x)
        (innamespace . +)

        ;; C++ template argument list continuations. Class Symbols.
        (template-args-cont . +)

        ;; A statement block inside an expression. The gcc C and C++ extension
        ;; for this is recognized. It's also used for the special functions
        ;; that take a statement block as an argument in Pike. Statement Block
        ;; Symbols.
        ;;;(inexpr-statement . x)

    ))

    ;; Comment Line-Up
    ;;;(c-comment-only-line-offset . x)

    ;; Other Indentation
    ;;; (c-special-indent-hook . x)
    ;;; (c-label-minimum-indentation . x)

    ;; Custom Macros
    ;;; (c-backslash-column . x)
    ;;; (c-backslash-max-column . x)

  ))

(defun igdev-c-mode-common-hook ()
  ;;;(y-or-n-p "In my C++ hook")		;Used for debugging

  ;; set igdev style for the current buffer
  (c-add-style "IGDEV-C++" igdev-c++-style t)

  ;; other customizations
  (setq fill-column 78)
  (auto-fill-mode t)
  (global-font-lock-mode t)		;Colored code
  (setq indent-tabs-mode nil)		;Use spaces instead of tabs
  (setq c-backslash-column 78)		;Minimum alignment column for line
                                        ;continuation backslashes
)
(add-hook 'c-mode-common-hook 'igdev-c-mode-common-hook)

(defun igdev-auto-hungy-c-mode-common-hook ()
  (c-toggle-auto-state t)		;Toggle auto-newline minor mode
                            ;Changed to c-toggle-auto-newline in later CC modes
  (c-toggle-hungry-state t)		;Toggle hungry-delete minor mode
)
;; If you want to enable auto-newline and hungry-delete, copy the following
;; line to your .emacs file (after the load-file call that pulls this file
;; in!), and uncomment:
; (add-hook 'c-mode-common-hook 'igdev-auto-hungry-c-mode-common-hook)

;;;We want *.h header files to use c++ mode as well.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
