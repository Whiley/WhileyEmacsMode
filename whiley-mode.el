;;; whiley-mode.el --- Major mode for Whiley language -*- lexical-binding: t; -*-

;; Copyright (C) 2012-- David J. Pearce
;;
;; Author: David J. Pearce <dave01001110@gmail.com>
;; URL: http://github.com/Whiley/WhileyEmacsMode
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This package provides a major mode for working with programs
;; written in the Whiley programming language.

(require 'cc-mode)

;;; Code:

(defun whiley-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'.  Argument ARG is optional prefiex
for `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "//") (comment-end ""))
     (comment-dwim arg)))

(defvar whiley-keywords
  '("native" "unsafe" "export" "extern" "final" "null" "return" "if" "is" "throw" "throws" "try" "catch" "switch" "case" "default" "break" "continue" "skip" "do" "while" "for" "else" "define" "assume" "assert" "assume" "package" "import" "function" "method" "type" "constant" "from" "with" "debug" "where" "ensures" "requires" "old" "public" "protected" "private" "this" "str" "new" "in" "no" "some" "all" "false" "true" "property" "template" "variant")
    "Whiley keywords.")

(defvar whiley-types
  '("real" "int" "bool" "void" "void" "ref")
  "Whiley types.")

(defvar whiley-keywords-regexp (regexp-opt whiley-keywords 'words))
(defvar whiley-type-regexp (regexp-opt whiley-types 'words))
(defvar whiley-font-lock-keywords
  `((,whiley-type-regexp . font-lock-type-face)(,whiley-keywords-regexp . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode whiley-mode fundamental-mode
  "whiley mode"
  "Major mode for editing Whiley ..."
  (setq font-lock-defaults '((whiley-font-lock-keywords)))
  (setq whiley-keywords-regexp nil)
  (setq whiley-type-regexp nil)

  ;; borrow adaptive fill for comments from cc-mode
  (substitute-key-definition 'fill-paragraph 'c-fill-paragraph
			     c-mode-base-map global-map)

  ;; java-style comments "// ..." and “/* … */”
  (define-key whiley-mode-map [remap comment-dwim] 'whiley-comment-dwim)
  (modify-syntax-entry ?\/ ". 124b" whiley-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" whiley-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" whiley-mode-syntax-table)

  ;; indentation.  Needs work!
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq tab-stop-list (list 4 8 12 16 20 24 28))

  ;; unicode characters
  (local-set-key "\M-u" '(lambda () (interactive) (ucs-insert #x222A)))
  (local-set-key "\M-n" '(lambda () (interactive) (ucs-insert #x2229)))
  (local-set-key "\M-e" '(lambda () (interactive) (ucs-insert #x2208))))

;;;###autoload
(setq auto-mode-alist (cons '("\\.whiley\\'" . whiley-mode) auto-mode-alist))

;;;###autoload
(provide 'whiley-mode)
;;; whiley-mode.el ends here
