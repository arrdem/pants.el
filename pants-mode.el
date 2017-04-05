;; pants-mode.el
;;
;; This goes together with pants.el, providing python-mode derived
;; syntax highlighting for pants BUILD files.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . pants-mode))

(define-derived-mode pants-mode python-mode "BUILD")

(font-lock-add-keywords 'pants-mode
  '(("\\(name\\|sources?\\|dependencies\\)" . font-lock-keyword-face)
    ("\\(\\(python\\|java\\|scala\\)_\\(library\\|binary\\)\\|globs\\)" . font-lock-function-name-face)))
