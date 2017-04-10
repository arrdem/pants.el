;; company-mode support for pants-mode
;;
;; Leverages the new keyword list(s) in pants-mode to provide trivial
;; autocompletion via company for pants BUILD files.

(require 'company)
(require 'pants-mode)

(defun company-pants-mode-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-pants-mode-backend))
    (prefix (and (eq major-mode 'pants-mode)
                 (company-grab-symbol)))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (concatenate 'list
                   pants-mode-targets-list
                   pants-mode-symbols-list
                   pants-mode-keywords-list)))))

;;;###autoload
(add-to-list 'company-backends 'company-pants-mode-backend)

(provide 'company-pants)
