;;; packages.el --- Factor packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.


(defconst factor-packages
  '(
    ;; Assume that factor is installed, and emacs lisp files are correctly
    ;; located in site-lisp
    (factor-mode :location site)
    ))

(defun factor/init-factor-mode()
  (use-package factor-mode
    :commands factor-mode
    :config
    (progn
      (require 'fuel-mode)
      (spacemacs/register-repl 'fuel-mode 'run-factor)
      (mapc (lambda (x)
              (spacemacs/declare-prefix-for-mode 'factor-mode (car x) (cdr x)))
            '(("mh" . "help")
              ("me" . "eval")
              ("mc" . "compile")
              ("mg" . "nav")
              ("ms" . "repl")))
      (spacemacs/set-leader-keys-for-major-mode 'factor-mode
        "'" 'run-factor

        "cc" 'fuel-run-file

        "ef" 'fuel-eval-definition
        "er" 'fuel-eval-region
        "eR" 'fuel-eval-extended-region

        "gg" 'fuel-edit-word
        "gG" 'fuel-edit-word-at-point
        "ga" 'factor-visit-other-file

        "ta" 'fuel-test-vocab

        "rs" 'fuel-refactor-extract-sexp
        "rr" 'fuel-refactor-extract-region
        "rv" 'fuel-refactor-extract-vocab
        "ri" 'fuel-refactor-inline-word
        "rw" 'fuel-refactor-rename-word
        "ra" 'fuel-refactor-extract-article
        "rg" 'fuel-refactor-make-generic
        )))

        "ss" 'run-factor

        "hh" 'fuel-help
        "he" 'factor//fuel-stack-effect
        "hp" 'fuel-apropos
        "hv" 'fuel-show-file-words
        "h<" 'fuel-show-callers
        "h>" 'fuel-show-callees
  )
