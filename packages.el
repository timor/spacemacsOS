;;; packages.el --- exwm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq exwm-packages
      '(
        ;; (xelb :location (recipe :fetcher github
        ;;                         :repo "ch11ng/xelb")
        ;;       :step pre)
        ;; (exwm :location (recipe :fetcher github
        ;;                         :repo "ch11ng/exwm")
        ;;       :step pre)
        (xelb :location elpa)
        (exwm :location elpa)
        ))

(defun exwm/init-xelb ()
  (use-package xelb))

(defun exwm/init-exwm ()
  (use-package exwm
    :init
    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)
    ;; You may want Emacs to show you the time
    (display-time-mode t)
    (when exwm--hide-tiling-modeline
      (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
    (setq exwm-input-line-mode-passthrough t)

    ;; EXWM is quite particular about handling frames, so we override the default
    ;; ~SPC F~ Frame leader mappings with EXWM workspace-specific stuff.
    (define-key spacemacs-default-map (kbd "F") nil)
    (define-key spacemacs-cmds (kbd "F") nil)

    ;; Keybindings for ~s-SPC F~ Frame handling Menu
    (spacemacs/declare-prefix "F" "EXWM")
    (spacemacs/declare-prefix "Fw" "workspace")
    (spacemacs/declare-prefix "Fm" "minibuffer")
    (spacemacs/set-leader-keys
      "Fr" 'exwm-reset
      "Fh" 'exwm-floating-hide
      "Fww" 'exwm-workspace-switch
      "Fwa" 'exwm-workspace-add
      "Fwd" 'exwm-workspace-delete
      "Fwm" 'exwm-workspace-move
      "Fws" 'exwm-workspace-swap
      "Fmd" 'exwm-workspace-detach-minibuffer
      "Fma" 'exwm-workspace-attach-minibuffer
      )

    ;; introduce leader for running programs
    (spacemacs/declare-prefix "&" "exwm-run")
    (spacemacs/set-leader-keys "&s" 'exwm/launch-split-below)
    (spacemacs/set-leader-keys "&v" 'exwm/launch-split-right)

    ;; Keybindings for ~SPC m~
    (spacemacs/set-leader-keys-for-major-mode 'exwm-mode
      "f" 'exwm-floating-toggle-floating
      "m" 'exwm-workspace-move-window
      "F" 'exwm-layout-toggle-fullscreen
      )
    :config

    ;; make sure that displaying transient states gets the keyboard input.
    ;; Borrowed from: https://github.com/abo-abo/hydra/issues/232
    (define-advice hydra-set-transient-map (:around (fun keymap on-exit &optional foreign-keys) exwm-passthrough)
      (setq exwm-input-line-mode-passthrough t)
      (let ((on-exit (lexical-let ((on-exit on-exit))
                       (lambda ()
                         (setq exwm-input-line-mode-passthrough nil)
                         (when on-exit (funcall on-exit))))))
        (funcall fun keymap on-exit foreign-keys)))

    ;; override persp-mode's idea of frame creation for floating frames.  These
    ;; are characterized by the 'unsplittable' frame parameter, and should not
    ;; be tried to assign an existing layout to.

    (eval-after-load 'persp-mode
      (advice-add 'persp-init-new-frame :before-until 'exwm//persp-mode-inhibit-p))

    (exwm-input-set-key (kbd "<s-return>")
                        (lambda ()
                          (interactive)
                          (start-process-shell-command exwm-terminal-command nil exwm-terminal-command)))

    (add-hook 'exwm-update-class-hook 'exwm/rename-buffer)
    (add-hook 'exwm-update-title-hook 'exwm/rename-buffer)

    ;; kick all exwm buffers into insert mode per default
    (add-hook 'exwm-manage-finish-hook 'exwm/enter-insert-state)

    (defvar exwm-workspace-switch-wrap t
      "Whether `exwm/workspace-next' and `exwm/workspace-prev' should wrap.")

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

    ;; `exwm-input-set-key' sets global key bindings, independent of char mode, line mode, and line mode passthru

    ;; + We always need a way to get to normal state if we are in insert state.
    (exwm-input-set-key (kbd "s-<escape>") 'exwm/escape)

    (exwm-input-set-key (kbd "<s-tab>") #'exwm/jump-to-last-exwm)
    ;; + Set shortcuts to switch to a certain workspace.
    (exwm-input-set-key (kbd "s-1")
                        (lambda () (interactive) (exwm-workspace-switch 0)))
    (exwm-input-set-key (kbd "s-2")
                        (lambda () (interactive) (exwm-workspace-switch 1)))
    (exwm-input-set-key (kbd "s-3")
                        (lambda () (interactive) (exwm-workspace-switch 2)))
    (exwm-input-set-key (kbd "s-4")
                        (lambda () (interactive) (exwm-workspace-switch 3)))
    (exwm-input-set-key (kbd "s-5")
                        (lambda () (interactive) (exwm-workspace-switch 4)))
    (exwm-input-set-key (kbd "s-6")
                        (lambda () (interactive) (exwm-workspace-switch 5)))
    (exwm-input-set-key (kbd "s-7")
                        (lambda () (interactive) (exwm-workspace-switch 6)))
    (exwm-input-set-key (kbd "s-8")
                        (lambda () (interactive) (exwm-workspace-switch 7)))
    (exwm-input-set-key (kbd "s-9")
                        (lambda () (interactive) (exwm-workspace-switch 8)))
    (exwm-input-set-key (kbd "s-0")
                        (lambda () (interactive) (exwm-workspace-switch 9)))
    ;; + Application launcher ('M-&' also works if the output buffer does not
    ;;   bother you). Note that there is no need for processes to be created by
    ;;   Emacs.
    (exwm-input-set-key (kbd "s-r") #'exwm/app-launcher)
    ;; + 'slock' is a simple X display locker provided by suckless tools. 'i3lock'
    ;;   is a more feature-rich alternative.
    (exwm-input-set-key (kbd "<s-pause>")
                        (lambda () (interactive) (start-process-shell-command "lock" nil exwm--locking-command)))

    ;; ensure that when char mode is left, state is restored to normal
    ;; (advice-add 'exwm-input-grab-keyboard :after (lambda (&optional id)
    ;;                                                (evil-normal-state)))
    ;; ensure that when char mode is entered, input state is activated
    ;; (advice-add 'exwm-input-release-keyboard :after (lambda(&optional id)
    ;;                                                   (evil-insert-state)))

    ;; TODO: optionally inhibit switching to char mode or line mode, used during transient state

    ;; in normal state/line mode, use the familiar i key to switch to input state
    ;; (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
    (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm/enter-insert-state)
    (dolist (k '("<down-mouse-1>" "<down-mouse-2>" "<down-mouse-3>"))
      (evil-define-key 'normal exwm-mode-map (kbd k) 'exwm/enter-insert-state))

    ;; Define super-space as default leader key.
    (exwm-input-set-key (kbd "s-SPC") spacemacs-default-map)
    (with-eval-after-load 'which-key
     ;; Hack which-key to translate our prefix into the original leader key prefix
      (define-advice which-key--get-bindings (:filter-args (oldargs) exwm-translate-leader-key)
        (exwm//which-key-transform-filter oldargs)))

    ;; EXWM does not bypass exwm-mode-map keybindings in line-mode, so the
    ;; default bindings are still mapped to C-c.  We remap that to C-s-c.

    (define-key exwm-mode-map (kbd "C-s-c") (lookup-key exwm-mode-map (kbd "C-c")))
    (define-key exwm-mode-map (kbd "C-c") nil)

    ;; User s-q to close buffers
    (exwm-input-set-key (kbd "s-q") 'spacemacs/kill-this-buffer)

    ;; Don't override any keybindings in line-mode
    (setq exwm-input-prefix-keys '())

    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "s-U") #'winner-redo)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'ivy-switch-buffer)
    ;; Focusing windows
    (exwm-input-set-key (kbd "s-h") #'evil-window-left)
    (exwm-input-set-key (kbd "s-j") #'evil-window-down)
    (exwm-input-set-key (kbd "s-k") #'evil-window-up)
    (exwm-input-set-key (kbd "s-l") #'evil-window-right)
    ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    (exwm-input-set-key (kbd "M-s-j") #'spacemacs/enlarge-window)
    (exwm-input-set-key (kbd "M-s-k") #'spacemacs/shrink-window)
    (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)
    (exwm-input-set-key (kbd "s-m") #'spacemacs/toggle-maximize-buffer)
    ;; Workspaces
    (exwm-input-set-key (kbd "s-]") #'exwm/workspace-next)
    (exwm-input-set-key (kbd "s-[") #'exwm/workspace-prev)
    ;; Debugging
    (exwm-input-set-key (kbd "s-d") #'exwm/toggle-debug-mode)

    (require 'exwm-randr)
    (setq exwm-randr-workspace-monitor-plist '(0 "VGA1"))
    (exwm-randr-enable)
    (when (and exwm--install-logind-lock-handler
               exwm--locking-command)
      (add-hook 'exwm-init-hook exwm//install-logind-lock-handler))
    ;; The following example demonstrates how to use simulation keys to mimic the
    ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
    ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
    ;; sequence (of type vector or string), while DEST can also be a single key.

    ;; (exwm-input-set-simulation-keys
    ;;  '(([?\C-b] . left)
    ;;    ([?\C-f] . right)
    ;;    ([?\C-p] . up)
    ;;    ([?\C-n] . down)
    ;;    ([?\M-v] . prior)
    ;;    ))

    ;; Do not forget to enable EXWM. It will start by itself when things are ready.
    ;; (exwm-enable)
    ))
