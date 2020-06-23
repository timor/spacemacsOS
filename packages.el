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

        ;; desktop-environment
        ;; TODO: remove :commit binding once upstream has new release
        (desktop-environment :location (recipe :fetcher github :repo "DamienCassou/desktop-environment" :upgrade t :commit "cd5145288944f4bbd7b2459e4b55a6a95e37f06d"))
        (framemove :location (recipe :fetcher github :repo "emacsmirror/framemove"))
        ))

(defun exwm/init-framemove ()
  (use-package framemove
    :after exwm
    :config
    (progn
      ;; Emacs frame parameters don't seem to be too reliable...
      (define-advice fm-frame-bbox (:around (oldfun frame) exwm-frame-bbox-from-randr)
        (if (frame-parameter frame 'exwm-geometry)
            (exwm//fm-frame-bbox-from-randr frame)
          (funcall oldfun frame)))
      (setq framemove-hook-into-windmove exwm-move-frame-at-edge))))

(defun exwm/init-desktop-environment ()
  (use-package desktop-environment
    :after exwm
    :diminish desktop-environment-mode
    :defer t
    :init
    (progn
      (spacemacs|add-toggle desktop-environment
        :mode desktop-environment-mode
        :documentation "Keybindings for Desktop Environment functionality."
        :evil-leader "TD")
      )
    :config
    (progn
      ;; We bypass desktop-environment's locking functionality for 2 reasons:
      ;; 1. s-l is most likely needed for window manipulation
      ;; 2. desktop-environment's locking mechanism does not support registering as session manager
      ;; TODO: To be completely consistent, we should put our own locking stuff also under this toggle
      ;; The following line would instead assign their locking command to the default binding:
      ;; (define-key desktop-environment-mode-map (kbd "<s-pause>") (lookup-key desktop-environment-mode-map (kbd "s-l")))
      (setq desktop-environment-update-exwm-global-keys :prefix)
      (define-key desktop-environment-mode-map (kbd "s-l") nil)
      ;; If we don't enable this, exwm/switch-to-buffer-or-run won't move an X window to the current frame
      (setq exwm-layout-show-all-buffers t))))

(defun exwm/init-xelb ()
  (use-package xelb))

(defun exwm//install-frame-keybindings ()
  "This installes the bindings that override the original ~SPC F~ frame
  commands.  This is a separate function becuse it is called only after exwm
  initialization, when the window manager has successfully be started."
  ;; EXWM is quite particular about handling frames, so we override the default
  ;; ~SPC F~ Frame leader mappings with EXWM workspace-specific stuff.
  (define-key spacemacs-default-map (kbd "F") nil)
  (define-key spacemacs-cmds (kbd "F") nil)

  ;; Keybindings for ~s-SPC F~ Frame handling Menu
  (spacemacs/declare-prefix "F" "EXWM")
  (spacemacs/declare-prefix "FM" "minibuffer")
  (spacemacs/set-leader-keys
    "Fr" 'exwm-reset
    "Fh" 'exwm-floating-hide
    "Fw" 'exwm-workspace-switch
    "Fa" 'exwm-workspace-add
    "Fd" 'exwm-workspace-delete
    "Fm" 'exwm-workspace-move
    "Fs" 'exwm-workspace-swap
    "FMd" 'exwm-workspace-detach-minibuffer
    "FMa" 'exwm-workspace-attach-minibuffer
    ))

(defun exwm/init-exwm ()
  (use-package exwm
    :init
    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)
    ;; You may want Emacs to show you the time
    (display-time-mode t)
    (when exwm-hide-tiling-modeline
      (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
    (setq exwm-input-line-mode-passthrough t)


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

    ;; make winner aware of new window configuration
    (with-eval-after-load 'winner
      (add-hook 'exwm-manage-finish-hook 'winner-save-old-configurations t))
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

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

    ;; `exwm-input-set-key' sets global key bindings, independent of char mode, line mode, and line mode passthru

    ;; + We always need a way to get to normal state if we are in insert state.
    (exwm-input-set-key (kbd "s-<escape>") 'exwm/escape)

    (exwm-input-set-key (kbd "s-c") 'exwm/enter-char-mode)

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
                        (lambda () (interactive) (start-process-shell-command "lock" nil exwm-locking-command)))

    ;; in normal state/line mode, use the familiar i key to switch to input state
    ;; (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
    (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm/enter-insert-state)
    (dolist (k '("<down-mouse-1>" "<down-mouse-2>" "<down-mouse-3>"))
      (evil-define-key 'normal exwm-mode-map (kbd k) 'exwm/enter-insert-state))

    ;; Define super-space as default leader key.
    (exwm-input-set-key (kbd "s-SPC") spacemacs-default-map)
    ;; Don't have to lift finger from s-key for M-x behavior:
    (if (configuration-layer/layer-used-p 'helm)
        (spacemacs/set-leader-keys "s-SPC" 'helm-M-x)
      (spacemacs/set-leader-keys "s-SPC" 'execute-extended-command))

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
    (exwm-input-set-key (kbd "s-}") #'exwm/workspace-move-buffer-next)
    (exwm-input-set-key (kbd "s-{") #'exwm/workspace-move-buffer-prev)
    ;; Debugging
    (exwm-input-set-key (kbd "s-d") #'exwm/toggle-debug-mode)

    (require 'exwm-randr)
    (setq exwm-randr-workspace-monitor-plist '(0 "VGA1"))
    (when (and exwm-use-autorandr
               (executable-find "autorandr"))
      (add-hook 'exwm-randr-screen-change-hook 'exwm//autorandr-hook))
    (exwm-randr-enable)
    (when exwm-enable-systray
      (require 'exwm-systemtray)
      (exwm-systemtray-enable))
    (when (and exwm-install-logind-lock-handler
               exwm-locking-command)
      (add-hook 'exwm-init-hook 'exwm//install-logind-lock-handler))
    (when exwm-autostart-xdg-applications
      (add-hook 'exwm-init-hook 'exwm//autostart-xdg-applications t))
    (add-hook 'exwm-init-hook 'exwm//install-frame-keybindings t)
    (when exwm-custom-init
      (add-hook 'exwm-init-hook exwm-custom-init t))
    ))
