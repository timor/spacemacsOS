;; Can be used to bind a key to jumping to an application, or alternatively starting it.  E.g.:
;;
;; (spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
;;
;; The window class can be found out with exwm's builtin info functions, but for most applications it should just match the buffer name.
(defun spacemacs/exwm-bind-switch-to-or-run-command (key window-class command)
  (exwm-input-set-key (kbd key)
                      `(lambda ()
                         (interactive)
                         (spacemacs/exwm-switch-to-buffer-or-run ,window-class ,command))))

(defun spacemacs//exwm-switch-to-line-mode ()
  "Used as a hook to switch to line mode when transient mode starts."
  (when (eq exwm--input-mode 'char-mode)
    ;; (setq exwm--switch-to-char-after-transient (current-buffer))
    (call-interactively 'exwm-input--grab-keyboard)))

(defun spacemacs//exwm-persp-mode-inhibit-p (frame)
  (frame-parameter frame 'unsplittable))

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun spacemacs/exwm-switch-to-buffer-or-run (window-class command)
  "Switch to first buffer with window-class, and if not present, run command."
  (let ((buffer
         (find window-class (buffer-list) :key (lambda(b) (cdr (assoc 'exwm-class-name (buffer-local-variables b)))) :test 'string-equal)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(defun spacemacs/exwm-rename-buffer ()
  (let* ((part1 exwm-class-name)
         (part2 (when (not (string-equal exwm-class-name exwm-title))
                  (concat "/" exwm-title)))
         (name (concat part1 (or part2 "")))
         (maxlen 40))
    (exwm-workspace-rename-buffer (if (> (length name) maxlen)
                                      (concat (subseq name 0 (- maxlen 3)) "...")
                                    name))))

(defun spacemacs/exwm-workspace-next ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index
                       (1- exwm-workspace-number))))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch 0)))
     (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

(defun spacemacs/exwm-workspace-prev ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index 0)))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch (1- exwm-workspace-number))))
     (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

(defun spacemacs/exwm-layout-toggle-fullscreen ()
  "Togggles full screen for Emacs and X windows"
  (interactive)
  (if exwm--id
      (if exwm--fullscreen
          (exwm-reset)
        (exwm-layout-set-fullscreen))
    (spacemacs/toggle-maximize-buffer)))

(defun spacemacs/exwm-run-program-in-home (command)
  (let ((default-directory user-home-directory))
    (start-process-shell-command command nil command)))

(defun spacemacs/exwm-app-launcher (command)
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ivy"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-below (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-below-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-right (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-right-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(defun spacemacs/exwm-exwm-buffers-info ()
  "Helper, return information about open exwm windows"
  (loop for buffer in (buffer-list)
        for name = (buffer-name buffer)
        for ecname = (buffer-local-value 'exwm-class-name buffer)
        when ecname
        collect (list :buffer-name name :exwm-class-name ecname)))
