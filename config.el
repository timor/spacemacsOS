(defvar exwm-locking-command "lock"
  "Command to run when locking session")

(defvar exwm-install-logind-lock-handler nil
  "If this is non-nil and `exwm-locking-command' is set, register a D-BUS handler on the session lock signal.")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm-hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-buffer-name-prefix "X:"
  "A prefix to append to each buffer managed by exwm")

(defvar exwm-enable-systray nil
  "Whether to enable EXWM's bundled system tray implementation.")

(defvar exwm-autostart-xdg-applications nil
  "Whether to run $XDG_USER_HOME/autostart applications after initialization.")

(defvar exwm-autostart-environment '()
  "List of \"KEY=value\" strings which should be set when running autostart applications.

Example: '(\"DESKTOP_SESSION=kde\" \"KDE_SESSION_VERSION=5\") ")

(defvar exwm-custom-init nil
  "This can be set to a function that runs after all other EXWM initialization.")

(defvar exwm-workspace-switch-wrap t
  "Whether `exwm/workspace-next' and `exwm/workspace-prev' should wrap.")

(defvar exwm-randr-dwim t
  "Whether to try to dwim workspace/screen association in the screen change hook.")

(defvar exwm-move-frame-at-edge t
  "If enabled, use framemove to switch frames when trying to move
  outside of current frame." )
