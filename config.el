(defvar exwm-terminal-command "xterm"
  "Terminal command to run.")

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

(defvar exwm-custom-init nil
  "This can be set to a function that runs after all other EXWM initialization.")

(defvar exwm-workspace-switch-wrap t
  "Whether `exwm/workspace-next' and `exwm/workspace-prev' should wrap.")

(defvar exwm-use-autorandr t
  "Whether to call autorandr in the screen change hook. Does
  nothing if the autorandr binary cannot be found in PATH. Note
  that changes to this variable probably require restarting EXWM.")
