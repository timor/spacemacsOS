(defvar exwm-terminal-command "xterm"
  "Terminal command to run.")

(defvar exwm--locking-command "lock"
  "Command to run when locking session")

(defvar exwm--install-logind-lock-handler nil
  "If this is non-nil and `exwm--locking-command' is set, register a D-BUS handler on the session lock signal.")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm--hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-buffer-name-prefix "X:"
  "A prefix to append to each buffer managed by exwm")
