(in-package :stumpwm-user)

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

;;; Wallpaper

(defparameter *my-wallpaper-path* "/home/ian/Pictures/mio.png")
(run-shell-command (format nil "feh --no-fehbg --bg-fill '~a'"
                           *my-wallpaper-path*))

;;; Colors

(load (merge-pathnames #p"base16-colors.lisp" *data-dir*))

(set-bg-color *color-base00*)
(set-fg-color *color-base05*)
(set-border-color *color-base03*)
(set-focus-color *color-base05*)
(set-unfocus-color *color-base00*)

(setf *mode-line-background-color* *color-base01*
      *mode-line-foreground-color* *color-base04*
      *mode-line-border-color* *color-base03*)

(setf *colors* (list *color-base00*
                     *color-base08*
                     *color-base0B*
                     *color-base0A*
                     *color-base0D*
                     *color-base0E*
                     *color-base0C*
                     *color-base05*))
(update-color-map (current-screen))

;;; Mode line

(setf *screen-mode-line-format* "[^B%n^b] %d | %t | %W")
(setf *time-modeline-string* "%a %b %e %k:%M")
(setf *mode-line-timeout* 5)
(enable-mode-line (current-screen) (current-head) t)
(add-screen-mode-line-formatter #\t
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  (format nil "~a ~d%"
                                          (get-battery-status "BAT0")
                                          (round (get-battery-charge "BAT0")))))

;;; Commands

(defcommand emacs () ()
  "Start Emacs unless it is already running in the current group, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs") nil))

(defcommand raise-volume (amount)
    ((:number "Percent to increase volume: "))
  (run-shell-command (format nil "pamixer -i ~d" amount) t)
  (message "Volume: ~d%" (get-volume)))

(defcommand lower-volume (amount)
    ((:number "Percent to lower volume: "))
  (run-shell-command (format nil "pamixer -d ~d" amount) t)
  (message "Volume: ~d%" (get-volume)))

;;; Keys

(run-shell-command "setxkbmap -option caps:ctrl_modifier")
(run-shell-command "ibus-daemon -drx")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "raise-volume 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "lower-volume 5")

;;; Helper functions

(defconstant +battery-directory+ "/sys/class/power_supply/")

(defun get-battery-charge (battery)
  "Return the remaining charge of BATTERY as a number between 0 and 100 (inclusive)."
  (let* ((charge-path (concatenate 'string
                                   +battery-directory+ battery "/charge_now"))
         (charge-full-path (concatenate 'string
                                        +battery-directory+ battery
                                        "/charge_full"))
         (charge (with-open-file (input charge-path)
                   (parse-integer (read-line input))))
         (charge-full (with-open-file (input charge-full-path)
                        (parse-integer (read-line input)))))
    (* 100 (/ charge charge-full))))

(defun get-battery-status (battery)
  "Return the current status (e.g. discharging) of BATTERY."
  (let ((status-path (concatenate 'string
                                  +battery-directory+ battery "/status")))
    (with-open-file (input status-path)
      (read-line input))))

(defun get-volume ()
  "Return the current audio volume as a number between 0 and 100 (inclusive)."
  (parse-integer (run-shell-command "pamixer --get-volume" t)))

(defun strip-string (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim #(#\Space #\Newline #\Tab) string))
