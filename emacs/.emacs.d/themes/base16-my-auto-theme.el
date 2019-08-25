;;
;; Custom theme
;;

(require 'base16-theme)

(defvar base16-my-auto-colors
  '(:base00 "#160501"
    :base01 "#3d1009"
    :base02 "#57130e"
    :base03 "#a03521"
    :base04 "#b82714"
    :base05 "#da451b"
    :base06 "#6e1910"
    :base07 "#930805"
    :base08 "#df5d3b"
    :base09 "#ff6314"
    :base0A "#fead4e"
    :base0B "#f64e29"
    :base0C "#ffce5f"
    :base0D "#fd120f"
    :base0E "#ff8d3c"
    :base0F "#ffce5f")
  "All colors for Base16 Macoy are defined here.")

;; Define the theme
(deftheme base16-my-auto)

;; Add all the faces to the theme
(base16-theme-define 'base16-my-auto base16-my-auto-colors)

;; Mark the theme as provided
(provide-theme 'base16-my-auto)

(provide 'base16-my-auto-theme)

;;; base16-my-auto-theme.el ends here
