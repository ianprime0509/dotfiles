;;; This is my system configuration for GuixSD.
(use-modules (gnu)
	     (gnu system nss))
(use-service-modules desktop xorg)
(use-package-modules bootloaders certs suckless wm)

(define custom-x11-keyboard
  "Section \"InputClass\"
       Identifier \"evdev keyboard catchall\"
       Driver \"evdev\"
       MatchIsKeyboard \"on\"
       Option \"XkbLayout\" \"us\"
       Option \"XkbOptions\" \"caps:escape,compose:menu\"
   EndSection")

(define custom-xorg-start-command
  (xorg-start-command
   #:configuration-file
   (xorg-configuration-file
    #:extra-config
    (list custom-x11-keyboard))))

(operating-system
 (host-name "ian")
 (timezone "America/New_York")
 (locale "en_US.utf8")

 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (target "/boot")))

 ;; Silence PC speaker (BEEP).
 (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))

 (file-systems (cons* (file-system
		       (device (file-system-label "my-root"))
		       (mount-point "/")
		       (type "ext4"))
		      (file-system
		       (device (uuid "56D9-9005" 'fat))
		       (mount-point "/boot")
		       (type "vfat"))
		      %base-file-systems))

 (users (cons (user-account
	       (name "ian")
	       (group "users")
	       (supplementary-groups '("wheel" "netdev"
				       "audio" "video"))
	       (home-directory "/home/ian"))
	      %base-user-accounts))

 (packages (cons* nss-certs i3-wm i3status dmenu
		  %base-packages))

 (services (cons*
	    (extra-special-file "/usr/bin/env"
				(file-append coreutils "/bin/env"))
	    (modify-services %desktop-services
			     (slim-service-type config =>
						(slim-configuration
						 (inherit config)
						 (startx custom-xorg-start-command))))))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
