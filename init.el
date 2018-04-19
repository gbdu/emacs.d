(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; Manual packages - essential stuff that gets loaded first:
(add-to-list 'load-path "~/.emacs.d/manual-packages")

;; Your settings and configurations
(require 'org)
(org-babel-load-file "/home/garg/.emacs.d/settings.org")

;; Custom used by emacs and elpa
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(column-number-mode t)
 '(fci-rule-color "#383838")
 '(frame-background-mode (quote dark))
 '(global-linum-mode t)
 '(linum-format "%4d  ")
 '(minibuffer-prompt-properties
   (quote
    (read-only t cursor-intangible t face minibuffer-prompt)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-log-into-drawer t)
 '(package-selected-packages
   (quote
    (org-ac headlong ace-window org-bullets ac-helm autopair babel hydra which-key ox-reveal auto-yasnippet org-brain pc-bufsw buffer-stack helm-w3m sublime-themes go-mode gnugo go doremi-frm evil-magit vdiff-magit magit helm-dired-recent-dirs helm-dired-history ripgrep company-c-headers company projectile-speedbar sr-speedbar function-args helm-gtags ggtags doremi-cmd doremi projector projectile helm-projectile svg-clock winnow ag dumb-jump expand-region virtualenv folding flymake-lua xclip elpy fzf fasd bookmark+ helm-bm pdf-tools auto-complete-clang auto-complete-c-headers auto-complete helm switch-window dracula-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(show-paren-mode t)
 '(sr-speedbar-max-width 0 t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-echo-common ((t (:foreground "firebrick1"))))
 '(linum ((t (:background "#151515" :foreground "#9F9f9f" :underline nil))))
 '(mode-line ((t (:foreground "#030303" :background "#adadad" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#353535" :box nil))))
 '(powerline-grey40-white ((t (:background "black" :foreground "white" :box nil))) t)
 '(show-paren-match ((t (:background "#141419")))))

