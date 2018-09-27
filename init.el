;;; Reload settings
(defun helper-reload-settings ()
  "Reload/re-eval ~/.emacs.d/init.el" (interactive)
  (eval '(load-file "~/.emacs.d/init.el")))

;;; Package management
;;;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;; require 
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/manual-packages")
  (require 'use-package)
  (require 'helpers1)
  (require 'recentf)
  (require 'helper-hydras)
  (require 'mouse)
  (require 'xclip)
  (require 'scpaste)
  (require 'dash)
  (require 'outshine))

;;;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)


;;; Hydras 
(use-package hydra)

;;; Defaults, visual, and editting style
;;;; Visual
;;;;; No splash screen and toolbar 
(setq inhibit-splash-screen 't)
(tool-bar-mode -1)
(menu-bar-mode 1)
;;;;; Default font
(add-to-list 'default-frame-alist '(font . "Neep-10" ))
(set-face-attribute 'default t :font "Neep-10" )

;;;;; Default theme
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(load-theme 'zenburn t)))
  (load-theme 'zenburn t))

;;;;; show-paren-mode
(show-paren-mode)
(setq blink-matching-delay 1)
(setq blink-matching-paren t)


(setq show-paren-style 'expression) ; highlight entire expression
;; (setq show-paren-style 'parenthesis) ; highlight brackets

;; blink-cursor-mode
(setq blink-cursor-delay 0.2)
(blink-cursor-mode)

;;;;; Linum
(setq linum-format "%4d  ") ;; no line
(setq nlinum-highlight-current-line t)
(global-linum-mode 1)

;;;;; Powerline and modeline
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

(use-package powerline
  :config (setq powerline-arrow-shape 'arrow14)) ;;  best for small fonts

;;;;; Window dividers
(set-face-foreground 'vertical-border "black")

;;;; Coding style
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
      c-basic-offset 4)
(setq tab-always-indent 't )
(delete-selection-mode 1) ;; delete selection when typing
;;;; auto fill mode 
(auto-fill-mode)
;;;; Enable mouse

(xterm-mouse-mode 1)
;;;; Stop ESC ESC ESC from destroying windows
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
	ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;;;; Integrate X11 xclipboard with emacs 

(setq x-select-enable-clipboard t) ;; Ctrl+c in Linux X11
(setq x-select-enable-primary t) ;;selection in X11
(xclip-mode 1)

;;;; ag and case-fold-search
(setq ag-highlight-search t)
(setq case-fold-search t) ;; case insensitive search

;;;; Windows and layout
;;;;; Winner and windmove 
(winner-mode 1) ;; remember window layouts
;; (setq windmove-wrap-around t) ;; warning: this breaks tmux windmove

;; (require 'window-purpose)
;; (purpose-mode)


;; (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
;; (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
;; (purpose-compile-user-configuration)

;;;;; use only one desktop for saving layouts
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

;;;;; Avy/ace
(setq avy-keys '( ?1 ?2 ?3 ?4 ?5 ?q ?w ?e ?r ?t ?a ?s ?d ?f ?x ?c))
(setq avy-all-windows 't)

;;;; Apropos searches more thoroughly
(setq apropos-do-all t)

;;; Org-mode
;;;; Basic options 
(setq org-cycle-separator-lines 0)
; the following setting prevents creating blank lines before headings
; but allows list items to adapt to existing blank lines around the
					; items
(setq org-blank-before-new-entry
      (quote
       ((heading)(plain-list-item . auto))))

(setq org-track-ordered-property-with-tag t)
(setq org-clock-into-drawer "CLOCKING")
(setq org-startup-indented t)
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq org-columns-default-format "%8TODO %50ITEM(Task) %2PRIORITY %10ALLTAGS(TAGS) %10CLOCKSUM")

;; prevent tasks from changing to DONE if any subtasks are still open
(setq org-enforce-todo-dependencies t) 
(setq org-clock-idle-time 15)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-pretty-entities t)
(setq org-log-done 'time)
(setq org-priority-faces '((?A . (:foreground "tomato" :weight 'bold))
                           (?B . (:foreground "coral"))
                           (?C . (:foreground "LawnGreen"))))
(setq org-hide-leading-stars t)
(setq org-adapt-indentation t)
(setq org-refile-targets '((nil :maxlevel . 10)
			   (org-agenda-files :maxlevel . 10)))
(setq org-src-fontify-natively t) ;; syntax highlighting for src blocks
(defun my-org-mode-hook ()
;;  (flyspell-mode 1)
  (org-reveal))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(require 'ox-md)

;; (setq org-todo-keywords
;;   '((sequence "TODO(t@/)" "|" "DONE(d!)" "CANCELED(c!)")))
;; (setq org-ellipsis "...")
;; (set-display-table-slot standard-display-table 'selective-display
;; ;; (string-to-vector "◦◦◦")) ; or whatever you like


;;;; Capture templates
(setq org-capture-templates (quote
    (("t" "Todo" entry
      (file+headline "~/org/general.org" "Tasks")
      (file "~/org/templates/todo")
      :empty-lines-after 1)
     ("b" "Book" entry
      (file+headline "~/org/general.org" "Books")
      (file "~/org/templates/book")
      :empty-lines-after 1)
     ("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
     ("g" "General note" entry
      (file+headline "~/org/general.org" "Capture")
      (file "~/org/templates/general")
      :empty-lines-after 1))))

;;;; Org-agenda
(setq org-agenda-files '("~/org"))
;;;;; Add files in ~/org/notes recursively since it has sub dirs
(setq org-agenda-files-notes (apply 'append
              (mapcar
               (lambda (directory)
                 (directory-files-recursively
                  directory org-agenda-file-regexp))
               '("~/org/notes" ) ) ) )

(mapc #'(lambda (item) (add-to-list 'org-agenda-files item))
      org-agenda-files-notes)

;;;;; Options 
(setq org-agenda-view-columns-initially t)
(setq org-agenda-start-on-weekday 1) ;; start agenda on monday

(setq org-agenda-skip-scheduled-if-done t)

(defun org-agenda-show-tags-in-columns (&optional arg)
  (interactive "P")
  (org-agenda arg "t"))

;;;; Org-journal
(use-package org-journal )

;;  :after (org-journal-update-auto-mode-alist))

;;; Helm
;;;; helm

(require 'helm-config)

(global-set-key (kbd "<f7>") 'helm-command-prefix)

(use-package helm
  :bind (:map helm-map
	      ("<tab>" . helm-execute-persistent-action)
	      ("C-i" . helm-execute-persistent-action)
	      ("C-z" . helm-select-action))
  :bind ("M-x" . helm-M-x)
  :config (progn
	    (setq helm-autoresize-max-height 30)
	    (setq helm-autoresize-min-height 30)
	    (setq helm-split-window-in-side-p t ; open helm buffer
					; inside current window, not
					; occupy whole other window
		  helm-move-to-line-cycle-in-source t ; move to end or
					; beginning of source when
					; reaching top or bottom of
					; source.
		  helm-ff-search-library-in-sexp t ; search for
					; library in `require' and
					; `declare-function' sexp.
		  helm-scroll-amount 4  ; scroll 8 lines other window
					; using M-<next>/M-<prior>
		  
		  helm-echo-input-in-header-line t)
	    (helm-autoresize-mode 1)
	    (helm-mode 1)))

;;;; helm-gtags
(use-package helm-gtags
  :config (setq helm-gtags-ignore-case t
		helm-gtags-auto-update t
		helm-gtags-use-input-at-cursor t
		helm-gtags-pulse-at-cursor t
		helm-gtags-prefix-key "C-+"
		helm-gtags-suggested-key-mapping t)
  :init (progn
	   (add-hook 'dired-mode-hook 'helm-gtags-mode)
	   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
	   (add-hook 'c-mode-hook 'helm-gtags-mode)
	   (add-hook 'c++-mode-hook 'helm-gtags-mode)
	   (add-hook 'asm-mode-hook 'helm-gtags-mode)))

;;;; helm-xref
(use-package helm-xref
  :after (helm)
  :config (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

;;;; helm-ag
(use-package helm-ag
  :config (progn (setq helm-follow-mode-persistent t)))

;;; Files & history
;;;; Backup/autosaves

(setq
   backup-by-copying t
   backup-directory-alist '(("~/org/" . "/dev/null")
			    ("." . "~/emacs-backups/"))
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   version-control t)      ; use versioned backups

(dir-locals-set-class-variables
 'nobackup-directory
 '((nil . ((make-backup-files . nil)))))

(dir-locals-set-directory-class "~/org/journal" 'nobackup-directory)

(setq auto-save-default t)

(auto-save-mode)


;;;; undo-tree (disabled)
;; (use-package undo-tree :config
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist '(("."
;;   . "~/.emacs.d/undo")))

;;   (global-undo-tree-mode))

;;;; savehist and save-place-mode (remember place in buffer)
(use-package savehist
  :init (progn (savehist-mode 1)
	       (save-place-mode 1))
  :config (progn
	    (add-to-list
	     'savehist-additional-variables
	     'helm-dired-history-variable)
	    (setq savehist-additional-variables
		  '(kill-ring
		    search-ring
		    regexp-search-ring))))

;;;; recentf

(setq recentf-max-saved-items 2000
      recentf-auto-cleanup 'never  
      recentf-max-menu-items 200)

(recentf-mode 1)
(run-at-time (current-time) 140 (lambda ()
				  (let ((inhibit-message t))
				    (recentf-save-list))))

;;;; fasd
(use-package fasd
  :config (progn
	    (setq fasd-enable-initial-prompt nil)
	    (global-fasd-mode 1)))


;;; Web
;;;; Default browser
(setq browse-url-browser-function 'eww-browse-url)

;;;; DON'T Use proportional fonts in eww
(setq shr-use-fonts nil)

;;;; Tramp
(setq tramp-default-method "ssh")

;;;; Use curl when possible
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;;;; SCP paste
(setq scpaste-http-destination "http://frezr.com/paste"
      scpaste-scp-destination "frezr.com:p/paste/")

;;; Completion & programming
;;;; whichkey
(which-key-mode 1)
;;;; Python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
;;;;; default virtualenv
(setenv "WORKON_HOME" "/home/garg/virtualenvs")
(pyvenv-workon "django2")

;;;;; elpy 
(setq elpy-rpc-backend "jedi")
(when (require 'elpy nil t)
  (elpy-enable))


;;;; company
(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-dabbrev-downcase 0
	  company-idle-delay 0.1
	  company-minimum-prefix-length 1
	  company-show-numbers t
	  company-dabbrev-time-limit 0.5
	  company-transformers nil)))
;;;; lsp-mode
(use-package lsp-mode
  :config
  (setq lsp-highlight-symbol-at-point nil))

;;;; lsp-ui
(use-package lsp-ui
  :init (add-hook 'lsp-after-open-hook #'lsp-ui-mode)
  :config
  (progn
    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-sideline-enable nil)
;;    (setq lsp-ui-doc-header t)
    (setq lsp-ui-doc-include-signature t)
    ;;    (setq lsp-ui-sideline-show-flycheck t)
    ))
    
;;;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'lsp)))
  (add-hook 'c-mode-hook (lambda() (flycheck-mode)))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'lsp)))
  (add-hook 'c++-mode-hook (lambda() (flycheck-mode))))



(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

;;;; company-lsp
(use-package company-lsp
  :init
  (defun my:company-lsp-enable ()
    (add-to-list 'company-backends 'company-lsp))
  (add-hook 'lsp-mode-hook #'my:company-lsp-enable)
  :config
  (setq company-lsp-enable-recompletion nil
        company-lsp-enable-snippet t
        company-lsp-cache-candidates nil
        company-lsp-async t)
  )

;;;; semantic completion

(add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")
(semantic-mode 1)
;; (global-semantic-idle-scheduler-mode 1)


;;;; cquery
(use-package cquery
  :config
  (progn
    (setq cquery-executable "/usr/bin/cquery")
    (setq cquery-extra-init-params
	  '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
    (add-hook 'c-mode-hook #'cquery//enable)
    (add-hook 'c++-mode-hook #'cquery//enable)))


;;;; yasnippet
(use-package yasnippet
  :config (yas-global-mode))

;;;; compilation
(require 'compile)

(add-hook 'c-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (format "make -k -f %s" (get-closest-pathname)))))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (format "make -k -f %s" (get-closest-pathname)))))
(add-to-list 'compilation-finish-functions 'notify-compilation-result)
(global-set-key (kbd "<f5>")
		(lambda ()
		  (interactive)
                  (setq-local compilation-read-command nil)
                  (call-interactively 'compile)))

;;;; stickyfunc
(use-package stickyfunc-enhance
  :config (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

;;;; Outshine/code folding
;; (setq outshine-org-style-global-cycling-at-bob-p t)
;; (setq outshine-startup-folded-p t)

(setq outshine-use-speed-commands t)
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-t") 'outshine-todo)
	    (local-set-key [kp-end] 'outshine-clock-in)
	    (local-set-key [kp-next] 'outshine-clock-out)
	    (local-set-key [kp-0] 'helm-navi-headings)
	    (local-set-key (kbd "<C-return>") 'outline-insert-heading)
	    (local-set-key (kbd "<C-M-return>") 'extract-comment-from-tag)))

;; Required for outshine
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;; Enables outline-minor-mode for *ALL* programming buffers
(add-hook 'conf-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

;;;; autopair mode
(use-package autopair
  :config
  (autopair-global-mode))

;;;; SREFACTOR
(require 'srefactor)
(require 'srefactor-lisp)

;;;; ff-search-directories
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq ff-search-directories '("."
			      "../inc"
			      "../inc/*"
			      "../include/*"
			      "../src"
                              "../include"
			      "../src/*"
			      "../../src/*"
			      "../../../src/*"
                              "../../src/*/*"
			      "../../../src/*/*/*"
                              "/usr/include"
			      "/usr/local/include/*"))
(defvar my-cpp-other-file-alist
'(("\\.cpp\\'" (".h" ".hpp"))
("\\.h\\'" (".hpp" ".cpp"))
("\\.hpp\\'" (".cpp" ".cpp"))
("\\.cxx\\'" (".hxx" ".ixx"))
("\\.ixx\\'" (".cxx" ".hxx"))
("\\.hxx\\'" (".ixx" ".cxx"))
("\\.c\\'" (".h"))
("\\.h\\'" (".c"))
))
(setq-default ff-other-file-alist 'my-cpp-other-file-alist)


;;; Keymaps
;;;; Org mode 
(with-eval-after-load "org"
  (progn (define-key org-mode-map [kp-0] 'helm-org-rifle)
	 (define-key org-mode-map [M-up] 'outline-previous-heading)
	 (define-key org-mode-map [M-down] 'outline-next-heading)
	 (define-key org-mode-map [M-S-up] 'org-metaup)
	 (define-key org-mode-map [M-S-down] 'org-metadown)
	 (define-key org-mode-map [C-M-s-up] 'org-shiftmetaup)
	 (define-key org-mode-map [C-M-s-down] 'org-shiftmetadown)
	 (define-key org-mode-map [kp-end] 'org-clock-in)
	 (define-key org-mode-map [kp-next] 'org-clock-out)))

(global-set-key (kbd "<H-tab>") 'org-todo-list)
(global-set-key (kbd "<H-return>") 'org-journal-new-entry)

;;;; Outline/semantic Hyper navigation 

(global-set-key (kbd "H-w") 'outline-up-heading)
(global-set-key (kbd "H-a") 'hyper-left)
(global-set-key (kbd "H-s") 'hyper-down)
(global-set-key (kbd "H-d") 'hyper-right)

;;;; bspc compatible window switching 
(global-set-key (kbd "<s-left>")
		'(lambda()
		   (interactive)
		   (windmove-emacs-or-tmux
		    "left" "bspc node -f west; if [ $? -gt 0 ]; then bspc desktop -f prev; fi;")))
(global-set-key (kbd "<s-right>")
		'(lambda()
		   (interactive)
		   (windmove-emacs-or-tmux
		    "right" "bspc node -f east; if [ $? -gt 0 ]; then bspc desktop -f next; fi;")))
(global-set-key (kbd "<s-down>")
		'(lambda()
		   (interactive) (windmove-emacs-or-tmux "down" "bspc node -f south")))
(global-set-key (kbd "<s-up>")
		'(lambda()
		   (interactive) (windmove-emacs-or-tmux "up" "bspc node -f north")))

;; (global-set-key (kbd "<s-left>") 'windmove-left)
;; (global-set-key (kbd "<s-up>") 'windmove-up)
;; (global-set-key (kbd "<s-down>") 'windmove-)


(global-set-key (kbd "<S-up>") 'outline-up-heading)
(global-set-key (kbd "<S-down>") 'outline-forward-same-level)
(global-set-key (kbd "<backtab>") 'switch-to-previous-buffer)
(global-set-key (kbd "<f12>") 'helm-mini)
(global-set-key (kbd "<C-f12>") 'fasd-find-file)
(global-set-key (kbd "<M-f12>") 'helm-recentf)
(global-set-key (kbd "C-j") 'join-line)

(global-set-key (kbd "C-*") 'destroy-win)

(global-set-key (kbd "<C-escape>") 'delete-frame)


(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-x C-e") 'eval-sexp-or-region)

(global-set-key (kbd "<C-enter>") 'helm-org-rifle)

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)


;;;; F1 hydra
;;;;; F1-Main 
(defhydra hydra-f1 (:color blue :timeout 12 :hint none)
  "
 +-( HYDRAS )^^---+-( SYMBOL )-----------------^^^^-+-( FIND INFO )----^^^^+-( MISC )--^^+
 | _1_ shortcuts  | _fd_  find-function (lisp)   ^^ | _dd_,_da_ apropos    | _a_ce window|
 | _2_ fonts      | _fx_  xref definition        ^^ | _df_ helpful      ^^ | _s_wap      |
 | _3_ yassnippet | _fr_  xref references        ^^ | _dt_ toggle help  ^^ | _k_ill-ring |
 | _4_ transpose  | _fg_  gtags (_<tab>_ resumes)   | _di_ info indices ^^ | _t_reeview  |
 | _5_ window     | _fo_  find string occurance  ^^ +-( FIND WEB  )----^^^^+ _b_ookmarks |
 | _r_ect         | _fm_  cquery member hierarchy^^ | _SPC_ surfraw     ^^ | _c_apture   |
 | _O_rg          | _ff_  find-files             ^^ | sear_x_           ^^ | _J_ournal   |
 | _A_genda       |                             ^^^^| _g_oogle          ^^ | _j_ournal   |
 |             ^^ | ^^                           ^^ | _w_ikipedia       ^^ | _S_earch    |
 | _<f3>_ macro   | _RET_ srefactor at point     ^^ | _W_ebjump         ^^ | _R_eload    |
 +-^^-------------+-^^^^----------------------------+-^^--------------^^---+-----------^^+
"
;;;;;; hydras
("1" helper-hydra-shortcuts/body )
;;("s" helper-hydra-shortcuts/body )
("2" helper-hydra-fonts/body )
("3" helper-hydra-yasnippet/body  )
("4" helper-hydra-transpose/body )
("5" helper-hydra-window/body  )
("r" helper-hydra-rectangle/body   )
("O" helper-hydra-org-utils/body )
("A" helper-hydra-org-agenda/body   )
("<f3>" helper-hydra-macro/body )
;;;;;; find symbol 
("fd" find-function)
("fx" xref-find-definitions-other-window)
("fr" xref-find-references)
("fg" helm-gtags-dwim)
("<tab>" helm-gtags-resume)
("fo" helm-multi-occur-from-isearch) 
("fm" cquery-member-hierarchy)
("ff" helm-find-files)
("RET" srefactor-refactor-at-point)
;;;;;; find info
("dd" apropos-documentation)
("da" apropos)
("dt" toggle-context-help)
("df" helpful-at-point)
("di" helm-info)
;;;;;; web find
("SPC" helm-surfraw)
("x" helm-google-searx)
("g" helm-google)
("w" helm-wikipedia-suggest)
("W" webjump)
;;;;;; misc
("a" ace-window)
("s" ace-swap-window)
("b" helm-bookmarks)
("k" helm-show-kill-ring )
("t" open-tree-view)
("c" org-capture)
("h" toggle-context-help)

("J" org-journal-new-scheduled-entry)
("j" org-journal-new-entry)
("S" org-journal-search)

("<f1>" helm-M-x)
("R" (helper-reload-settings) "Reload settings.org")

;;;;;; hidden
("K" save-buffers-kill-emacs)

("H" (lambda ()  (interactive)
       (org-cycle-hide-drawers 'all)))

("BC" (lambda () (interactive)
	(setq-local company-backends (remove 'company-capf company-backends))))

("BS" (lambda () (interactive)
	(setq-local company-backends (remove 'company-semantic company-backends)))))
(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'hydra-f1/body)

;;;;; F1-shortcuts
(defhydra helper-hydra-shortcuts (:color blue :hint nil)
"
              ^Settings^
--------------------------------------------
Edit:                       ^^  Reload:
_1_: init.el                    _x_modmap and keyboard
_2_: helpers1.el                _<f2>_:emacs
_3_: helper-hydras.el           
_o_: openbox rc.xml             _i_nsert backends 
_z_: ~/.zshrc
_s_: ~/scripts
_g_: general.org

"
  ("1" ((lambda () (interactive) (find-file "~/.emacs.d/init.el"))))
  ("2" ((lambda () (interactive) (find-file "~/.emacs.d/manual-packages/helpers1.el"))))
  ("3" ((lambda () (interactive) (find-file "~/.emacs.d/manual-packages/helper-hydras.el"))))
  ("o" ((lambda () (interactive) (find-file "~/.config/openbox/rc.xml"))))
  ("z" ((lambda () (interactive) (find-file "~/.zshrc"))))
  ("g" ((lambda () (interactive) (find-file "~/org/general.org"))))
  ("s" ((lambda () (interactive) (helm-find-files "~/scripts/"))))
  ("x" (lambda () (interactive) (shell-command "keyboard_config.sh")))
  ("i" (insert (format "(setq company-backends '%s" company-backends)))
  ("<f2>" helper-reload-settings))





(global-set-key (kbd "<H-kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "<H-kp-add>") 'text-scale-increase)
(global-set-key (kbd "H-SPC") 'helm-all-mark-rings)

;;;;; Fill region 
(global-set-key (kbd "C-H-f") 'org-adjust-region)
(global-set-key (kbd "M-H-f") 'fill-region-as-paragraph)
(global-set-key (kbd "H-f") 'fill-region)

;;;; rmenu 
(global-set-key [f8] 'rmenumap)
(progn
  (define-prefix-command 'rmenumap)
  (define-key rmenumap [f8] (lambda () (interactive) (org-show-current-heading-tidily)))
  (define-key rmenumap [right] (lambda() (interactive) (enlarge-window-horizontally 15)))
  (define-key rmenumap [left] (lambda() (interactive) (shrink-window-horizontally 15)))
  (define-key rmenumap [down] (lambda() (interactive) (enlarge-window 5)))
  (define-key rmenumap [up] (lambda() (interactive) (shrink-window 5))))


;;;; rshift (F9) map
;; (global-set-key (kbd "<C-f9>") 'ace-jump-mode)

(global-set-key (kbd "<C-f9>") 'helm-all-mark-rings)
(global-set-key (kbd "<M-f9>") 'avy-goto-word-0)
(global-set-key (kbd "<M-f8>") 'avy-goto-char)

(global-set-key [f9] 'rshiftmap)
(progn
   ;; define a prefix keymap
   (define-prefix-command 'rshiftmap)
   (define-key rshiftmap [f9] 'ace-window)   
   (define-key rshiftmap [? ] 'srefactor-refactor-at-point)
   (define-key rshiftmap (kbd "c") 'aya-create)
   (define-key rshiftmap (kbd "e") 'aya-expand)
   (define-key rshiftmap (kbd "o") 'aya-open-line)   
   (define-key rshiftmap [?\t] 'org-global-cycle)
   (define-key rshiftmap [?\r]
     (lambda()
       (interactive)
       (let ((current-prefix-arg 4)) (call-interactively 'helm-ag))))
   
   (define-key rshiftmap [?\d] 'kill-whitespace)
   (define-key rshiftmap (kbd "<deletechar>") 'kill-whitespace)
   (define-key rshiftmap (kbd "=") 'align-using-char)
   (define-key rshiftmap (kbd "<up>")
     (lambda() (interactive) (windmove-emacs-or-tmux "up" "tmux select-pane -U")))
   (define-key rshiftmap (kbd "<down>")
     (lambda() (interactive) (windmove-emacs-or-tmux "down" "tmux select-pane -D")))
   (define-key rshiftmap (kbd "<right>")
     (lambda() (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
   (define-key rshiftmap (kbd "<left>")
     (lambda() (interactive) (windmove-emacs-or-tmux "left" "tmux select-pane -L")))

   (define-key rshiftmap (kbd "q") 'magit-diff-popup)
   (define-key rshiftmap (kbd "s") 'magit-status)
   (define-key rshiftmap (kbd "a") 'magit-dispatch-popup)
   (define-key rshiftmap (kbd "x") 'magit-commit)

   (define-key rshiftmap (kbd "t") 'org-tags-view)
   (define-key rshiftmap (kbd "T") (lambda () (interactive) (org-tags-view 'TODO-ONLY)))
;;    (define-key rshiftmap (kbd "H-t") (lambda () (interactive) (org-tags-view 'TODO-ONLY)))
   
   (define-key rshiftmap (kbd "f") 'helm-find-files)
   (define-key rshiftmap (kbd "k") 'kill-buffer)
   (define-key rshiftmap (kbd "b") 'helm-buffers-list)
   (define-key rshiftmap (kbd "`") 'rotate-window)
   (define-key rshiftmap (kbd "1") 'rotate:even-horizontal)
   (define-key rshiftmap (kbd "2") 'rotate:even-vertical)
   (define-key rshiftmap (kbd "3") 'rotate:main-horizontal)
   (define-key rshiftmap (kbd "4") 'rotate:main-vertical)
   (define-key rshiftmap (kbd "5") 'rotate:tiled)

   (define-key rshiftmap (kbd "\]") (lambda () (interactive)
				      (find-file "/ssh:garg@frezr.com:")))
   (define-key rshiftmap (kbd "\[") (lambda () (interactive)
				      (find-file
				       "/ssh:garg@frezr.com:pelican/content")))
   (define-key rshiftmap (kbd "p") (lambda () (interactive)
				      (find-file
				       "/ssh:garg@frezr.com:pelican/")))
   
 ;;  (define-key rshiftmap (kbd "\\") 'ripgrep-regexp)
   (define-key rshiftmap (kbd "\\") 'ff-find-other-file)

   (define-key rshiftmap [f8] 'helm-semantic-or-imenu))


;;;;; numpad / kp map
;; (global-set-key [kp-multiply] 'ace-jump-mode)
(global-set-key [kp-multiply] 'ace-jump-mode)

;;;;;; . for yasnippet
(global-set-key [kp-decimal] 'yas/insert-snippet)
(global-set-key [C-kp-decimal] 'yas-new-snippet)

(global-set-key [kp-delete] 'yas/insert-snippet)
(global-set-key [C-kp-delete] 'yas-new-snippet)


;;;;;; 0 for imenu

;; (global-set-key [kp-0] 'helm-navi-headings)

(global-set-key [C-kp-0] 'helm-semantic-or-imenu)
(global-set-key [kp-insert] 'helm-navi-headings)
(global-set-key [C-kp-insert] 'helm-semantic-or-imenu)

(global-set-key [kp-5] 'toggle-window-zoom)
(global-set-key [C-kp-5] 'fit-window-to-buffer)

(global-set-key [C-next] (lambda () (interactive) (enlarge-window -5)))
(global-set-key [M-previous] (lambda () (interactive) (enlarge-window 5)))

;;;;;; 1-4 axis: clock in and out
(global-set-key [kp-end] 'org-clock-in)
(global-set-key [kp-next] 'org-clock-out)


;;;;;; 4-6 axis: mark ring
;;;;;;; Pop mark
(global-set-key [kp-6] 'xah-pop-local-mark-ring) ; last local mark
(global-set-key [kp-right] 'xah-pop-local-mark-ring)
(global-set-key [C-kp-6] 'pop-global-mark) ;; global mark
(global-set-key [C-kp-right] 'pop-global-mark)
;;;;;;; View marks in Helm
(global-set-key [kp-4] 'helm-mark-ring)
(global-set-key [kp-left] 'helm-mark-ring)
(global-set-key [C-kp-4] 'helm-global-mark-ring)
(global-set-key [C-kp-left] 'helm-global-mark-ring)
(global-set-key [C-M-kp-4] 'helm-all-mark-rings)
;;;;;; 7-9 axis: next/prev buffer
(global-set-key [kp-9] 'next-buffer)
(global-set-key [kp-7] 'previous-buffer)
(global-set-key [kp-prior] 'next-buffer)
(global-set-key [kp-home] 'previous-buffer)

;;;;;; 8-2 axis: move by sexp
(global-set-key [C-kp-2] 'end-of-defun)
(global-set-key [C-kp-8] 'beginning-of-defun)

(global-set-key [kp-2] 'forward-sexp)
(global-set-key [kp-8] 'backward-sexp)

(global-set-key [C-kp-down] 'end-of-defun)
(global-set-key [C-kp-up] 'beginning-of-defun)

(global-set-key [kp-down] 'forward-sexp)
(global-set-key [kp-up] 'backward-sexp)


;;;;;;; Hydras

(defhydra hydra-navigation (:color pink)
  ("1" open-or-jump-tree-view)
  ("X" xref-find-definitions-other-window)
  ("x" xref-find-references)
  ("b" xref-pop-marker-stack)
;;  ("<kp-0>" helm-navi-headings :color blue)
 ("<f6>" helm-gtags-dwim :color blue)
 ("<right>" helm-gtags-next-history)
 ("<left>" helm-gtags-previous-history)

 ("<kp-5>" srefactor-refactor-at-point)

 ;; ("<kp-5>" ggtags-find-tag-dwim)

  ;; ("<kp-8>" ggtags-prev-next-mark)
  ;; ("<kp-2>" ggtags-next-mark)

 ("<kp-8>" helm-gtags-next-history)


 ;; ("<kp-8>" outline-previous-heading)

 ("+" outshine-insert-heading :color blue)
 ("TAB" outshine-cycle-buffer)
 ("q" nil "cancel" :color blue)
 )

(global-set-key (kbd "C-<f6>") 'hydra-outline/body)
(global-set-key (kbd "<f6>") 'hydra-navigation/body)

;;; Custom used by emacs and elpa

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-matching-paren t)
 '(column-number-mode t)
 '(company-auto-complete-chars (quote (32 95 40 41 46 39)))
 '(company-quickhelp-color-background "#202020")
 '(company-quickhelp-color-foreground "#bfbfbf")
 '(custom-safe-themes
   (quote
    ("93224e36419b7afe2e01f9a40cae34537254dd52cd0342ffa7f5668b2443fa9f" "1304f4c2eb1f7cf3f456a3bbd6b759db91f872bc2abc1b1f3131a0b2d70fea14" default)))
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0.5)
 '(fci-rule-color "#383838")
 '(flycheck-clang-include-path (quote ("(\"~/ogl/pone/include\")")))
 '(flycheck-clang-language-standard nil)
 '(flycheck-idle-change-delay 2)
 '(frame-background-mode (quote dark))
 '(global-linum-mode t)
 '(helm-google-engines
   (quote
    ((google . "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=%s")
     (searx . "https://searx.me/?engines=google&format=json&q=%s"))))
 '(keypad-numlock-setup 46 nil (keypad))
 '(keypad-numlock-shifted-setup (quote S-cursor) nil (keypad))
 '(keypad-setup 46 nil (keypad))
 '(keypad-shifted-setup 46 nil (keypad))
 '(lsp-ui-sideline-show-flycheck t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t cursor-intangible t face minibuffer-prompt)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("/home/garg/org/notes/python.org" "/home/garg/org/notes/opengl.org" "/home/garg/org/archive.org" "/home/garg/org/discrete-math.org" "/home/garg/org/general.org" "/home/garg/org/taskplan.org" "/home/garg/org/journal/20180917.org")))
 '(org-bullets-bullet-list (quote ("◉" "○" "◦" "•")))
 '(org-journal-dir "~/org/journal/")
 '(org-journal-enable-agenda-integration t)
 '(org-journal-file-format "%Y%m%d.org")
 '(org-journal-time-format "[ %R ] ")
 '(org-log-into-drawer t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
 '(org-outline-path-complete-in-steps nil)
 '(outline-minor-mode-prefix "M-#")
 '(package-selected-packages
   (quote
    (window-purpose unicode-input common-lisp-snippets lsp-html yasnippet-classic-snippets python-docstring org-projectile-helm pyvenv org-pdfview helpful powerline org-journal paredit clojure-mode react-snippets lsp-go srefactor-lisp helm-navi navi-mode use-package ace-jump-mode company-box lsp-ui company-lsp lsp-clangd cquery helm-firefox eyebrowse stickyfunc-enhance company-rtags helm-org-rifle helm-google company-web company-lua company-quickhelp helm-ag outshine neotree fold-dwim-org htmlize org-elisp-help elpa-mirror ecb helm-xref pelican-mode goto-last-change helm-company imenu-anywhere company-shell c-eldoc srefactor glsl-mode lua-mode undo-tree yasnippet-snippets rotate cmake-mode auctex headlong ace-window org-bullets autopair babel hydra which-key ox-reveal auto-yasnippet org-brain pc-bufsw buffer-stack helm-w3m sublime-themes go-mode gnugo go doremi-frm evil-magit vdiff-magit magit helm-dired-recent-dirs helm-dired-history ripgrep company-c-headers company projectile-speedbar sr-speedbar function-args helm-gtags projector projectile helm-projectile ag dumb-jump expand-region virtualenv flymake-lua xclip elpy fzf fasd bookmark+ helm-bm pdf-tools helm switch-window dracula-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pyvenv-exec-shell "/usr/bin/zsh")
 '(pyvenv-mode t)
 '(pyvenv-tracking-ask-before-change t)
 '(safe-local-variable-values
   (quote
    ((make-backup-files)
     (company-clang-arguments "-I/home/garg/ogl/" "-I /home/garg/ogl/pone/include"))))
 '(shift-select-mode nil)
 '(sr-speedbar-max-width 0)
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
 '(calendar-today ((t (:underline "sandy brown"))))
 '(company-echo-common ((t (:foreground "firebrick1"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(linum ((t (:background "#151515" :foreground "#9F9f9f" :underline nil))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "white" :box (:line-width -1 :color "white") :weight ultra-bold :height 0.99))))
 '(mode-line ((t (:foreground "#030303" :background "#adadad" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#353535" :box nil))))
 '(org-done ((t (:background "gray15" :foreground "green yellow" :weight bold))))
 '(org-todo ((t (:background "gray15" :foreground "#CC9393" :weight bold))))
 '(powerline-grey40-white ((t (:background "black" :foreground "white" :box nil))) t)
 '(show-paren-match ((t (:background "#191925" :weight bold))))
 '(show-paren-match-expression ((t (:inherit show-paren-match :underline nil)))))

(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'narrow-to-region 'disabled nil)

