(require 'helm-config)
(require 'helm)
;;; toggle modes

(defun my-helm-shortcuts ()
  (interactive)
  (let ((selected-source
	(helm :sources (helm-build-sync-source "shortcuts"
			 :candidates 'my-sources)
	      :buffer "*shortcuts*")))
    (funcall selected-source)))


;;; fonts 
(setq my-font-list '("Neep-10"
		     "Terminus-9"
		     "lime-8"
		     "Fira Mono Medium-9"
		     "Iosevka-10:weight=Semibold"
		     "Monaco-9"
		     "Ubuntu Mono-11"
		     "Space Mono-10"))

(setq helm-source-fonts
      (helm-build-sync-source "fonts"
	:candidates 'my-font-list))

(defun my-change-font ()
   (interactive)
   (set-default-font (helm :sources helm-source-fonts
      :buffer "*fonts*")))


;;; goto settings
(defun my-goto-settings ()
  (interactive)
  (helm :sources (helm-build-in-file-source
                   "settings" "~/.emacs.d/init.el"
                   :action (lambda (candidate)
                             (let ((linum (with-helm-buffer
                                            (get-text-property
                                             1 'helm-linum
                                             (helm-get-selection nil 'withprop)))))
                               (find-file (with-helm-buffer
                                            (helm-attr 'candidates-file)))
                               (goto-line linum)))
		   :get-line #'buffer-substring)
      :buffer "*settings*"))

;;; favorites/shortcuts
(defclass helm-source-fav (helm-source-in-file helm-type-file)
  ((candidates-file :initform "~/.emacs.d/shortcuts")))

(defun my-goto-shortcuts ()
  (helm :sources (helm-make-source "fav" 'helm-source-fav)
      :buffer "*helm fav*"))

;;; entry point my-helm-shortcuts
(setq my-sources '(("Font" . my-change-font)
		   ("Setting" . my-goto-settings)
		   ("Shortcuts" . my-goto-shortcuts)
		   ("Modes" . my-mode-shortcuts)
		   ("TODO" . org-todo-list)))

(defun my-helm-shortcuts ()
  (interactive)
  (let ((selected-source
	(helm :sources (helm-build-sync-source "shortcuts"
			 :candidates 'my-sources)
	      :buffer "*shortcuts*")))
    (funcall selected-source)))


(provide 'my-helm-sources)
