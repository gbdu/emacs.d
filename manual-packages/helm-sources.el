;; TODO
(setq my-font-list '("Neep-10"
		     "Terminus-9"
		     "lime-8"
		     "Fira Mono Medium-9"
		     "Iosevka-10:weight=Semibold"
		     "Monaco-9"
		     "Ubuntu Mono-11"
		     "Space Mono-10"))


(defun my-todo-items ()
  "Show `hl-todo'-keyword items in buffer."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "hl-todo items"
                   :data (current-buffer)
                   :candidate-transformer (lambda (candidates)
					    (let ((case-fold-search nil))
                                            (cl-loop for c in candidates
                                                     when (string-match "TODO" c)
                                                     collect c)))
                   :get-line #'buffer-substring)
        :buffer "*helm hl-todo*"))

(defun my--helm-source-select-action (candidate)
	   (set-default-font candidate))
	
(defclass my-font-class (helm-source-sync)
  ((candidates :initform 'my-font-list)
   (action :initform (helm-make-actions
                      "Select" #'my--helm-source-select-action))))

(defun my-change-font ()
   (interactive)
   (helm :sources (helm-make-source "fonts" 'my-font-class)
      :buffer "*fonts*"))


(setq my-helm-shortcuts '(my-change-font my-todo-items) )

(defun my--call-action (candidate)
	   (candidate))
	
(defclass my-helm-class (helm-source-sync)
  ((candidates :initform 'my-helm-shortcuts)
   (action :initform (helm-make-actions
                      "Select" #'my--call-action))))

(defun my-helm()
  (interactive)
  (helm :sources (helm-make-source "shortcuts" 'my-helm-class)
      :buffer "*shortcuts*"))

(my-helm)



(defun some-action (candidate)
  (candidate))

(defun some-actions (candidate)
  (mapc 'some-action (helm-marked-candidates)))

(helm :sources '(((name . "HELM")
                  (candidates . my-helm-shortcuts
                  (action . some-actions)))))


(defun random-candidates ()
  "Return a list of 4 random numbers from 0 to 10"
  (loop for i below 4 collect (random 10)))

(setq some-helm-source
      '((name . "HELM at the Emacs")
        (candidates . my-helm-shortcuts)
        (action . (lambda (candidate)
                    (call-interactively candidate)))))

(helm :sources '(some-helm-source))

(provide 'my-helm-sources)
