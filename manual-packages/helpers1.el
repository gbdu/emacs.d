;; * Required
(require 'cl) 

;; * eval-sexp-or-region
(defun eval-sexp-or-region ()
  "Eval sexp or region depending on whether or not a region is active"
  (interactive)
  (cond
   (mark-active (call-interactively 'eval-region))
   (t (call-interactively 'eval-last-sexp))))

;; * Completion
(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(defun et/semantic-remove-hooks ()
  "Fix for some semantic completion problems"
  (interactive)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))

(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))


;; * Org
;; ** adjust region
(defun org-adjust-region (b e)
  "Re-adjust stuff in region according to the preceeding stuff."
  (interactive "r") ;; current region
  (save-excursion
    (let ((e (set-marker (make-marker) e))
      (_indent (lambda ()
	     (insert ?\n)
	     (backward-char)
	     (org-indent-line)
	     (delete-char 1)))
      last-item-pos)
      (goto-char b)
      (beginning-of-line)
      (while (< (point) e)
    (indent-line-to 0)
    (cond
     ((looking-at "[[:space:]]*$")) ;; ignore empty lines
     ((org-at-heading-p)) ;; just leave the zero-indent
     ((org-at-item-p)
      (funcall _indent)
      (let ((struct (org-list-struct))
	(mark-active nil))
	(ignore-errors (org-list-indent-item-generic -1 t struct)))
      (setq last-item-pos (point))
      (when current-prefix-arg
	(fill-paragraph)))
     ((org-at-block-p)
      (funcall _indent)
      (goto-char (plist-get (cadr (org-element-special-block-parser e nil)) :contents-end))
      (org-indent-line))
     (t (funcall _indent)))
    (forward-line))
      (when last-item-pos
    (goto-char last-item-pos)
    (org-list-repair)))))

;; ** Add ids to headlines
(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

;; ** kill ID
(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring, if no ID is there then
   create a new unique ID. This function works only in org-mode buffers."
  
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)))

;; ** Hide drawers

(defun org-cycle-hide-drawers (state)
  "The following answer completely hides everything from :PROPERTIES:
through :END:. It can be tested by evaluating (org-cycle-hide-drawers
'children), or (org-cycle-hide-drawers 'all), or in conjunction with
the other functions relating to cycling the outline views. The
standard functions to unfold that are included within the org-mode
family all work -- e.g., show-all; org-show-subtree; etc."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                      (point-min)
                    (point)))
             (end (if globalp
                      (point-max)
                    (if (eq state 'children)
			(save-excursion
                          (outline-next-heading)
                          (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                      (save-excursion
                        (outline-next-heading)
                        (point)))
                     (msg (format
                           (concat
                            "org-cycle-hide-drawers:  "
                            "`:END:`"
                            " line missing at position %s")
                           (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                    (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))

;; ** org-journal-find-location 
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))


;; ** TODO Show next entry keeping other entries closed
(defun org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))


;; * File handling
;; ** Find sub dirs 
(defun find-all-subdirectories(dir-list)
  "Returns a list of all recursive subdirectories of dir-list, 
   ignoring directories with names that start with . (dot)"
  (split-string 
   (shell-command-to-string 
     (concat "find " 
             (mapconcat 'identity dir-list " ")
             " -type d -not -regex \".*/\\\..*\""))))

;; ** Open current file with xdg

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;; ** TODO Show in file manager 

;; (defun xah-show-in-desktop ()
;;   "Show current file in desktop.
;;  (Mac Finder, Windows Explorer, Linux file manager)
;;  This command can be called when in a file or in `dired'.

;; URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
;; Version 2018-01-13"
;;   (interactive)
;;   (let (($path (if (buffer-file-name) (buffer-file-name) default-directory )))
;;     (cond
;;      ((string-equal system-type "windows-nt")
;;       (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" $path t t)))
;;      ((string-equal system-type "darwin")
;;       (if (eq major-mode 'dired-mode)
;;           (let (($files (dired-get-marked-files )))
;;             (if (eq (length $files) 0)
;;                 (shell-command
;;                  (concat "open " (shell-quote-argument default-directory)))
;;               (shell-command
;;                (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
;;         (shell-command
;;          (concat "open -R " $path))))
;;      ((string-equal system-type "gnu/linux")
;;       (let ((process-connection-type nil)
;;             ;; (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
;;             ;;                      "/usr/bin/gvfs-open"
;;             ;;                    "/usr/bin/xdg-open")))
;; 	    (openFileProgram "pcmanfm"))
	    
;;         (start-process "" nil "pcmanfm" $path))
;;       ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
;;       ))))

;; ** Get closest path name


(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from
the current directory towards root.  This may not do the correct thing
in presence of links. If it does not find FILE, then it shall return
the name of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return nil))))


;; * Editing 
;; ** Newline without break of line
;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "move to end of the line, insert newline with indent"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; ** Delete leading whitespace
(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))

;; ** Kill whitespace
(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*") ;; means error is signaled if readonly
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (re-search-backward "[^ \t\r\n]" nil t)
	  (re-search-forward "[ \t\r\n]+" nil t)
	  (replace-match "" nil nil))))))

;; ** Align text
(defun align-to-equals (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp
    begin end "\\(\\s-*\\)=" 1 1 ))

(defun align-using-char (begin end spacer)
  "Align region using the char given as spacer"
  (interactive "r\nsSpacer to use: ")
  (align-regexp begin end (concat "\\(\\s-*\\)" spacer) 1 1 ))
  
;; ** Move lines 
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))






(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;; ** increment-number-at-point 
(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; * Hyper Navigation
;; ** Hyper navigation (semantic-outline)
;; ** extract-comment-from-tag  
(defun extract-comment-from-tag ()
  (interactive)
  (progn
    (senator-copy-tag-to-register 'a)
    (beginning-of-defun-comments)
    (outshine-insert-heading)
    (insert-register 'a)
    (re-search-forward "(" nil t)
    (replace-match " ")
    (re-search-forward ")" nil t)
    (replace-match " ")))


;; ** hyper-nav-up-or-previous  
(defun hyper-nav-up-or-previous ()
  (interactive)
  ;; (message (format "%s" (outline-up-heading -1))))
  (outline-up-heading -1))


;; ** hyper-left  
(defun hyper-left ()
  (interactive)
  (condition-case nil
    (progn
      (senator-previous-tag)
;;      (outline-hide-other)
      (outline-show-entry))
    (error (progn
	     (outline-previous-heading)
	     (outline-show-entry)))))

;; ** hyper-right  
(defun hyper-right ()
  (interactive)
  (cond
   ((org-at-heading-p) (outline-toggle-children))
   (t (senator-fold-tag-toggle))))

;; ** hyper-up  
(defun hyper-up ()
  "go up semantically or in outline"
  (interactive)
  (condition-case nil
      (senator-go-to-up-reference)
    (error (progn
	     (outline-up-heading 1)
	     (outline-show-entry)))))

;; ** hyper-down  
(defun hyper-down ()
  "go down semantically or in outline"
  (interactive)
  (condition-case nil
      (progn
	(senator-next-tag)
;;	(outline-hide-other)
	(outline-show-entry))
    (error (progn
;;	     (outline-up-heading 1)
	     (outline-next-heading)
	     (outline-show-entry)))))


;; * context help
;; help-window that will automatically update to
;; display the help of the symbol before point.

(defun toggle-context-help ()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it. A double toggle will make it reappear"
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (if (setq context-help (not context-help))
        (progn
           (if (not (get-buffer-window (help-buffer)))
               (display-buffer (help-buffer)))))
    (message "Context help %s" (if context-help "ON" "OFF"))))

(defun context-help ()
  "Display function or variable at point in *Help* buffer if visible.
Default behaviour can be turned off by setting the buffer local
context-help to false"
  (interactive)
  (let ((rgr-symbol (symbol-at-point))) ; symbol-at-point
					; http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
    (with-current-buffer (help-buffer)
     (unless (local-variable-p 'context-help)
       (set (make-local-variable 'context-help) t))
     (if (and context-help (get-buffer-window (help-buffer))
         rgr-symbol)
       (if (fboundp  rgr-symbol)
           (describe-function rgr-symbol)
         (if (boundp  rgr-symbol) (describe-variable rgr-symbol)))))))

;; * eldoc-print-current-symbol-info  
(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag activate)
  (cond
        ((eq major-mode 'emacs-lisp-mode) (context-help) ad-do-it)
        ((eq major-mode 'lisp-interaction-mode) (context-help) ad-do-it)
        ((eq major-mode 'apropos-mode) (context-help) ad-do-it)
        (t ad-do-it)))

;; * pop local mark ring
(defun xah-pop-local-mark-ring ()
  "  Call this repeatedly will cycle all positions in `mark-ring'.
  URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
  version 2016-04-04"
    (interactive)
    (set-mark-command t))

;; * kill window and buffer 
(defun destroy-win()
  "Kill buffer and delete the window"
  (interactive)
  (kill-this-buffer) (delete-window))

;; * switch to previous buffer 
(defun switch-to-previous-buffer ()
  "Repeated invocations toggle between the two most recently open buffers."
  (interactive) (switch-to-buffer (other-buffer (current-buffer) )))

;; * open-tree-view
(defun open-tree-view ()
    "Open a clone of the current buffer to the left, resize it to 30
columns, and bind <mouse-1> to jump to the same position in the base
buffer."
    (interactive)
    (if (string-match-p (regexp-quote "<tree>") (buffer-name))
    (jump-tree-view)

    (let ((new-buffer-name (concat "<tree>" (buffer-name))))
      ;; Create tree buffer
      (split-window-right 30)
      (if (get-buffer new-buffer-name)
          (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
        ;; Make new tree buffer
        (progn  (clone-indirect-buffer new-buffer-name nil t)
                (switch-to-buffer new-buffer-name)
                (read-only-mode)
                (hide-body)
                (toggle-truncate-lines)

                ;; Do this twice in case the point is in a hidden line
                (dotimes (_ 2 (forward-line 0)))

                ;; Map keys
                (use-local-map (copy-keymap outline-mode-map))
                (local-set-key (kbd "q") 'delete-window)
                (mapc (lambda (key) (local-set-key (kbd key) 'jump-tree-view))
                '("<mouse-1>" "RET")))))))

;; ** jump to cloned buffer 
(defun jump-tree-view()
  "Switch to a cloned buffer's base buffer and move point to the cursor position in the clone."
  (interactive)
  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))
      (goto-char pos)
      (when (invisible-p (point))
        (show-branches)))))



;; * set vim foldmarkers (aka "{{{")
(defun set-vim-foldmarker (fmr)
      "Set Vim-type foldmarkers for the current buffer"
      (interactive "sSet local Vim foldmarker: ")
      (if (equal fmr "")
          (message "Abort")
        (setq fmr (regexp-quote fmr))
        (set (make-local-variable 'outline-regexp)
             (concat ".*" fmr "\\([0-9]+\\)"))
        (set (make-local-variable 'outline-level)
             `(lambda ()
                (save-excursion
                  (save-match-data
                    (re-search-forward ,(concat fmr "\\([0-9]+\\)") nil t)
                    (string-to-number (match-string 1))))))))

;; * insert date

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "date")))

;; * Move by window direction even inside tmux 
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
     nil                       ;; Moving within emacs
     (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

;; * Notify compilation result
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
    (progn
     (delete-windows-on buffer)
     (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n "))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )


;; * Hideshow
;; (defun display-code-line-counts (ov)
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (overlay-put ov 'help-echo
;; 		 (buffer-substring (overlay-start ov)
;; 				   (overlay-end ov)))))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

;; (defun toggle-hiding (column)
;;   (interactive "P")
;;   (if hs-minor-mode
;;       (if (condition-case nil
;;               (hs-toggle-hiding)
;;             (error t))
;;           (hs-show-all))
;;     (toggle-selective-display column)))


(defun toggle-window-zoom ()
  "Toggles one window zoom and switches back using winner"
  (interactive)
  (let ((len (length (window-list))))
    (if (equal 1 len)
	(winner-undo)
      (delete-other-windows))))


(defun toggle-window-zoom2 ()
  "docstring"
  (interactive)
  (let ((len (length (window-list))))
    (if (equal 1 len)
	(progn (jump-to-register 'a))

      (progn (window-configuration-to-register 'a)
	     (delete-other-windows)))))

;; (defun check-register (args)
;;   "docstring"
;; ;;  (get-register 119)
;;   (message "%s aaaa" (car args))
;;   (set-window-configuration (car args))

;;   )
;; window-configuration-to-register (C-x r w)
;; (let ((list (copy-sequence register-alist)))
;;   (dolist (elt list)
;;     (when (get-register (car elt))
;;     (check-register (get-register (car elt))))))

;; (register-alist)
;; (toggle-window-zoom)

;; * Provide 
(provide 'helpers1)
