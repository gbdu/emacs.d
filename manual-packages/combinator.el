;; (require 'helm-config)
;; (require 'helm)
;; ;;; toggle modes

;; (defun my-helm-shortcuts ()
;;   (interactive)
;;   (let ((selected-source
;; 	(helm :sources (helm-build-sync-source "shortcuts"
;; 			 :candidates 'my-sources)
;; 	      :buffer "*shortcuts*")))
;;     (funcall selected-source)))
