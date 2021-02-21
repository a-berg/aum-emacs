;;;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun aum/kill-this-buffer ()
  (interactive)
  (kill-buffer))

(defun aum/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers.
Source: https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Faicon functions from Jerry Peng, useful for making pretty hydras prettier.
;; found in: https://github.com/jerrypnz/.emacs.d/blob/master/lisp/jp-icons.el
(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-octicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
  (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-mode-icon (mode str &optional height nospace face)
  (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
         (args     `(:height ,(or height 1) :v-adjust ,v-adjust))
         (_         (when face
                      (lax-plist-put args :face face)))
         (icon     (apply #'all-the-icons-icon-for-mode mode args))
         (icon     (if (symbolp icon)
                       (apply #'all-the-icons-octicon "file-text" args)
                     icon)))
    (s-concat icon (if nospace "" " ") str)))
;; ---

(provide 'aum-functions-macros)
