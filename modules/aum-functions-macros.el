;;;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun aum/kill-this-buffer ()
  (interactive)
  (kill-buffer))


(provide 'aum-functions-macros)
