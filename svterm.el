
(defvar svterm-buffers (make-ring 10)
  "Ring of `vterm' buffers")

(defvar svterm-disply-buffer-actions '((display-buffer-reuse-window display-buffer-pop-up-window display-buffer-use-least-recent-window)
                                       ((reusable-frames . 0)))
  "Choose how the vterm buffer is to be displayed. It has the structure of the
ACTION parameter of `display-buffer'.")

(defun svterm-pop-to-buffer (buf)
  (pop-to-buffer buf svterm-disply-buffer-actions))

(defun svterm-vterm-buffer-p (buf)
  (let ((buf-proc (get-buffer-process buf)))
    (if (and buf-proc
             (string-prefix-p "vterm" (process-name buf-proc))))))

(defun svterm-generate-vterm-buffer ()
  "Create and return a new `vterm' buffer."
  (let ((buf (generate-new-buffer (format "*vterm*<%d>" (ring-length svterm-buffers)))))
    (with-current-buffer buf
      (vterm-mode))
    buf))

(defun svterm-find-buffer-index (buf)
  (seq-position (ring-elements svterm-buffers) buf #'equal))

(defun svterm-new-buffer ()
  "Generate new `vterm' buffer, put it into `svterm-buffers' and return it."
  (when (= (ring-length svterm-buffers) (ring-size svterm-buffers))
    ;; ring full: increase size
    (ring-resize svterm-buffers
                 (+ (ring-size svterm-buffers) 5)))
  (ring-insert svterm-buffers (svterm-generate-vterm-buffer)))

(defun svterm-remove-buffer (buf)
  (let ((index (svterm-find-buffer-index buf)))
    (when index
      (ring-remove svterm-buffers index))))

(defun svterm-buffers-remove-killed-buffer ()
  "Remove the killed `vterm' buffer from `svterm-buffers'."
  (when (svterm-vterm-buffer-p (current-buffer))
    (svterm-remove-buffer (current-buffer))))

(add-to-list 'kill-buffer-hook #'svterm-buffers-remove-killed-buffer)

(defun svterm-new ()
  (interactive)
  (svterm-pop-to-buffer (svterm-new-buffer)))

(defun svterm-next (&optional prefix-arg)
  (interactive "P")
  (cond (;; create new one if the ring is empty
         (ring-empty-p svterm-buffers)
         (svterm-new))

        (;; if a vterm buffer is current
         (svterm-vterm-buffer-p (current-buffer))
         (svterm-pop-to-buffer
          (ring-ref svterm-buffers
                    (if prefix-arg -1 1))))

        (t ;; else
         (svterm-pop-to-buffer (ring-ref svterm-buffers 0)))))

(defun svterm-prev (&optional prefix-arg)
  (svterm-next
   (if prefix-arg
       nil
     prefix-arg)))
