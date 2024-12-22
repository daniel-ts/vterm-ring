
(require 'seq)
(require 'vterm)

(defvar vterm-ring-vterms (make-ring 10)
  "Ring that satisfies `ring-p' of `vterm' buffers.")

(defvar vterm-ring-display-buffer-in-focus
  '((display-buffer-same-window)
    ((reusable-frames . 0)))
  "`display-buffer' actions like in the `display-buffer-base-action' variable
that are used to display a `vterm' buffer when one is already in focus.")

(defvar vterm-ring-display-buffer-out-of-focus
  '((display-buffer-reuse-window
     display-buffer-pop-up-window
     display-buffer-use-least-recent-window)
    ((reusable-frames . 0)))
  "`display-buffer' actions like in the `display-buffer-base-action' variable
that are used to display a `vterm' buffer when none is current.")

(defun vterm-ring--vterm-buffer-p (buf)
  "Predicate that gauges if BUF is a `vterm' buffer."
  (let ((buf-proc (get-buffer-process buf)))
    (and buf-proc
         (string-prefix-p "vterm" (process-name buf-proc)))))

(defun vterm-ring--generate-vterm-buffer ()
  "Create and evalutate to a new `vterm' buffer."
  (let ((buf (generate-new-buffer
              ;; TODO: this is not unique:
              (format "*vterm*<%d>" (ring-length vterm-ring-vterms)))))
    (with-current-buffer buf
      (vterm-mode))
    buf))

(defun vterm-ring--find-buffer-index (buf)
  "Find a `vterm' buffer that is `equal' to BUF in `vterm-ring-vterms' and
evalutate to its index."
  (seq-position (ring-elements vterm-ring-vterms) buf #'equal))

(defun vterm-ring--new-buffer ()
  "Generate new `vterm' buffer, put it into `vterm-ring-vterms' and evaluate to
it."
  (when (= (ring-length vterm-ring-vterms) (ring-size vterm-ring-vterms))
    ;; ring full: increase size
    (ring-resize vterm-ring-vterms
                 (+ (ring-size vterm-ring-vterms) 5)))
  (ring-insert vterm-ring-vterms (vterm-ring--generate-vterm-buffer)))

(defun vterm-ring--remove-buffer (buf)
  "Remove BUF from `vterm-ring-vterms' and evaluate to it."
  (let ((index (vterm-ring--find-buffer-index buf)))
    (when index
      (ring-remove vterm-ring-vterms index))))

(defun vterm-ring--remove-killed (buf event)
  "Remove the killed `vterm' buffer from `vterm-ring-vterms', like in the
`vterm-exit-functions' abnormal hook."
  (vterm-ring--remove-buffer buf))

(add-to-list 'vterm-exit-functions #'vterm-ring--remove-killed)

(defun vterm-ring-new ()
  "Create and display a new `vterm' buffer that is managed by `vterm-ring'."
  (interactive)
  (pop-to-buffer (vterm-ring--new-buffer) 'vterm-ring-display-buffer-out-of-focus))

(defun vterm-ring-next (&optional prefix-arg)
  "Similar to `vterm-ring-prev'.

Display an existing `vterm' buffer. If no vterm buffer exists yet, create one.
If a vterm buffer is current, display its successor in the ring.
Otherwise, display the most recent vterm buffer.

This behavior can be controlled via `vterm-ring-display-buffer-in-focus' and
`vterm-ring-display-buffer-out-of-focus', or, of course, via
`display-buffer-overriding-action' or an entry in `display-buffer-alist'.

If called with a PREFIX-ARG, the predecessor is displayed instead of the
successor."
  (interactive "P")
  (cond (;; create new one if the ring is empty
         (ring-empty-p vterm-ring-vterms)
         (vterm-ring-new))

        (;; if a vterm buffer is current
         (vterm-ring--vterm-buffer-p (current-buffer))
         (pop-to-buffer
          (ring-ref vterm-ring-vterms
                    (+ (vterm-ring--find-buffer-index (current-buffer))
                       (if prefix-arg -1 1)))
          'vterm-ring-display-buffer-in-focus))

        (t ;; else
         (pop-to-buffer
          (ring-ref vterm-ring-vterms 0)
          'vterm-ring-display-buffer-out-of-focus))))

(defun vterm-ring-prev (&optional prefix-arg)
  "Similar to `vterm-ring-prev'.

Display an existing `vterm' buffer. If no vterm buffer exists yet, create one.
If a vterm buffer is current, display its predecessor in the ring.
Otherwise, display the most recent vterm buffer.

This behavior can be controlled via `vterm-ring-display-buffer-in-focus' and
`vterm-ring-display-buffer-out-of-focus', or, of course, via
`display-buffer-overriding-action' or an entry in `display-buffer-alist'.

If called with a PREFIX-ARG, the successor is displayed instead of the
predecessor."
  (vterm-ring-next
   (if prefix-arg
       nil
     prefix-arg)))

(provide 'vterm-ring)
