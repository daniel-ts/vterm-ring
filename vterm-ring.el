;;; vterm-ring.el --- Simply manage multiple vterm buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024 by Daniel Tschertkow
;;
;; Author: Daniel Tschertkow <daniel.tschertkow@posteo.de>
;; URL: https://github.com/akermu/emacs-libvterm
;; Keywords: terminals vterm
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Simply manage multiple vterm buffers via a ring.

;;; Code:
(require 'seq)
(require 'vterm)

(defvar vterm-ring--vterms (make-ring 10)
  "Ring that satisfies `ring-p' of `vterm' buffers.")

(defcustom vterm-ring-display-buffer-in-focus
  '((display-buffer-same-window)
    ((reusable-frames . 0)))
  "Set the `display-buffer' actions like in `display-buffer-base-action'.
These are used to display a `vterm' buffer when one is already in focus."
  :group 'vterm-ring)

(defcustom vterm-ring-display-buffer-out-of-focus
  '((display-buffer-reuse-window
     display-buffer-pop-up-window
     display-buffer-use-least-recent-window)
    ((reusable-frames . 0)))
  "Set the `display-buffer' actions like in `display-buffer-base-action'.
These are used to display a `vterm' buffer when none is current."
  :group 'vterm-ring)

(defun vterm-ring--vterm-buffer-p (buf)
  "Predicate that gauges if BUF is a `vterm' buffer."
  (let ((buf-proc (get-buffer-process buf)))
    (and buf-proc
         (string-prefix-p "vterm" (process-name buf-proc)))))

(let ((cnt -1))
  (defun vterm-ring--next-buf-id ()
    "Evaluate to monotonically incremeted value."
    (setq cnt (1+ cnt))))

(defun vterm-ring--generate-vterm-buffer ()
  "Create and evalutate to a new `vterm' buffer."
  (let ((buf (generate-new-buffer
              (format "*vterm*<%d>" (vterm-ring--next-buf-id)))))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook
                #'(lambda ()
                    (run-hook-with-args 'vterm-exit-functions buf))
                nil t)
      (vterm-mode))
    buf))

(defun vterm-ring--find-buffer-index (buf)
  "Find a `vterm' buffer that is `equal' to BUF in `vterm-ring--vterms'.
Evalutate to its index."
  (seq-position (ring-elements vterm-ring--vterms) buf #'equal))

(defun vterm-ring--new-buffer ()
  "Generate a new `vterm' buffer.
Put it into `vterm-ring--vterms' and evaluate to it."
  (when (= (ring-length vterm-ring--vterms) (ring-size vterm-ring--vterms))
    ;; ring full: increase size
    (ring-resize vterm-ring--vterms
                 (+ (ring-size vterm-ring--vterms) 5)))
  (ring-insert vterm-ring--vterms (vterm-ring--generate-vterm-buffer)))

(defun vterm-ring--remove-buffer (buf)
  "Remove BUF from `vterm-ring--vterms' and evaluate to it."
  (let ((index (vterm-ring--find-buffer-index buf)))
    (when index
      (ring-remove vterm-ring--vterms index))))

(defun vterm-ring--remove-killed (buf &optional _)
  "Remove the killed `vterm' buffer BUF from `vterm-ring--vterms'.
See the `vterm-exit-functions' abnormal hook."
  (vterm-ring--remove-buffer buf))

(eval-after-load 'vterm
  '(add-to-list 'vterm-exit-functions #'vterm-ring--remove-killed))

;;;###autoload
(defun vterm-ring-new ()
  "Create and display a new `vterm' buffer that is managed by `vterm-ring'."
  (interactive)
  (pop-to-buffer (vterm-ring--new-buffer) 'vterm-ring-display-buffer-out-of-focus))

;;;###autoload
(defun vterm-ring-next (&optional prefix)
  "Similar to `vterm-ring-prev'.

Display an existing `vterm' buffer.  If no vterm buffer exists yet, create one.
If a vterm buffer is current, display its successor in the ring.
Otherwise, display the most recent vterm buffer.

This behavior can be controlled via `vterm-ring-display-buffer-in-focus' and
`vterm-ring-display-buffer-out-of-focus', or, of course, via
`display-buffer-overriding-action' or an entry in `display-buffer-alist'.

If called with a PREFIX, the predecessor is displayed instead of the
successor."
  (interactive "P")
  (cond (;; create new one if the ring is empty
         (ring-empty-p vterm-ring--vterms)
         (vterm-ring-new))

        (;; if a vterm buffer is current
         (vterm-ring--vterm-buffer-p (current-buffer))
         (pop-to-buffer
          (ring-ref vterm-ring--vterms
                    (+ (vterm-ring--find-buffer-index (current-buffer))
                       (if prefix -1 1)))
          'vterm-ring-display-buffer-in-focus))

        (t ;; else
         (pop-to-buffer
          (ring-ref vterm-ring--vterms 0)
          'vterm-ring-display-buffer-out-of-focus))))

;;;###autoload
(defun vterm-ring-prev (&optional prefix)
  "Inversion of `vterm-ring-next'.
Use PREFIX to chenge direction."
  (interactive "P")
  (vterm-ring-next
   (if prefix
       nil
     prefix-arg)))

(provide 'vterm-ring)
;;; vterm-ring.el ends here
