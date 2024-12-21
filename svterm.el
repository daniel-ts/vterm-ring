
(defvar svterm-buffer-count 20
  "Number of vterm buffers that are managed by smulti-vterm.")

(defvar svterm-buffer-index 0
  "Index of current vterm buffer managed by smulti-vterm")

(defvar svterm-buffers (make-vector svterm-buffer-count nil)
  "vector of lengthvterm buffers")

(defun svterm-advance-index! ()
  "Return next index of `svterm-buffer-index'."
  (setq svterm-buffer-index
        (mod (+ svterm-buffer-index 1) svterm-buffer-count)))

(defun svterm-current ()
  "Return current buffer of `svterm-buffers' pointed to by `svterm-buffer-index'."
  (aref svterm-buffers svterm-buffer-index))

(defun svterm-next-slot! ()
  "Return the buffer of `svterm-buffers' in the slot next in line or nil, if it
is free."
  (svterm-advance-index!)
  (if (buffer-live-p (svterm-current))
      (svterm-current)
    (aset svterm-buffers svterm-buffer-index nil)))

(defun svterm-find-next! ()
  "Iterate through `svterm-buffers' and return the next live `vterm' buffer."
  (let ((start-i vterm-buffer-index))
    (while (not (= vterm-buffer-index start-i))
      (when (svterm-next-slot!)
        (return (svterm-current)))
      )))

(defun svterm-find-free! ()
  "Iterate through `svterm-buffers' and return the index of the next free slot."
  (let ((start-i vterm-buffer-index))
    (while (not (= vterm-buffer-index start-i))
      (unless (svterm-next-slot!)
        (return svterm-buffer-index))
      )))

(defun svterm-make-buffer ()
  "Create and return a new `vterm' buffer."
  (let ((buf (generate-new-buffer (format "*vterm*<%d>" svterm-buffer-index))))
    (with-current-buffer buf
      (vterm-mode))
    buf))

(defun svterm-new ()
  (interactive)
  (let ((start-i vterm-buffer-index))
    (while t
      (svterm-advance-index!)

      )
    )
  (if (svterm-find-free)
      ())
  )


(defun svterm-next ()
  (interactive)
  (let ((buf (svterm-find-next)))
    (if buf
        (pop-to-buffer buf)
      (set buf (svterm-make-buffer))
      (aset svterm-buffers svterm-buffer-index buf)
      (pop-to-buffer buf))))
