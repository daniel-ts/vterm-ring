
(defvar svterm-buffer-count 20
  "Number of vterm buffers that are managed by smulti-vterm.")

(defvar svterm-buffer-index 0
  "Index of current vterm buffer managed by smulti-vterm")

(defvar svterm-buffers (make-vector svterm-buffer-count nil)
  "vector of lengthvterm buffers")

(defun svterm-advance-index! ()
  (setq svterm-buffer-index
        (mod (+ svterm-buffer-index 1) svterm-buffer-count)))

(defun svterm-next ()
  "Remember current index, loop around `svterm-buffers' and check for a
live buffer. Return the live buffer or nil if none found."
  (let ((start-i vterm-buffer-index))
    (while t
      (svterm-advance-index!)
      (let ((current-item (aref svterm-buffer-count svterm-buffer-index)))
        (cond
         ((= vterm-buffer-index start-i)
          (if (buffer-live-p current-item)
              (return current-item)
            (aset svterm-buffers svterm-buffer-index nil)
            (return nil)))
         ((buffer-live-p current-item) (return current-item))
         (t
          ;; buffer is neither live nor did we loop around
          (aset svterm-buffers svterm-buffer-index nil)))))))

(defun list-rotate-clockwise (lst)
  (append (cdr lst) (list (car lst))))

(defun list-rotate-counter-clockwise (lst)
  (cons (car (last lst)) (butlast lst)))


(defun smulti-vterm-make-buffer (name)
  ;; make buffer and give it vterm-mode
  ;; give it a random name like *vterm*<5742685>
  ;; insert it into vterm-buffers
  ;; display it

  ;; or: make vector of bool where nth entry
  ;; is taken (t) or free (nil) of length 20
  ;; when creating a buffer, use these or
  ;; message that the user has lost control of
  ;; his or her life

  ;; but while I'm at it, why not make the vector
  ;; not hold buffers rather than booleans
  ;; slide through them to the left and right
  )

(defun get-next ()
  ;; (car vter-buffers)
  ;; if killed buffer, remove and car again
  ;; until list empty or a live buffer is found
  )

(defun get-prev ()
  ;; like get-next
  )
