;; -*- lexical-binding: t -*-
;; Dired With preview

;; ========================================
;;  Full Path (1)         |   Info (2)    |
;; -----------------------+               |
;;           |            +---------------+
;;   Parent  |            |               |
;;   Dir     |  Current   |   Preview     |
;;   (5)     |  Dir       |   (4)         |
;;           |  (3)       |               |
;;           |            |               |
;;           |            |               |
;; ========================================


;;; logs
;; show dired in directory and parent window -- finished in 20220920
;; hide the first three line of dired first three lines
;; decrease the path and info window size
;; highlight the dired files
;; preview dispatcher
;; short cut key minor mode

(require 'dired)
(require 'dired-x)

;; Customizations
(defgroup dired-pdp nil
  "This package is used for browsing the files via dired,
with preview"
  :group 'convenience)

(defcustom dired-pdp-dired-icons
  '((rich-text . "")
    (book . "")
    (code . "")
    (file . "")
    (database . "")
    (config . "")
    (compress . "")
    (folder . "")
    (image . "")
    (mindmap . "")
    (video . "")
    (audio . "")
    (screct . ""))
  "")

(define-button-type 'dired-pdp-button
  'action nil
  'filename nil
  'follow-link t
  'use-window nil
  'help-echo
  "Dired-Pdp button which should have a box")

(defcustom dired3-preview-file-variables
  '((inhibit-message . t)
    (enable-dir-local-variables)
    (enable-local-variables . :safe)
    (non-essential . t)
    (delay-mode-hooks . t))
  "")

(defvar dired-pdp-box 'dired-pdp-box "Face name to use for strong sub title.")
(defface dired-pdp-box
  '((t :box t :extent t))
  "Modeline face just one thin bar."
  :group 'dired-pdp)

(defvar dired-preview-buffer-prefix "*dired-pdp-preview-"
  "we will preview a file with this prefix.")

(defvar dired-pdp-path-window nil
  "The window on the top window, which shows current path.")

(defvar dired-pdp-parent-window nil
  "The window on the left window, which shows the files up level.")

(defvar dired-pdp-directory-window nil
  "The window on the center window, which shows current dir.")

(defvar dired-pdp-info-window nil
  "The window on the bottom-right window, which shows the preview file info.")

(defvar dired-pdp-preview-window nil
  "The window on the right window, which shows the preview file.")

(defvar dired-pdp-current-filename nil
  "The file is previewing now.")

(defvar dired-pdp-last-filename nil
  "The file is previewing now.")

(defvar dired-pdp-cancel-change-magic nil
  "The var will prevent change the change once.")

(defun dired-pdp--make-icon-variables ()
  (dolist (e dired-pdp-dired-icons)
    (let* ((name (format "dired-pdp--icon-%s" (car e)))
           (s (make-symbol name)))
      (set s (cdr e))
      (intern name))))

(defun dired-pdp--build-windows ()
  "This function is used to build windows mentioned above, which are in a atomic window."
  (when-let* ((tmp-window (selected-window))
              (info-window (split-window tmp-window nil 'right nil))
              (path-window (split-window info-window nil 'up nil))
              (atomic-window (window-make-atom (window-parent path-window)))
              (tmp-buffer-name "*TempDired-Pdp*")
              (tmp-buffer (get-buffer-create tmp-buffer-name))
              (directory-window (display-buffer-in-atom-window
                                 tmp-buffer
                                 `((window . ,info-window) (side . left))))
              (parent-window (display-buffer-in-atom-window
                              tmp-buffer
                              `((window . ,directory-window) (side . left))))
              (preview-window (display-buffer-in-atom-window
                               tmp-buffer
                               `((window . ,info-window) (side . left)))))
    (setq dired-pdp-path-window       path-window
          dired-pdp-parent-window     parent-window
          dired-pdp-directory-window  directory-window
          dired-pdp-info-window       info-window
          dired-pdp-preview-window    preview-window)
    (dired-pdp--get-parent-own-buffer)
    (dired-pdp--get-directory-own-buffer)
    (dired-pdp--get-preview-own-buffer)
    (kill-buffer tmp-buffer)
    (delete-window tmp-window)))

(defun dired-pdp--reset-window-sizes ()
  (let* ((total-width (window-pixel-width (window-atom-root dired-pdp-directory-window)))
         (parent-width    (/ (* total-width 18) 100))
         (directory-width (/ (* total-width 18) 100))
         (preview-width   (/ (* total-width 40) 100))
         (info-width      (/ (* total-width 24) 100))
         (f (make-symbol "dired-pdp--resize-by-right")))
    (setf f (lambda (window width)
              (let* ((old-width (window-width window t))
                     (delta (- width old-width)))
                (adjust-window-trailing-edge window delta t t))))
    (funcall f dired-pdp-parent-window parent-width)
    (funcall f dired-pdp-directory-window directory-width)
    (funcall f dired-pdp-preview-window preview-width)))

(defun dired-pdp--get-directory-own-buffer ()
  (dired-pdp-get-or-set-window-buffer dired-pdp-directory-window "*dired-pdp-directory*"))

(defun dired-pdp--get-parent-own-buffer ()
  (dired-pdp-get-or-set-window-buffer dired-pdp-parent-window "*dired-pdp-parent*"))

(defun dired-pdp--get-preview-own-buffer ()
  (dired-pdp-get-or-set-window-buffer dired-pdp-preview-window "*dired-pdp-preview*"))

(defun dired-pdp--get-info-own-buffer ()
  (dired-pdp-get-or-set-window-buffer dired-pdp-info-window "*dired-pdp-info*"))

(defun dired-pdp--get-path-own-buffer ()
  (dired-pdp-get-or-set-window-buffer dired-pdp-path-window "*dired-pdp-path*"))

(defun dired-pdp-get-or-set-window-buffer (window buffer-name)
  (let* ((dired-pdp-buffer (window-parameter window 'dired-pdp-buffer)))
    (if (and dired-pdp-buffer (buffer-live-p dired-pdp-buffer))
        dired-pdp-buffer
      (when (buffer-live-p (get-buffer buffer-name))
        (kill-buffer (get-buffer buffer-name)))
      (setq dired-pdp-buffer (generate-new-buffer buffer-name))
      (with-current-buffer dired-pdp-buffer (dired-pdp-mode))
      (set-window-parameter window 'dired-pdp-buffer dired-pdp-buffer))
    dired-pdp-buffer))

(defun dired-pdp-update-parent-window (item-path &optional init)
  (dired-pdp-ignore-errors
   dired-pdp-parent-window
   (if (string= item-path "/")
       (dired-pdp-rewrite-buffer-and-switch dired-pdp-parent-window "")
     (dired-pdp-update-dired-window 'dired-pdp-parent-window item-path init)
     (dired-pdp-clear-invisiable-buffers))))

(defun dired-pdp-update-directory-window (item-path &optional init)
  (dired-pdp-ignore-errors
   dired-pdp-directory-window
   (dired-pdp-update-dired-window 'dired-pdp-directory-window item-path init)))

(defun dired-pdp-hl-current-dired-line (window)
  (let ((cur-overlay (window-parameter window 'hl-overlay)))
    (if cur-overlay
        (move-overlay cur-overlay (line-beginning-position)
                      (1+ (line-end-position)) (window-buffer window))
      (setq cur-overlay (make-overlay (line-beginning-position) (1+ (line-end-position))))
      (overlay-put cur-overlay 'face '(:inherit highlight :extend t))
      (set-window-parameter window 'hl-overlay cur-overlay)
      (overlay-put cur-overlay 'window window))))

(defun dired-pdp-set-buffer-face-mode (face)
  (when face
    (setq-local buffer-face-mode-face face)
    (buffer-face-mode)))


(defun dired-pdp-update-dired-window (window-name filename &optional init)
  (when-let* ((window (symbol-value window-name)))
    (with-selected-window window
      (let* ((init (or init (not (eq major-mode 'dired-mode))))
             (old-path (if (eq major-mode 'dired-mode) (dired-get-filename nil t) nil))
             (old-dir (dired-pdp-get-dir-by-item old-path))
             (new-dir (dired-pdp-get-dir-by-item filename)))
        (unless (string= old-path filename)
          (dired-jump nil filename)
          (dired-pdp-hl-current-dired-line window)
          (recenter))
        (unless (string= old-dir new-dir)
          (dired-hide-details-mode)
          (dired-omit-mode)
          (dired-pdp-mode)
          (dired-pdp-update-dired-mode-line)
          (dired-pdp-hide-dired-header)
          (dired-pdp-add-icons-for-dired)
          (dired-pdp-set-buffer-face-mode ibuffer-sidebar-face))))))

(defun dired-pdp-get-dir-by-item (str)
  (cond ((not str) "")
        ((string-match "^/[^/]*$" str) "/")
        (t (replace-regexp-in-string "/[^/]*$" "" str))))

(defun dired-pdp-as-item-name (filename)
  (if (file-directory-p filename)
      (file-directory-name filename)
    filename))

(defun dired-pdp-update-path-window (&optional filename)
  (when-let* ((window dired-pdp-path-window)
              (buffer (dired-pdp--get-path-own-buffer))
              (path (if filename filename dired-pdp-current-filename)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert path)
      (goto-char (point-min))
      (while (re-search-forward "\\([^/]+\\)/" (line-end-position) t)
        (let* ((name (match-string 1))
               (mbegin (match-beginning 1))
               (mend (match-end 1))
               (full (buffer-substring-no-properties (point-min) mend)))
          (make-text-button mbegin mend
                            :type 'dired-pdp-button
                            'action 'dired-pdp-change-directory-by-button
                            'filename full)))
      (goto-char (point-min))
      (insert " ")
      (goto-char (point-max))
      (insert " ")
      (put-text-property (point-min) (point-max) 'face 'dired-pdp-box)
      (setq buffer-read-only t))))

(defun dired-pdp-setup-path-window ()
  (let* ((buffer (dired-pdp--get-path-own-buffer))
         (window-safe-min-height 0)
         (window-min-height 0))
    (with-selected-window dired-pdp-path-window
      (switch-to-buffer buffer)
      (add-hook 'window-buffer-change-functions
                (lambda (&rest _)
                  (when (equal (window-buffer) dired-pdp-path-window)
                    (switch-to-buffer buffer))))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (fit-window-to-buffer)
        (setq-local truncate-lines t)
        (dired-pdp-set-path-keymap)
        (setq mode-line-format nil)
        (dired-pdp-mode)
        (dired-pdp-set-buffer-face-mode ibuffer-sidebar-face))
      (setq window-size-fixed 'height))
    (add-hook 'window-state-change-hook
              (lambda ()
                (when (window-live-p dired-pdp-path-window)
                  (dired-pdp-update-path-window))))))

(defun dired-pdp-setup-info-window ()
  (with-current-buffer (dired-pdp--get-info-own-buffer)
    (dired-pdp-set-mode-line-face nil nil)
    (setq mode-line-format " ")
    (dired-pdp-set-buffer-face-mode ibuffer-sidebar-face)))

(defun dired-pdp--get-fonticon-by-extension (extenstion)
  (let ((image '("png" "jpg" "jpeg" "bmp" "svg" "xmp" "webp" "xpm"))
        (video '("mp4" "mkv" "avi" "flv" "wmv" "webm" "mov"))
        (audio '("mp3" "ape" "flav" "m4a" "aac" "wav" "ogg" "opus")))

    ))

(defun dired-pdp-preview (filename)
  (let* ((file-type (dired-pdp-get-file-type filename t))
         (file-attrs (file-attributes filename)))
    (dired-pdp--update-info-window file-type file-attrs)
    (dired-pdp-ignore-errors
     dired-pdp-preview-window
     (with-selected-window dired-pdp-preview-window
       (let* ((preview-buffer (dired-pdp-get-preview-buffer filename))
              (buffer-name (buffer-name preview-buffer)))
         (with-current-buffer preview-buffer
           (dired-pdp-set-mode-line-face nil nil)
           (setq mode-line-format `(" [" ,mode-name "] (%l, %C)")))
         (switch-to-buffer preview-buffer))))))


(defun dired3--find-file-temporarily (name)
  "Open file NAME temporarily for preview."
  ;; file-attributes may throw permission denied error
  (when-let* ((attrs (ignore-errors (file-attributes name)))
              (size (file-attribute-size attrs)))
    (if (> size 10485760)
        (progn
          (message "File `%s' (%s) is too large for preview"
                   name (file-size-human-readable size))
          nil)
      (let* ((vars (delq nil
                         (mapcar
                          (pcase-lambda (`(,k . ,v))
                            (if (boundp k)
                                (list k v (default-value k) (symbol-value k))
                              (message "consult-preview-variables: The variable `%s' is not bound" k)
                              nil))
                          dired3-preview-file-variables)))
             (buf (unwind-protect
                      (progn
                        (pcase-dolist (`(,k ,v . ,_) vars)
                          (set-default k v)
                          (set k v))
                        (find-file-noselect name 'nowarn (> size 524288)))
                    (pcase-dolist (`(,k ,_ ,d ,v) vars)
                      (set-default k d)
                      (set k v)))))
        (cond
         ((and (> size 524288)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\0" nil 'noerror))))
          (kill-buffer buf)
          (message "Binary file `%s' not previewed literally" name)
          nil)
         ((ignore-errors (buffer-local-value 'so-long-detected-p buf))
          (kill-buffer buf)
          (message "File `%s' with long lines not previewed" name)
          nil)
         (t buf))))))

(defun dired-pdp-get-preview-buffer (filename)
  (let* ((vc-follow-symlinks nil)
         (buffer (cond ((file-directory-p filename)
                        (dired-pdp-get-preview-buffer-directory filename))
                       (t (dired3--find-file-temporarily filename))))
         (buffer-name (buffer-name buffer)))
    (with-current-buffer buffer
      (unless (string-prefix-p dired-preview-buffer-prefix buffer-name)
        (setq buffer-name (concat dired-preview-buffer-prefix buffer-name "*"))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))
        (rename-buffer buffer-name)))
    buffer))

(defun dired-pdp-get-preview-buffer-directory (filename)
  (let* ((output (shell-command-to-string
                  (format "ls -lh %s --time-style='+%%y-%%m-%%d %%H:%%M:%%S'"
                          (prin1-to-string filename))))
         (buffer-name (replace-regexp-in-string "[^a-z0-9A-Z-]" "_" filename))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (setq buffer-read-only t))
    buffer))

(defun dired-pdp-get-file-type (filename &optional brief)
  (let* ((file-type (dired-show-file-type filename)))
    (if brief (replace-regexp-in-string (concat (regexp-quote filename) ": ?") "" file-type)
      file-type)))

(defun dired-pdp-change-directory-by-button (&optional button)
  (let ((filename (button-get button 'filename)))
    (setq filename (dired-pdp-append-slash-for-dir filename))
    (dired-pdp-update-current-filename filename)))

(defun dired-pdp-setup-preview-window ()
  (with-selected-window dired-pdp-preview-window
    (add-hook 'window-buffer-change-functions
              (lambda (&rest _)
                (set-window-margins dired-pdp-preview-window 4 4)))))

;;;###autoload
(defun dired-pdp (filename)
  (interactive "f")
  (when-let* ((filename (if (file-exists-p filename) filename nil))
              (dirpath (dired-pdp-get-dir-by-item filename)))
    (setq dired-pdp-current-filename filename)
    (dired-pdp--build-windows)
    (dired-pdp--reset-window-sizes)
    (dired-pdp-toggle-dired-hooks)
    (dired-pdp-update-directory-window filename t)
    (dired-pdp-update-parent-window dirpath t)
    (dired-pdp-setup-path-window)
    (dired-pdp-setup-info-window)
    (dired-pdp-setup-preview-window)
    (dired-pdp-toggle-idle)
    (select-window dired-pdp-directory-window)))

(defun dired-pdp-toggle-idle ()
  (let* ((fname "dired-pdp-preview-idle-func")
         (f (make-symbol fname)))
    (fset f
          (lambda ()
            (when (window-live-p dired-pdp-preview-window)
              (dired-pdp-refresh))))
    (run-with-idle-timer 0.1 t f)))

(defun dired-pdp-hide-dired-header ()
  (save-excursion
    (goto-char (point-min))
    (let* ((line-end (line-end-position))
           (overlay (make-overlay (point-min) (1+ line-end))))
      (overlay-put overlay 'invisible t))))

(defun dired-pdp-toggle-dired-hooks (&optional remove)
  (let* ((fname "dired-pdp-dired-advice")
         (f (make-symbol fname)))
    (fset f
          (lambda (&rest _)
            (when dired-pdp-mode
              (dired-pdp-hide-dired-header)
              (dired-pdp-add-icons-for-dired))))
    (advice-add 'dired-revert :after f)))

(defun dired-pdp-get-file-count ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp dired-move-to-filename-regexp nil t)
    (count-lines (line-beginning-position) (point-max))))

(defun dired-pdp-set-mode-line-face (up down)
  (face-remap-add-relative
   'mode-line-active
   `(:background ,(face-background 'default)
                 :box nil
                 :overline ,up
                 :underline ,down))
  (face-remap-add-relative
   'mode-line-inactive
   `(:background ,(face-background 'default)
                 :box nil
                 :overline ,up
                 :underline ,down)))

(defun dired-pdp-update-dired-mode-line ()
  (let* ((files-count (dired-pdp-get-file-count))
         (count (format "/%s" files-count)))
    (dired-pdp-set-mode-line-face nil nil)
    (setq-local mode-line-format
                `("["
                  (:eval (format "%s" (1- (string-to-number (format-mode-line "%l")))))
                  ,count "]"))))

(defun dired-pdp-update-current-filename-by-dired ()
  (when (and (eq major-mode 'dired)
             (= (point) (point-max))
             (not (dired-get-filename nil t)))
    (dired-previous-line 1))
  (let ((filename (dired-get-filename nil t)))
    (when filename (dired-pdp-hl-current-dired-line (selected-window)))
    (dired-pdp-update-current-filename filename)))

(defun dired-pdp-update-current-filename (filename &optional refresh)
  (when (and filename (not (string= filename dired-pdp-current-filename)))
    (setq dired-pdp-current-filename (expand-file-name filename))
    (when refresh (dired-pdp-refresh))))

(defun dired-pdp-refresh ()
  (let ((filename dired-pdp-current-filename))
    (unless (string= filename dired-pdp-last-filename)
      (dired-pdp-update-directory-window filename)
      (with-selected-window dired-pdp-directory-window
        (with-current-buffer (window-buffer)
          (when-let ((go! (and (string-suffix-p "/" filename)
                               (not (string= "/" filename))))
                     (f2 (dired-get-filename nil t)))
            (setq filename f2)
            (dired-pdp-update-directory-window filename))))
      (dired-pdp-update-path-window filename)
      (dired-pdp-update-parent-window
       (dired-pdp-get-dir-by-item filename))
      (dired-pdp-preview filename)
      (dired-pdp-update-current-filename filename)
      (setq dired-pdp-last-filename filename))))

(defun dired-pdp-rewrite-buffer-and-switch (window str)
  (if-let* ((_ (window-live-p window))
            (buffer (window-parameter window 'dired-pdp-buffer))
            (read-only t))
      (progn
        (with-current-buffer buffer
          (setq read-only buffer-read-only)
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert str)
          (setq buffer-read-only read-only))
        (with-selected-window window
          (switch-to-buffer buffer)))
    (message "dired-pdp: do not find buffer: %s" str)))

(defun dired-pdp-clear-invisiable-buffers ()
  (let ((visiable-buffers (mapcar 'window-buffer (window-list))))
    (dolist (buffer (buffer-list))
      (when (and (not (member buffer visiable-buffers))
                 (string-prefix-p dired-preview-buffer-prefix (buffer-name buffer)))
        (kill-buffer buffer)))))

(defun dired-pdp-set-path-keymap ()
  (keymap-local-set "<down>"
                    (lambda ()
                      (interactive)
                      (select-window dired-pdp-directory-window))))

(defun dired-pdp-goto-parent ()
  (interactive)
  (if (string-match "^/[^/]*$" dired-pdp-current-filename)
      (dired-pdp-update-current-filename dired-pdp-current-filename)
    (let* ((directory (dired-pdp-get-dir-by-item dired-pdp-current-filename)))
      (dired-pdp-update-current-filename directory))))

(defun dired-pdp-append-slash-for-dir (filename)
  (if (and (file-directory-p filename)
           (not (string-suffix-p "/" filename)))
      (concat filename "/")
    filename))

(defun dired-pdp-goto-current ()
  (interactive)
  (let* ((file (dired-pdp-append-slash-for-dir dired-pdp-current-filename)))
    (cond ((file-directory-p file)
           (dired-pdp-update-current-filename
            (dired-pdp-append-slash-for-dir file)))
          (t (message "This is a file: %s" file)))))

(defun dired-pdp-set-common-keymap (&optional buffer)
  (let ((f (make-symbol "dired-pdp-select-window-keybinding"))
        (buffer (if buffer buffer (window-buffer))))
    (setf f
          (lambda (key window)
            (interactive)
            (keymap-local-set
             key
             (lambda ()
               (interactive)
               (when (window-live-p window)
                 (select-window window))))))
    (with-current-buffer buffer
      (funcall f "M-<down>" dired-pdp-directory-window)
      (funcall f "M-," dired-pdp-preview-window)
      (funcall f "M-." dired-pdp-info-window)
      (funcall f "M-<up>" dired-pdp-path-window))))

(defun dired-pdp-set-directory-keymap (&optional buffer)
  (with-current-buffer (if buffer buffer (window-buffer))
    (keymap-local-set "<right>" 'dired-pdp-goto-current)
    (keymap-local-set "<left>" 'dired-pdp-goto-parent)
    (keymap-local-set
     "<up>"
     (lambda ()
       (interactive)
       (dired-previous-line 1)
       (dired-pdp-update-current-filename-by-dired)))
    (keymap-local-set
     "<down>"
     (lambda ()
       (interactive)
       (let ((pos (point)))
         (dired-next-line 1)
         (when (and (eobp) (not (dired-get-filename nil t)))
           (goto-char pos)))
       (dired-pdp-update-current-filename-by-dired)))))

(defun dired-pdp-add-icons-for-dired ()
  (save-excursion
    (let ((mimes (dired-pdp--build-mimes default-directory))
          (old-overlays (cl-remove-if-not
                         (lambda (ov)
                           (overlay-get ov 'dired-pdp-icon))
                         (overlays-in (point-min) (point-max)))))
      (save-restriction
        (widen)
        (mapc #'delete-overlay old-overlays))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((find-file! (dired-move-to-filename nil))
               (file (dired-get-filename nil t))
               (overlay (make-overlay (1- (point)) (point)))
               (info (assoc file mimes))
               (icon (nth 3 info)))
          (put-text-property (point) (point) 'dired-pdp-file-info info)
          (overlay-put overlay 'dired-pdp-icon (concat icon " "))
          (overlay-put overlay 'after-string (concat icon " ")))
        (forward-line 1)))))

(define-minor-mode dired-pdp-mode
  ""
  :lighter dired-pdp
  :group dired-pdp
  (let* ((window (selected-window))
         (buffer (window-buffer window)))
    (setq cursor-in-non-selected-windows nil)
    (dired-pdp-set-common-keymap)
    (cond ((equal window dired-pdp-path-window)
           (dired-pdp-set-path-keymap))
          ((equal window dired-pdp-directory-window)
           (dired-pdp-set-directory-keymap buffer))
          (t (message "unmapped window")))))

(defmacro dired-pdp-ignore-errors (window &rest body)
  (declare (debug t) (indent 0))
  `(condition-case err
       (progn ,@body)
     (error
      (dired-pdp-rewrite-buffer-and-switch
       ,window (format "Dired-Pdp-Error: %S" err)))))

;;; Info Window Part
(defun dired-pdp--get-icon-by-mime (mime)
  (cond ( ;directory
         (string-suffix-p "directory" mime)
         "")
        ( ; audio
         (string-prefix-p "audio" mime)
         "")
        ( ; video
         (string-prefix-p "video" mime)
         "")
        ( ; image
         (string-prefix-p "image" mime)
         "")
        ( ; text
         (string-prefix-p "text" mime)
         "")
        ( ; compress
         (string-match-p "zip" mime)
         "")
        (t "")))

(defun dired-pdp--build-mimes (directory)
  (let* ((full-directory (prin1-to-string (expand-file-name (directory-file-name directory))))
         (program (list "file" nil t nil "-i"))
         (files (directory-files default-directory t nil t))
         (process-file-side-effects)
         mimes)
    (with-temp-buffer
      (apply 'process-file (append program files))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\):[ ]*\\([^ ]*/[^ ]*\\); charset=\\([^ ]*\\)$" nil t)
        (let (mtype mencoding filename)
          (setq filename (match-string 1))
          (setq mtype (match-string 2))
          (setq mencoding (match-string 3))
          (setq icon (dired-pdp--get-icon-by-mime mtype))
          (push (list filename mtype mencoding icon) mimes))))
    mimes))

(defun dired-pdp--line-width-here (&optional pos)
  (car (window-text-pixel-size nil (line-beginning-position) (or pos (point)) t)))

(defun dired-pdp--insert-head-info (icon desc window-width)
  (let ((icon-width 0)
        (space-width 0)
        first-space last-word width)
    (goto-char (point-min))
    (when icon
      (insert icon)
      (setq icon-width (dired-pdp--line-width-here))
      (insert " ")
      (setq space-width (- (dired-pdp--line-width-here) icon-width)))
    (setq first-space (point))
    (insert desc)
    (setq width (if icon (dired-pdp--line-width-here) (1+ window-width)))
    (newline)
    (if (<= width window-width)
        (when-let* ((_ (> space-width 0))
                    (delta (- window-width width)))
          (goto-char first-space)
          (insert (make-string (/ delta space-width) ? )))
      (let* ((prefix-width (+ icon-width space-width))
             (prefix (if (> space-width 0)
                         (make-string (/ prefix-width space-width) ? )
                       " "))
             (word-count 0)
             (last-word first-space))
        (goto-char last-word)
        (while (not (eolp))
          (setq last-word (point))
          (forward-word)
          (setq word-count (1+ word-count))
          (setq width (dired-pdp--line-width-here))
          (when (>= width window-width)
            (when (> word-count 1) (goto-char last-word))
            (newline)
            (when (looking-at " ") (delete-char 1))
            (insert prefix)
            (setq word-count 0)))))))

(defun dired-pdp--update-info-window (file-type file-attrs)
  (dired-pdp-ignore-errors
   dired-pdp-info-window
   (with-selected-window dired-pdp-info-window
     (let* ((window-pixel-width (window-body-width nil t))
            (info-buffer (dired-pdp--get-info-own-buffer)))
       (switch-to-buffer info-buffer)
       (with-current-buffer info-buffer
         (setq truncate-lines t)
         (erase-buffer)
         (dired-pdp--insert-preview-common-info
          window-pixel-width file-type file-attrs)
         (goto-char (point-min)))))))

(defun dired-pdp--insert-info-center-icon (icon window-pixel-width)
  (let ((icon (propertize (if icon icon "")
                          'face
                          '(:height 6.0)))
        icon-width space-width pos icon-pos half-count)
    (goto-char (point-min))
    (setq pos (point))
    (insert icon)
    (setq icon-width (dired-pdp--line-width-here))
    (newline)
    (goto-char pos)
    (insert " ")
    (setq space-width (dired-pdp--line-width-here)
          half-count (/ (- window-pixel-width icon-width) 2 space-width))
    (message "centericon %s %s %s %s" window-pixel-width half-count space-width icon-width)
    (insert (make-string half-count ? ))
    (goto-char (line-end-position))))

(defun dired-pdp--insert-preview-common-info
    (window-pixel-width file-type file-attrs &optional icon)
  (dired-pdp--insert-head-info "" (user-login-name (file-attribute-user-id file-attrs)) window-pixel-width)
  (dired-pdp--insert-head-info "" (group-name (file-attribute-group-id file-attrs)) window-pixel-width)
  (dired-pdp--insert-head-info "" (file-attribute-modes file-attrs) window-pixel-width)
  (dired-pdp--insert-head-info "" (file-size-human-readable (file-attribute-size file-attrs))
                               window-pixel-width)
  (dired-pdp--insert-head-info "" (format-time-string
                                    "%y-%m-%d %H:%M:%S"
                                    (file-attribute-modification-time file-attrs))
                               window-pixel-width)
  (goto-char (point-min))
  (newline)
  (dired-pdp--insert-head-info nil file-type window-pixel-width)
  (goto-char (point-min))
  (newline)
  (dired-pdp--insert-info-center-icon icon window-pixel-width))

(provide 'dired-pdp)
