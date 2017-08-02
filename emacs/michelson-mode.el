;; Major mode for editing Michelson smart contracts.

(defvar michelson-mode-hook nil)

(defgroup michelson nil
  "Major mode for editing Michelson smart contracts."
  :prefix "michelson-"
  :group 'languages)

(defgroup michelson-options nil
  "General options for Michelson mode."
  :prefix "michelson-"
  :group 'michelson)

(defcustom michelson-mode-client-command "tezos-client"
  "Path to the `tezos-client' binary."
  :type 'string
  :group 'michelson-options)

(defcustom michelson-alphanet nil
  "Is the client command currently using the alphanet.sh script?"
  :type 'boolean
  :group 'michelson-options)

(defgroup michelson-faces nil
  "Font lock faces for Michelson mode."
  :prefix "michelson-"
  :group 'michelson
  :group 'faces)

(defvar michelson-face-instruction
  'michelson-face-instruction
  "Face name for Michelson instructions.")
(defface michelson-face-instruction
  '((t (:inherit 'font-lock-keyword-face)))
  "Face for Michelson instructions."
  :group 'michelson-faces)

(defvar michelson-face-type
  'michelson-face-type
  "Face name for Michelson types.")
(defface michelson-face-type
   '((t (:inherit 'font-lock-type-face)))
   "Face for Michelson types."
   :group 'michelson-faces)

(defvar michelson-face-constant
  'michelson-face-constant
  "Face name for Michelson constants.")
(defface michelson-face-constant
   '((t (:inherit 'font-lock-constant-face)))
   "Face for Michelson constants."
   :group 'michelson-faces)

(defvar michelson-face-instruction
  'michelson-face-instruction
  "Face name for Michelson instructions.")
(defface michelson-face-annotation
   '((t (:inherit 'font-lock-string-face)))
   "Face for Michelson annotations."
   :group 'michelson-faces)

(defvar michelson-face-comment
  'michelson-face-comment
  "Face name for Michelson comments.")
(defface michelson-face-comment
   '((t (:inherit 'font-lock-comment-face)))
   "Face for Michelson comments."
   :group 'michelson-faces)

(defvar michelson-face-error
  'michelson-face-error
  "Face name for Michelson comments.")
(defface michelson-face-error
  '(( ((class color) (background light)) (:background "MistyRose") )
    ( ((class color) (background dark)) (:background "DarkRed") ))
   "Face for Michelson annotations."
   :group 'michelson-faces)

(defun michelson-customize-options ()
  "Open the general customization group for Michelson mode."
  (interactive)
  (customize-group-other-window `michelson-options))

(defun michelson-customize-faces ()
  "Open the face customization group for Michelson mode."
  (interactive)
  (customize-group-other-window `michelson-faces))

(defconst michelson-mode-map
  (let ((michelson-mode-map (make-sparse-keymap)))
    ;; menu
    (define-key michelson-mode-map
      [menu-bar michelson-menu]
      (cons "Michelson" (make-sparse-keymap "michelson-menu")))
    (define-key michelson-mode-map
      [menu-bar michelson-menu faces]
      (cons "Display options group" 'michelson-customize-faces))
    (define-key michelson-mode-map
      [menu-bar michelson-menu options]
      (cons "General options group" 'michelson-customize-options))
    (define-key michelson-mode-map
      [menu-bar michelson-menu separator]
      '(menu-item "--"))
    (define-key michelson-mode-map
      [menu-bar michelson-menu what]
      (cons "What's under the cursor?" 'michelson-type-at-point))
    ;; keys
    (define-key michelson-mode-map
      (kbd "C-j") 'newline-and-indent)
    (define-key michelson-mode-map
      (kbd "TAB") 'indent-for-tab-command)
    (define-key michelson-mode-map
      (kbd "<f1>") 'michelson-type-at-point)
    michelson-mode-map))

(defun michelson-font-lock-syntactic-face-function (s)
  (cond ((nth 3 s) 'font-lock-constant-face)
        (t 'michelson-face-comment)))

(defconst michelson-font-lock-defaults
  (list
   (list
    '("\\<[0-9]+\\>" . michelson-face-constant)
    '("\\<[A-Z][a-z_0-9]+\\>" . michelson-face-constant)
    '("\\<[A-Z][A-Z_0-9]*\\>" . michelson-face-instruction)
    '("\\<\$[A-Za-z-_0-9]*\\>" . michelson-face-annotation)
    '("\\<[a-z][a-z_0-9]*\\>" . michelson-face-type))
   nil nil nil nil
   '(font-lock-syntactic-face-function . michelson-font-lock-syntactic-face-function)))

(defconst michelson-mode-syntax-table
  (let ((michelson-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" michelson-mode-syntax-table)
    (modify-syntax-entry ?/ ". 1n4" michelson-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" michelson-mode-syntax-table)
    (modify-syntax-entry ?# "<b" michelson-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" michelson-mode-syntax-table)
    michelson-mode-syntax-table))

(defun in-space ()
  (or (looking-at "[[:space:]\n]")
      (equal (get-text-property (point) 'face)
             'michelson-face-comment)))

(defun michelson-goto-previous-token ()
  (interactive)
  (if (bobp)
      (cons 0 nil)
    (progn
      (backward-char 1)
      (while (and (not (bobp)) (in-space)) (backward-char 1))
      (let ((token-face (get-text-property (point) 'face)))
        (forward-char 1)
        (let ((end-of-token (point)))
          (backward-char 1)
          (unless (looking-at "[{()};]")
            (while (and (not (bobp))
                        (equal (get-text-property (point) 'face) token-face))
              (backward-char 1))
            (when (not (equal (get-text-property (point) 'face) token-face))
              (forward-char 1)))
          (cons (point) (buffer-substring-no-properties (point) end-of-token)))))))

(defun michelson-goto-next-token ()
  (interactive)
  (if (eobp)
      (cons (point) nil)
    (progn
      (while (and (not (eobp)) (in-space)) (forward-char 1))
      (let ((token-face (get-text-property (point) 'face)))
        (let ((start-of-token (point)))
          (if (looking-at "[{()};]")
              (forward-char 1)
            (progn
              (while (and (not (eobp))
                          (equal (get-text-property (point) 'face) token-face))
                (forward-char 1))))
          (cons start-of-token (buffer-substring-no-properties start-of-token (point))))))))

(defun michelson-goto-opener ()
  (interactive)
  (let ((paren-level 0))
    (while (and (not (bobp))
                (or (> paren-level 0)
                    (not (looking-at "[{(]"))))
      (cond ((looking-at "[{(]")
             (setq paren-level (- paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (+ paren-level 1))))
      (michelson-goto-previous-token))
    (cons (point)
          (when (looking-at "[{(]")
            (buffer-substring-no-properties (point) (+ (point) 1))))))

(defun michelson-goto-closer ()
  (interactive)
  (let ((paren-level 0) (last-token ""))
    (while (and (not (eobp))
                (or (> paren-level 0)
                    (not (string-match "[)}]" last-token))))
      (cond ((looking-at "[{(]")
             (setq paren-level (+ paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (- paren-level 1))))
      (setq last-token (cdr (michelson-goto-next-token))))
    (cons (point)
          (when (looking-at "[)}]")
            (buffer-substring-no-properties (point) (+ (point) 1))))))

(defun michelson-goto-previous-application-start ()
  (interactive)
  (let ((paren-level 0) (application-start 0))
    (while (and (not (bobp))
                (or (> paren-level 0)
                    (not (looking-at "[{(;]"))))
      (cond ((looking-at "[{(]")
             (setq paren-level (- paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (+ paren-level 1))))
      (setq application-start (point))
      (michelson-goto-previous-token))
    (cons application-start (goto-char application-start))))

(defun michelson-indent ()
  "Indent current line of Michelson code."
  (interactive)
  (let ((new-indentation 0)
        (previous-indentation (current-indentation))
        (previous-column (current-column))
        (current-token
         (save-excursion
           (beginning-of-line 1)
           (michelson-goto-next-token))))
    (save-excursion
      (end-of-line 0)
      (let ((previous-token
             (save-excursion (michelson-goto-previous-token)))
            (previous-opener
             (save-excursion (michelson-goto-opener))))
        (cond ((and (not (cdr previous-opener))
                    (not (cdr previous-token)))
               (setq new-indentation 0))
              ((and (not (cdr previous-opener))
                    (equal (cdr previous-token) ";"))
               (setq new-indentation 0))
              ((not (cdr previous-opener))
               (setq new-indentation 2))
              ((and (equal (cdr current-token) "}")
                    (equal (cdr previous-opener) "{"))
               (goto-char (car previous-opener))
               (setq new-indentation (current-column)))
              ((and (or (equal (cdr previous-token) ";")
                        (equal (cdr previous-token) "{"))
                    (equal (cdr previous-opener) "{"))
               (goto-char (car previous-opener))
               (setq new-indentation (+ (current-column) 2)))
              ((equal (cdr previous-opener) "{")
               (progn
                 (michelson-goto-previous-application-start)
                 (let ((default-param-indentation
                         (+ (current-column) 2))
                       (first-param-point
                        (save-excursion
                          (michelson-goto-next-token)
                          (car (michelson-goto-next-token)))))
                   (if (= first-param-point (car current-token))
                       (setq new-indentation default-param-indentation)
                     (progn
                       (goto-char first-param-point)
                       (setq new-indentation (current-column)))))))
              ((and (equal (cdr current-token) ")")
                    (equal (cdr previous-opener) "("))
               (goto-char (car previous-opener))
               (setq new-indentation (current-column)))
              ((equal (cdr previous-token) "(")
               (goto-char (car previous-token))
               (setq new-indentation (+ (current-column) 1)))
              ((equal (cdr previous-opener) "(")
               (goto-char (car previous-opener))
               (setq new-indentation (+ (current-column) 3))))))
    (indent-line-to new-indentation)
    (beginning-of-line)
    (forward-char
     (+ (- previous-column previous-indentation) new-indentation))
    (when (< (current-column) new-indentation)
      (beginning-of-line)
      (forward-char new-indentation))))

(defun michelson-token-at-point ()
  "Display the token closest to the cursor."
  (interactive)
  (let ((message
         (cdr (save-excursion
                (michelson-goto-next-token)
                (michelson-goto-previous-token)))))
    (display-message-or-buffer message "*Michelson*")))

(defun michelson-type-at-point ()
  "Display the type of the expression under the cursor."
  (interactive)
  (let ((tmp-file (make-temp-file (buffer-name))))
    (write-region (point-min) (point-max) tmp-file nil 'no-message)
    (let* ((command (concat
                     (if michelson-alphanet "ALPHANET_EMACS=true " "")
                     michelson-mode-client-command
                     " typecheck program "
                     (if michelson-alphanet
                         (concat "container:" buffer-file-name)
                       buffer-file-name)
                     " -details -emacs"))
           (stdout
            (shell-command-to-string command))
           (record
            (car (read-from-string stdout)))
           (errors
            (cdr (assoc 'errors record)))
           (types
            (cdr (assoc 'types record)))
           (message ""))
      (delete-file tmp-file)
      (remove-overlays)
      (setq message "")
      (dolist (elt types)
        (if (and (<= (car elt) (point)) (<= (point) (cadr elt))
                 (equal (get-text-property (point) 'face)
                        'michelson-face-instruction))
            (setq message (cadr (cdr elt)))))
      (dolist (elt errors)
        (overlay-put (make-overlay (car elt) (cadr elt)) 'face 'michelson-face-error)
        (if (and (<= (car elt) (point)) (<= (point) (cadr elt)))
            (progn
              (if (string= "" message)
                  nil
                (setq message (concat message "\n")))
              (setq message (concat message (cadr (cdr elt)))))))
      (if (string= message "")
          (setq message "\n  No instruction at point."))
      (let
          ((message-window
            (display-buffer-below-selected
             (get-buffer-create "*Michelson*") nil))
           (lines 0))
        (save-excursion
          (set-buffer "*Michelson*")
          (toggle-read-only nil)
          (erase-buffer)
          (insert message)
          (toggle-read-only t)
          (beginning-of-buffer)
          (while (not (eobp))
            (vertical-motion 1)
            (setq lines (+ 1 lines)))
          (window-resize
           message-window
           (+ (- (max 4 lines)
                 (window-size message-window)) 1)))))))

(defun michelson-update-minibuffer-info ()
  (when (nth 2 michelson-state) (cancel-timer (nth 2 michelson-state)))
  (setf
   (nth 2 michelson-state)
   (run-at-time
    "0.3 sec" nil
    (lambda (buffer)
      (with-current-buffer buffer
        (setf (nth 2 michelson-state) nil)
        (when (not (= (nth 0 michelson-state) (point)))
          (setf (nth 0 michelson-state) (point))
          (michelson-type-at-point))))
    (current-buffer))))

(define-derived-mode michelson-mode prog-mode "Michelson"
  "Major mode for editing Michelson smart contracts."
  (interactive)
  (kill-all-local-variables)
  (use-local-map michelson-mode-map)
  (set-syntax-table michelson-mode-syntax-table)
  (set
   (make-local-variable 'font-lock-defaults)
   michelson-font-lock-defaults)
  (set
   (make-local-variable 'indent-line-function)
   'michelson-indent)
  (set
   (make-local-variable 'indent-for-tab-command)
   'michelson-indent)
  (set
   (make-local-variable 'michelson-state)
   (list 0 0 nil))
  (add-to-list
   (make-local-variable 'pre-command-hook)
   'michelson-update-minibuffer-info)
  (add-to-list
   (make-local-variable 'focus-in-hook)
   'michelson-update-minibuffer-info)
  (setq major-mode 'michelson-mode)
  (setq mode-name "Michelson")
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq buffer-file-coding-system 'utf-8-unix)
  (run-hooks 'michelson-mode-hook))

(add-to-list 'auto-mode-alist '("\\.tz\\'" . michelson-mode))
(add-to-list 'auto-mode-alist '("\\.tez\\'" . michelson-mode))

(provide 'michelson-mode)
