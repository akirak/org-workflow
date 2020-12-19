(require 'dash)

(defgroup org-workflow
  nil
  "FIXME"
  :group 'org)

;;;; Actions
;;;;; Macros
(defmacro org-workflow--with-marker (marker &rest form)
  (declare (indent 1))
  `(progn
     (unless (buffer-live-p (marker-buffer ,marker))
       (error "Marker buffer is not alive %s" (marker-buffer ,marker)))
     ,@form))

(defmacro org-workflow--on-marker-hidden (marker &rest form)
  "Temporarily go to MARKER and run FORM."
  (declare (indent 1))
  `(with-current-buffer (marker-buffer ,marker)
     ;; (org-base-buffer (marker-buffer ,marker))
     (org-with-wide-buffer
      (goto-char ,marker)
      ,@form)))

(defmacro org-workflow--on-marker-show-context (marker &rest form)
  "Jump to MARKER, display the context, and run FORM."
  (declare (indent 1))
  `(progn
     (pop-to-buffer (marker-buffer ,marker))
     (when (or (> ,marker (point-max)) (< ,marker (point-min)))
       (widen))
     (goto-char ,marker)
     (org-show-context)
     ,@form))

(defmacro org-workflow--on-marker-interactively (marker &rest form)
  (declare (indent 1))
  `(save-window-excursion
     (pop-to-buffer-same-window (marker-buffer ,marker))
     (delete-other-windows)
     (save-excursion
       (save-restriction
         (widen)
         (goto-char ,marker)
         (org-narrow-to-subtree)
         ,@form))))

;;;;; Functions for building actions
(defmacro org-workflow--expand-action (dispatch)
  (pcase dispatch
    (`(on-marker :display t . ,exp)
     `(lambda (marker)
        (org-workflow--with-marker marker
          (org-workflow--on-marker-show-context marker ,@exp))))
    (`(on-marker :interactive t . ,exp)
     `(lambda (marker)
        (org-workflow--with-marker marker
          (org-workflow--on-marker-interactively marker ,@exp))))
    (`(on-marker . ,exp)
     `(lambda (marker)
        (org-workflow--with-marker marker
          (org-workflow--on-marker-hidden marker ,@exp))))
    (_
     (error "Unsupported pattern: %s" dispatch))))

(defun org-workflow--compile-action (dispatch)
  (if (functionp dispatch)
      dispatch
    (byte-compile `(org-workflow--expand-action ,dispatch))))

(defun org-workflow--build-ivy-actions (actions)
  (->> actions
       (-map (pcase-lambda (`(_ ,label . ,plist))
               (list (plist-get plist :ivy-key)
                     (org-workflow--compile-action (plist-get plist :dispatch))
                     label)))
       (-filter #'car)))

(defun org-workflow--build-helm-actions (actions)
  (-map (pcase-lambda (`(_ ,label . ,plist))
          (cons label
                (org-workflow--compile-action (plist-get plist :dispatch))))
        actions))

;;;;; Custom variables
(defcustom org-workflow-heading-action-list
  '((goto
     "Show"
     :ivy-key "g"
     :dispatch org-goto-marker-or-bmk)
    (goto-indirect
     "Show in indirect buffer"
     :dispatch (on-marker :display t (org-tree-to-indirect-buffer)))
    (clock-in
     "Clock in"
     :dispatch (on-marker (org-clock-in)))
    (insert-link
     "Insert link to this heading"
     :dispatch org-workflow-insert-link)
    (store-link
     "Store link"
     :dispatch (on-marker
                (call-interactively 'org-store-link))))
  "Alist of actions."
  :type '(repeat (list (symbol :tag "Identifier")
                       (string :tag "Label that describes the action")
                       (plist :inline t
                              :options
                              (((const :tag "Function that dispatches the action on a marker" :dispatch)
                                function)))))
  :set (lambda (sym value)
         (set-default sym value)
         ;; NOTE: For development
         (setq helm-org-headings-actions
               (org-workflow--build-helm-actions value))))

(defconst org-workflow-extra-heading-action-list-1
  '((clock-out
     "Clock out"
     :dispatch (on-marker
                (assert (org-clocking-p))
                (assert (org-workflow-same-heading-p org-clock-marker))
                (org-clock-out)))
    (clock-dwim
     "Clock in/out dwim"
     :dispatch (on-marker
                (if (and (org-clocking-p)
                         (org-workflow-same-heading-p org-clock-marker))
                    (org-clock-out)
                  (org-clock-in))))
    (todo
     "Change the TODO state"
     :dispatch (on-marker
                :interactive t
                (org-todo)))
    (set-tags
     :label "Set tags"
     :dispatch (on-marker
                :interactive t
                (org-set-tags-command)))
    (set-property
     :label "Set a property"
     :dispatch (on-marker
                :interactive t
                (call-interactively 'org-set-property))))
  "Actions that were available in `counsel-org-clock' but I want to remove.")

;;;; Utilities
(defun org-workflow-same-heading-p (marker1 &optional marker2)
  "Return non-nil if MARKER1 and MARKER2 point to the same Org heading."
  (let ((marker (or marker2 (point-marker))))
    (and (eql (org-base-buffer (marker-buffer marker1))
              (org-base-buffer (marker-buffer marker)))
         (or (eq (marker-position marker1)
                 (marker-position marker))
             (cl-macrolet ((heading-pos (m)
                                        (with-current-buffer (org-base-buffer (marker-buffer ,m))
                                          (goto-char (marker-position ,m))
                                          (save-excursion
                                            (org-back-to-heading)
                                            (point)))))
               (eq (heading-pos marker1)
                   (heading-pos marker)))))))

(defun org-workflow-insert-link (marker)
  "In Org, insert a link to the heading at MARKER."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-workflow--with-marker marker
    (org-workflow--on-marker-hidden marker
      (call-interactively #'org-store-link)))
  (call-interactively #'org-insert-last-stored-link))

(provide 'org-workflow)
