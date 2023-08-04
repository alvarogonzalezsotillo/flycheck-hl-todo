;;; package --- Summary

;;; Commentary:


;; Based on https://emacs.stackexchange.com/questions/29496/automatically-run-org-lint-through-flycheck
;; TODO: test with flycheck-projectile-list-errors

(require 'flycheck)
(require 'hl-todo)

;;; Code:

;; PROMT: elisp function that receives a regex and returns a list of
;; line numbers where the regex matches the current buffer
(defun hl-todo-flycheck--occur-to-error (&optional buffer regex)
  "Find lines in BUFFER where the given REGEX matches.
Return a list of (position text id)."
  (let* ((buffer (or buffer (current-buffer)))
         (regex (or regex (hl-todo--regexp)))
         (occurrences '()))
    (with-current-buffer buffer
      (with-syntax-table hl-todo--syntax-table ; TODO: from hl-todo-occur, dont know the actual effect
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil)) ; Only exact case in search
            (while (re-search-forward regex nil t)
              ;;(message "buscando en:%s" (point))word

              (let* ((pos (point))
                     (id (thing-at-point 'symbol))
                     (bol (line-beginning-position))
                     (eol (line-end-position))
                     (line-at-point (buffer-substring bol eol))
                     (msg (substring line-at-point (string-match regex line-at-point))))
                (push (list pos msg id) occurrences)))))))
    occurrences))

(defun hl-todo-flycheck--start (checker callback)
  "Start function of hl-todo checker.
CHECKER and CALLBACK are documented in `flycheck-define-generic-checker'."
  ;;(message "hl-todo-flycheck--start")
  (funcall
   callback 'finished
   (mapcar (lambda (pos-msg-id)
             (let ((pos (nth 0 pos-msg-id))
                   (msg (nth 1 pos-msg-id))
                   (id  (nth 2 pos-msg-id)))
               ;;(message "nuevo error:%s %s" pos msg)
               (flycheck-error-new-at-pos pos 'info msg :id id :checker checker)))
           (hl-todo-flycheck--occur-to-error))))

(defun hl-todo-flycheck--get-all-modes ()
  "Computes all modes referenced by existing checkers."
  (seq-uniq
   (mapcan (lambda (checker)
             (let* ((modes (flycheck-checker-get checker 'modes))
                    ;; Ensure modes is a list
                    (modes ( if (listp modes)
                               modes
                             (list modes))))
               ;; Copy the list, to do not modify original list of checker
               (copy-sequence modes)))
           flycheck-checkers)))


;; FIXME: Convert to customizable variable
(defvar hl-todo-flycheck-disabled-modes '())


(defun hl-todo-flycheck-enable ()
  "Install and enable hl-todo-flycheck."
  (interactive)

  ;; Create hl-todo checker
  (flycheck-define-generic-checker 'hl-todo
    "Syntax checker for hl-todo."
    :start 'hl-todo-flycheck--start
    :modes (hl-todo-flycheck--get-all-modes))

  ;; Register hl-todo checker, and enable it
  (add-to-list 'flycheck-checkers 'hl-todo)
  ;;(flycheck-disable-checker 'hl-todo t)
  (setq flycheck-disabled-checkers (delete 'hl-todo flycheck-disabled-checkers))
  
  ;; Chain hl-todo checker to all existing checkers, except disabled modes, and self
  (dolist (checker flycheck-checkers)
    (unless (or
             (eq checker 'hl-todo)
             (member checker hl-todo-flycheck-disabled-modes))
      (flycheck-add-next-checker checker 'hl-todo t)))
  
  ;; Force flycheck update
  (flycheck-buffer))

(defun hl-todo-flycheck-disable ()
  "Disable hl-todo-flycheck."
  (interactive)
  ;;(flycheck-disable-checker 'hl-todo nil)
  (add-to-list 'flycheck-disabled-checkers 'hl-todo)
  (flycheck-buffer))

(provide 'hl-todo-flycheck)

;;; hl-todo-flycheck.el ends here


