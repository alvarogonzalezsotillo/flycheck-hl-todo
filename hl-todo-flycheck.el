;;; hl-todo-flycheck.el --- Display hl-todo keywords in flycheck  -*- lexical-binding: t; -*-

;; Author: Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; Homepage: https://github.com/alvarogonzalezsotillo/hl-todo-flycheck
;; Package-Requires: ((emacs "25.1") (hl-todo) (flycheck))
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; GNU General Public License v3.0. See COPYING for details.

;;; Commentary:

;; 
;;
;; Quick start:
;;
;; Configure `hl-todo' and `flycheck'.
;; Invoke `hl-todo-flycheck-enable' and open flyckeck error list.

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
  (funcall
   callback 'finished
   (mapcar (lambda (pos-msg-id)
             (let ((pos (nth 0 pos-msg-id))
                   (msg (nth 1 pos-msg-id))
                   (id  (nth 2 pos-msg-id)))
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


(defgroup hl-todo-flycheck-group nil
  "Integration of hl-todo and flycheck."
  :group 'convenience)

(defcustom hl-todo-flycheck-not-chained-checkers '()
  "List of checkers to not be augmented with hl-todo."
  :type 'list)

;; Create hl-todo checker
(flycheck-define-generic-checker 'hl-todo
  "Syntax checker for hl-todo."
  :start '#hl-todo-flycheck--start
  :modes '(text-mode))


;;;###autoload
(defun hl-todo-flycheck-enable ()
  "Install and enable hl-todo-flycheck."
  (interactive)

  ;; Register hl-todo checker, and enable it
  (add-to-list 'flycheck-checkers 'hl-todo)
  (setq flycheck-disabled-checkers (delete 'hl-todo flycheck-disabled-checkers))

  ;; Add all modes to hl-todo checker
  (dolist (checker (hl-todo-flycheck--get-all-modes))
    (flycheck-add-mode 'hl-todo checker))
  
  ;; Chain hl-todo checker to all existing checkers, except disabled modes, and self
  (dolist (checker flycheck-checkers)
    (unless (or
             (eq checker 'hl-todo)
             (member checker hl-todo-flycheck-not-chained-checkers))
      (flycheck-add-next-checker checker 'hl-todo t)))
  
  ;; Force flycheck update
  (flycheck-buffer))

;;;###autoload
(defun hl-todo-flycheck-disable ()
  "Disable hl-todo-flycheck."
  (interactive)
  (add-to-list 'flycheck-disabled-checkers 'hl-todo)

  ;; Force flycheck update
  (flycheck-buffer))

(provide 'hl-todo-flycheck)

;;; hl-todo-flycheck.el ends here


