;;; flycheck-hl-todo.el --- Display hl-todo keywords in flycheck  -*- lexical-binding: t; -*-
;; Author: Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; Homepage: https://github.com/alvarogonzalezsotillo/flycheck-hl-todo
;; Package-Requires: ((emacs "25.1") (hl-todo "1.9.0") (flycheck "0.14"))
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
;; Invoke `flycheck-hl-todo-enable' and open flyckeck error list.

(require 'flycheck)
(require 'hl-todo)

;;; Code:

;; PROMT: elisp function that receives a regex and returns a list of
;; line numbers where the regex matches the current buffer
(defun flycheck-hl-todo--occur-to-error (&optional buffer regex)
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
                     (id (thing-at-point 'word))
                     (kk (message "id:%s" id))
                     (bol (line-beginning-position))
                     (eol (line-end-position))
                     (line-at-point (buffer-substring bol eol))
                     (msg (substring line-at-point (string-match regex line-at-point))))
                (push (list pos msg id) occurrences)))))))
    occurrences))

(defun flycheck-hl-todo--start (checker callback)
  "Start function of hl-todo checker.
CHECKER and CALLBACK are documented in `flycheck-define-generic-checker'."
  (funcall
   callback 'finished
   (mapcar (lambda (pos-msg-id)
             (let ((pos (nth 0 pos-msg-id))
                   (msg (nth 1 pos-msg-id))
                   (id  (nth 2 pos-msg-id)))
               (flycheck-error-new-at-pos pos 'info msg :id id :checker checker)))
           (flycheck-hl-todo--occur-to-error))))

(defun flycheck-hl-todo--get-all-modes-of-checkers ()
  "Computes all modes referenced by existing checkers."
  (seq-uniq
   (mapcan (lambda (checker)
             (let* ((modes (flycheck-checker-get checker 'modes))
                    ;; Ensure modes is a list
                    (modes (if (listp modes)
                               modes
                             (list modes))))
               ;; Copy the list, to do not modify original list of checker
               (copy-sequence modes)))
           flycheck-checkers)))


(defgroup flycheck-hl-todo-group nil
  "Integration of hl-todo and flycheck."
  :group 'convenience)

(defcustom flycheck-hl-todo-not-chained-checkers '()
  "List of checkers to not be augmented with hl-todo."
  :type '(repeat (symbol :tag "Flycheck checker")))


(defcustom flycheck-hl-todo-extra-modes '()
  "List of additional major modes where hl-todo checker will be registered.
All the modes of the checkers of `flycheck-checkers' will also be registered."
  :type '(repeat (symbol :tag "Mode")))

(defvar flycheck-hl-todo-enabled
  t
  "Buffer local variable to decide if the checker should be run." )

(make-variable-buffer-local 'flycheck-hl-todo-enabled)

(defun flycheck-hl-todo-enabled-p ()
  "Decide if the checker should generate errors."
  flycheck-hl-todo-enabled)

(defun flycheck-hl-todo-enable ()
  "Enable flycheck-hl-todo in the current buffer."
  (interactive)

  (flycheck-hl-todo-setup)
  (setq flycheck-hl-todo-enabled t)

  ;; Force flycheck update
  (flycheck-buffer))

(defun flycheck-hl-todo-disable ()
  "Disable flycheck-hl-todo in the current buffer."
  (interactive)
  (setq flycheck-hl-todo-enabled nil)

  ;; Force flycheck update
  (flycheck-buffer))


;; Create hl-todo checker
(flycheck-define-generic-checker 'hl-todo
  "Syntax checker for hl-todo."
  :start #'flycheck-hl-todo--start
  :predicate #'flycheck-hl-todo-enabled-p
  :modes '(text-mode))

;;;###autoload
(defun flycheck-hl-todo-setup ()
  "Install flycheck-hl-todo, and enable it in the current buffer."
  (interactive)

  ;; Register hl-todo checker
  ;; Add it to the end of the checkers list, it will be invoked via chained checkers
  (add-to-list 'flycheck-checkers 'hl-todo t)

  ;; Add all modes to hl-todo checker
  (dolist (mode (flycheck-hl-todo--get-all-modes-of-checkers))
    (flycheck-add-mode 'hl-todo mode))
  (dolist (mode flycheck-hl-todo-extra-modes)
    (flycheck-add-mode 'hl-todo mode))
  
  ;; Chain hl-todo checker to all existing checkers, except disabled modes, and self
  (dolist (checker flycheck-checkers)
    (unless (or
             (eq checker 'hl-todo)
             (member checker flycheck-hl-todo-not-chained-checkers))
      (flycheck-add-next-checker checker 'hl-todo t))))


(provide 'flycheck-hl-todo)

;;; flycheck-hl-todo.el ends here
