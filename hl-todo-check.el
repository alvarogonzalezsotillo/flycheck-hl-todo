


;; flycheck-add-next-checker
;; emacs-lisp-checkdoc
;; hl-todo--regexp

;; \\(\\<\\(TODO\\|FIXME\\|DEBUG\\|GOTCHA\\|STUB\\)\\>\\)
;;https://emacs.stackexchange.com/questions/29496/automatically-run-org-lint-through-flycheck


;; (flycheck-define-checker hl-todo-checker

;;   :command ("scalac" "-Ystop-after:parser" source)
;;   :error-patterns
;;     ((error line-start (file-name) ":" line ": error: " (message) line-end))
;;   :modes scala-mode
;;   :next-checkers ((warning . scala-scalastyle)))

(require 'hl-todo)
(require 'flycheck)


;; PROMT: elisp function that receives a regex and returns a list of
;; line numbers where the regex matches the current buffer
(defun hl-todo-flycheck--occur-to-error (&optional buffer regex)
  "Find lines in the current buffer where the given regex matches. Return a list of (position text)"
  (let* ((buffer (or buffer (current-buffer)))
         (regex (or regex (hl-todo--regexp)))
         (occurrences '()))
    (with-current-buffer buffer
      (with-syntax-table hl-todo--syntax-table ;; TODO: from hl-todo-occur, dont know the actual effect
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil)) ;; Only exact case in search
            (while (re-search-forward regex nil t)
              ;;(message "buscando en:%s" (point))

              (let* ((pos (point))
                     (bol (line-beginning-position))
                     (eol (line-end-position))
                     (line-at-point (buffer-substring bol eol))
                     (msg (substring line-at-point (string-match regex line-at-point))))
                (push (list pos msg) occurrences)))))))
    occurrences))

(defun hl-todo-flycheck--start (checker callback)
  ;;(message "hl-todo-flycheck--start")
  (funcall
   callback 'finished
   (mapcar (lambda (pos-and-msg)
             (let ((pos (nth 0 pos-and-msg))
                   (msg (nth 1 pos-and-msg)))
               ;;(message "nuevo error:%s %s" pos msg)
               (flycheck-error-new-at-pos pos 'info msg :checker checker)
               ))
           (hl-todo-flycheck--occur-to-error))))

(defun hl-todo-flycheck--get-all-modes ()
  (interactive)
  (seq-uniq
   (mapcan (lambda (checker) (mapcar #'copy-sequence  (flycheck-checker-get checker 'modes)))
           flycheck-checkers)))

  

(defun hl-todo-flycheck-install ()
  (interactive)
  
  (flycheck-define-generic-checker 'hl-todo
    "Syntax checker for test."
    :start 'hl-todo-flycheck--start
    :modes (hl-todo-flycheck--get-all-modes))

  (add-to-list 'flycheck-checkers 'hl-todo t))



