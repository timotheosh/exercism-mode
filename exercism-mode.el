;;; exercism-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Tim Hawes
;;
;; Author: Tim Hawes <https://github.com/timotheosh>
;; Maintainer: Tim Hawes <trhawes@gmail.com>
;; Created: May 11, 2021
;; Modified: May 13, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/thawes/exercism-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides some functionality while working on programming problems found on
;; https://exercism.io. Rather than try to use an undocumented API, this uses
;; the exercism cli tool to run tasks.
;;
;; This is a work in progress.
;;
;;
;;; Code:
(require 'json)
(require 'projectile)

(defun find-exercism-config ()
  "Looks for the common places the exercism config might be in. Returns nil if it doesn't find it."
  (cond ((file-exists-p (expand-file-name "~/snap/exercism/current/.config/exercism/user.json")) (expand-file-name "~/snap/exercism/current/.config/exercism/user.json"))
        ((file-exists-p (expand-file-name "~/.config/exercism/user.json")) (expand-file-name "~/.config/exercism/user.json"))
        ((file-exists-p (expand-file-name "~/.exercism/user.json")) (expand-file-name "~/.exercism/user.json"))))

(defcustom exercism-config-path (find-exercism-config) "Path where the exercism config file can be found.")

(defun read-exercism-config ()
  "Reads the config file for exercism, Returns read data as a hash-table."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file exercism-config-path)))

(defcustom exercism-workspace (gethash "workspace" (read-exercism-config)) "Path for the Exercism workspace.")

(defun exercism ()
  (interactive)
  (dired exercism-workspace))

(defun exercism-find-project-root (&optional dir)
  (let* ((dir (or dir default-directory))
         (exc-dir (length exercism-workspace))
         (buff-dir (length dir)))
    (when (< exc-dir buff-dir)
      (let ((p (substring dir 0 exc-dir)))
        (when (string= exercism-workspace p)
          (let* ((exer-dir (substring dir exc-dir))
                 (dir-list (split-string exer-dir "/" t)))
            (if (or (string= (car dir-list) "users")
                    (string= (car dir-list) "teams"))
                (string-join (list exercism-workspace (nth 0 dir-list) (nth 1 dir-list) (nth 2 dir-list)) "/")
              (string-join (list exercism-workspace (nth 0 dir-list) (nth 1 dir-list)) "/"))))))))


(define-minor-mode exercism-mode
  "Minor mode for exercism."
  :lighter "Exercism"
  :group 'exercism
  ;; Add a project root discovery function for projectile and give it precedence.
  (push 'exercism-find-project-root
        projectile-project-root-functions))

(add-hook 'projectile-mode-hook 'exercism-mode)

(provide 'exercism-mode)
;;; exercism-mode.el ends here
