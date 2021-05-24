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

(defcustom exercism-config-path
  (find-exercism-config)
  "Path where the exercism config file can be found."
  :group 'exercism-mode
  :type 'string)

(defun read-exercism-config ()
  "Reads the config file for exercism, Returns read data as a hash-table."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file exercism-config-path)))

(defcustom exercism-workspace
  (gethash "workspace" (read-exercism-config))
  "Path for the Exercism workspace."
  :type 'string
  :group 'exercism-mode)

(defun exercism ()
  "Open a dired window at the exercism root directory."
  (interactive)
  (dired exercism-workspace))

(defun exercism-find-project-root (&optional dir)
  "Finds the project root of an exercism.io exercise."
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

(defcustom exercism-cli-path
  (executable-find "exercism")
  "Path for the exercism cli program."
  :group 'exercism-mode
  :type 'string)

(defun exc-process-output (process output)
  "This is a filter for the the run-exercism function. PROCESS is unused. OUTPUT is from the running process."
  (message (string-trim output)))

(defun run-exercism (&rest args)
  "Run the exercism cli tool with ARGS."
  (let ((cmd (push exercism-cli-path args)))
    (make-process
     :name "exercism"
     :buffer "*exercism-cli*"
     :filter #'exc-process-output
     :command cmd)))

(defun exercism-download-exercise (track exercise)
  "Downloads an exercise. TRACK is the exercism track. EXERCISE is the name of the exercism exercise."
  (interactive "sTrack: \nsExercise: ")
  (run-exercism "download"
                "--track" track
                "--exercise" exercise)
  (when (file-directory-p (concat exercism-workspace "/" track "/" exercise))
    (dired (concat exercism-workspace "/" track "/" exercise))))

(defun exercism-submit-exercise (track exercise)
  "Submits an exercise. TRACK is the exercism track. EXERCISE is the name of the exercism exercise."
  (interactive "sTrack: \nsExercise: ")
  (run-exercism "submit"
                "--track" track
                "--exercise" exercise))

(defun exercism-submit-exercise-in-buffer ()
  "Submit an exercise in buffer."
  (let ((exc-path (exercism-find-project-root default-directory)))
    (when exc-path
      (let* ((dirs (split-string exc-path "/" t))
             (size (safe-length dirs))
             (exercise (nth (- size 1) dirs))
             (track (nth (- size 2) dirs)))
        (run-exercism "submit"
                      "--track" track
                      "--exercise" exercise)))))

(define-minor-mode exercism-mode
  "Minor mode for exercism."
  :lighter "Exercism"
  :group 'exercism
  ;; Add a project root discovery function for projectile and give it precedence.
  (when (not (member 'exercism-find-project-root projectile-project-root-functions))
    (push 'exercism-find-project-root
          projectile-project-root-functions))
  (when (null exercism-cli-path)
    (message "You need to set the path to your exercism cli program to exercism-cli-path")))

(add-hook 'projectile-mode-hook 'exercism-mode)

(provide 'exercism-mode)
;;; exercism-mode.el ends here
(projectile-ensure-project default-directory)
