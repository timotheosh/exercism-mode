;;; exercism-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Tim Hawes
;;
;; Author: Tim Hawes <https://github.com/timotheosh>
;; Maintainer: Tim Hawes <trhawes@gmail.com>
;; Created: May 11, 2021
;; Modified: May 13, 2021
;; Version: 0.0.2
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/timotheosh/exercism-mode
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
  "Looks for the common places the exercism config might be in.
Returns nil if it doesn't find it."
  (cond ((file-exists-p (expand-file-name "~/snap/exercism/current/.config/exercism/user.json")) (expand-file-name "~/snap/exercism/current/.config/exercism/user.json"))
        ((file-exists-p (expand-file-name "~/.config/exercism/user.json")) (expand-file-name "~/.config/exercism/user.json"))
        ((file-exists-p (expand-file-name "~/.exercism/user.json")) (expand-file-name "~/.exercism/user.json"))))

(defcustom exercism-config-path
  (find-exercism-config)
  "Path where the exercism config file can be found."
  :group 'exercism-mode
  :type 'string)

(defcustom exercism-web-browser-function
  'eww-browse-url
  "Browser function to use when viewing documentation for Exercism."
  :group 'exercism-mode
  :type browse-url--browser-defcustom-type)

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
  (let ((dir (file-truename (or dir default-directory))))
    (or
     (locate-dominating-file dir ".exercism/metadata.json")
     (let ((exc-dir (length exercism-workspace))
           (buff-dir (length dir)))
       (when (< exc-dir buff-dir)
         (let ((p (substring dir 0 exc-dir)))
           (when (string= exercism-workspace p)
             (let* ((exer-dir (substring dir exc-dir))
                    (dir-list (split-string exer-dir "/" t)))
               (string-join (list exercism-workspace (nth 0 dir-list) (nth 1 dir-list)) "/")))))))))

(defun exercism-get-track-exercise ()
  "Returns the track and exercise names for currect file."
  (let ((exc-path (exercism-find-project-root default-directory)))
    (when exc-path
      (let* ((dirs (split-string exc-path "/" t))
             (size (safe-length dirs))
             (exercise (nth (- size 1) dirs))
             (track (nth (- size 2) dirs)))
        (list :track track :exercise exercise)))))

(defcustom exercism-cli-path
  (executable-find "exercism")
  "Path for the exercism cli program."
  :group 'exercism-mode
  :type 'string)

(defun exc-process-output (process output)
  "This is a filter for the the run-exercism function. PROCESS is
unused. OUTPUT is from the running process."
  (message (string-trim output)))

(defun run-exercism (&rest args)
  "Run the exercism cli tool with ARGS. ARGS can be &rest or a
single list. It should never be both."
  (when (equal (type-of (car args)) 'cons) ; Check if ARGS is being passed as a list.
    (setq args (car args)))
  (let ((cmd (push exercism-cli-path args)))
    (message (message "Running exercism-cli with: %s" cmd))
    (make-process
     :name "exercism"
     :buffer "*exercism-cli*"
     :filter #'exc-process-output
     :command cmd)))

(defun exercism-download-exercise (track exercise)
  "Downloads an exercise. TRACK is the exercism track. EXERCISE
is the name of the exercism exercise."
  (interactive "sTrack: \nsExercise: ")
  (run-exercism "download"
                "--track" track
                "--exercise" exercise)
  (when (file-directory-p (concat exercism-workspace "/" track "/" exercise))
    (dired (concat exercism-workspace "/" track "/" exercise))))

(defun exercism-refresh-exercise ()
  "Updates current exercise from Exercism.io to the latest version."
  (interactive)
  (let ((exercise-data (exercism-get-track-exercise)))
    (if exercise-data
        (exercism-download-exercise (plist-get exercise-data :track) (plist-get exercise-data :exercise))
      (message "This file is not in an Exercism project."))))

(defun exercism-download-command (cmd)
  "exercism.io website has a button that allows the user to copy
  and paste the needed exercism cli command. This function allows
  the user to paste the command directly into emacs."
  (interactive "sPaste exercism download command: ")
  (let* ((cmd-list (split-string cmd))
         (exercise (nth 1 (split-string (nth 2 cmd-list) "=" t)))
         (track (nth 1 (split-string (nth 3 cmd-list) "=" t))))
    (exercism-download-exercise track exercise)))

(defun exercism-submit-file ()
  "Submits an exercise. TRACK is the exercism track. EXERCISE is
the name of the exercism exercise."
  (interactive)
  (if (exercism-find-project-root default-directory)
      (run-exercism "submit" buffer-file-name)
    (message "This file is not in an Exercism project.")))

(defun exercism-dired-submit-marked-files ()
  "Submit all files marked in dired to exercism.io"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (run-exercism (push "submit" files))
      (message "There are no marked files in dired."))))

(defun exercism-open-exercise-web ()
  "Opens the current exercism exercise on the exercism website."
  (interactive)
  (let ((track-exercise (exercism-get-track-exercise)))
    (if track-exercise
        (funcall exercism-web-browser-function
                 (format "https://exercism.io/tracks/%s/exercises/%s"
                         (plist-get track-exercise :track)
                         (plist-get track-exercise :exercise)))
      (message "File is not part of an Exercism exercise."))))

(defvar exercism-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") 'exercism-submit-file)
    (define-key map (kbd "C-c d") 'exercism-download-exercise)
    (define-key map (kbd "C-c p") 'exercism-download-command)
    (define-key map (kbd "C-c r") 'exercism-refresh-exercise)
    (define-key map (kbd "C-c w") 'exercism-open-exercise-web)
    (define-key dired-mode-map (kbd "C-c e") 'exercism-dired-submit-marked-files)
    map))

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

(provide 'exercism-mode)
;;; exercism-mode.el ends here
