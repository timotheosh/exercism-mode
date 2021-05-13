;;; exercism-mode-pkg.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Tim Hawes
;;
;; Author: Tim Hawes <https://github.com/thawes>
;; Maintainer: Tim Hawes <trhawes@gmail.com>
;; Created: May 13, 2021
;; Modified: May 13, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/thawes/exercism-mode-pkg
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(define-package "exercism-mode" "0.0.1"
  "An Emacs mode for working with exercism.io programming problems."
  '((projectile "2.0" "https://github.com/bbatsov/projectile"))
  :authors
  '(("Tim Hawes" . "trhawes@gmail.com"))
  :maintainer
  '(("Tim Hawes" . "trhawes@gmail.com"))
  :keywords
  '("project" "projectile" "exercism.io" "exercism")
  :homepage "https://github.com/timotheosh/exercism-mode")


(provide 'exercism-mode-pkg)
;;; exercism-mode-pkg.el ends here
