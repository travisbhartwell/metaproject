;;; metaproject-project-buffer-mode.el --- metaproject plug-in for project-buffer-mode

;; Copyright (C) 2009 Travis B. Hartwell (nafai AT travishartwell DOT net)

;; Version: 0.02-SNAPSHOT

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; TODO: Summary goes here

;;; History:
(require 'metaproject)
(require 'project-buffer-mode)

(defun metaproject-project-buffer-create-buffer-name (project-top-dir)
  (concat "*project:" project-top-dir "*"))

(defun metaproject-project-buffer-get-project-top-dir (project-config)
  (file-name-as-directory
   (metaproject-config-get project-config 'project-base-dir)))

(defun metaproject-project-buffer-mode (project-config args)
  "Just takes dummy args for now"
  (let* ((project-top-dir (metaproject-project-buffer-get-project-top-dir project-config))
	 (files (metaproject-config-get project-config 'files))
	 (config-buffer (metaproject-config-get project-config 'project-config-buffer))
	 (buffer (generate-new-buffer (metaproject-project-buffer-create-buffer-name project-top-dir))))
    (save-excursion
      (set-buffer buffer)
      (cd project-top-dir)
      (project-buffer-mode)
      (metaproject-setup-buffer project-config buffer)
      (project-buffer-insert project-top-dir 'project project-top-dir project-top-dir)
      (mapc (lambda (file) (project-buffer-insert file 'file file project-top-dir)) files))))

(metaproject-register-action "project-buffer-mode" 'metaproject-project-buffer-mode)

(defun metaproject-project-buffer-switch-to ()
  (interactive)
  (let* ((project-config (metaproject-get-project-config-from-buffer (current-buffer)))
	 (project-top-dir (metaproject-project-buffer-get-project-top-dir project-config))
	 (project-buffer-buffer-name (metaproject-project-buffer-create-buffer-name project-top-dir))
	 (project-buffer (get-buffer project-buffer-buffer-name)))
    (if (not (null project-buffer))
	(switch-to-buffer project-buffer)
      (metaproject-project-buffer-mode project-config t))))

(metaproject-add-binding-to-keymap "p" 'metaproject-project-buffer-switch-to)

(provide 'metaproject-project-buffer-mode)