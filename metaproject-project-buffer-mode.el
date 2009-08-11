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

(defun metaproject-project-buffer-mode (project-config args)
  "Just takes dummy args for now"
  (let* ((project-top-dir
	  (file-name-as-directory
	   (metaproject-config-get project-config 'project-base-dir)))
	 (files (metaproject-config-get project-config 'files))
	(buffer (generate-new-buffer (concat "project:" project-top-dir))))
	(display-buffer buffer)
	(metaproject-setup-buffer project-config buffer)
	(with-current-buffer buffer
	  (cd project-top-dir)
	  (project-buffer-mode)
	  (project-buffer-insert project-top-dir 'project project-top-dir project-top-dir)
	  (mapc (lambda (file) (project-buffer-insert file 'file file project-top-dir)) files))))

(metaproject-register-action "project-buffer" 'metaproject-project-buffer-mode)

(provide 'metaproject-project-buffer-mode)