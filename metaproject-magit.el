;;; metaproject-magit.el --- magit plug-in for metaproject

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
(require 'project-utils)
(require 'magit)

(defun metaproject-magit-status (project-config args)
  (let ((git-dir (magit-get-top-dir (file-name-as-directory (plist-get project-config 'project-base-dir)))))
    (when (not (null git-dir))
      (magit-status git-dir))))

(metaproject-register-action "magit-status" 'metaproject-magit-status)

(defcustom metaproject-magit-repo-dirs nil
  "A list of directories that contain git repos")

(defun metaproject-magit-is-git-repo (dir)
  (file-exists-p (expand-file-name ".git" dir)))

(defun metaproject-magit-find-git-repos (dir)
  (project-util-find #'metaproject-magit-is-git-repo dir))

(defun metaproject-magit-status-repos (arg)
  "Provides, via ido, a list of repos to choose from to display their status buffer.
When called normally, the repo directory that the current buffer is in is selected
by default. If called with the universal argument, none are selected."
  (interactive "P")
  (project-util-action-ido
   #'metaproject-magit-find-git-repos
   metaproject-magit-repo-dirs
   #'magit-get-top-dir
   #'magit-status
   "Git repository? "
   arg))

(provide 'metaproject-magit)

