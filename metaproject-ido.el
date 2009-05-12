;;; metaproject-ido.el --- ido wrappers for various metaproject functions

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

(defun metaproject-open-ido (arg)
  (interactive "P")
  (project-util-action-ido
   #'metaproject-find-metaprojects
   metaproject-project-dirs
   #'metaproject-get-top-dir
   #'metaproject-open-project
   "Metaproject Project to open? "
   arg))

(provide 'metaproject-ido)
