;;; metaproject.el --- Project support and utilities for Emacs

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
;; Metaproject is a library providing functionality centered around
;; the the idea of a project.  A project is simply a group of related
;; files and buffers, with all of the files located under a common
;; directory.

;; I originally wrote Metaproject as I found it a pain to always open
;; the same small group of source files I was working on and I
;; realized that most of what I do is in context of a given "project"
;; and that it would be simpler if many of the common operations I do
;; in Emacs -- including buffer switching, using a version control
;; system, opening and closing files, etc are almost always around a
;; related set of things.

;; But I also realized that the set of "things" I do are not
;; necessarily the same set of "things" others do.  For example, I use
;; the excellent mode magit for interacting with my git
;; repositories.  It doesn't ship with Emacs.  Others may not even use
;; git.  So I wanted some of the functionality to be optional, but
;; still be integrated seamlessly when used.

;; This lead to the current architecture.  All but the most core
;; functions that manipulate a project's data and the global project
;; list are found in what I term 'modules'.  These modules will
;; register with the core when loaded.  Only when configuration for a
;; given module is found in a project and the module is loaded will it
;; be used.  I hope that it will prove to be both flexible enough and
;; powerful enough to meet other's needs.  Look in the documentation
;; for the various functions and variables for how modules should be
;; implemented and what they can do.


;; A bit of Metaproject philosophy is borrowed from the Zen of Python:
;; 'Explicit is better than implicit.'  Or perhaps the Principle of
;; Least Astonishment applys a bit better in this  case.  Anyway,
;; what I mean is that Metaproject (or its modules) should not do
;; anything the user has not explicitly asked for.  This means, for
;; example that even if the user has configuration for a module
;; in their project file, it should not load the corresponding
;; buffers until explcitly told to.  The 'files' module, for example,
;; is a way of specifying what files are in a project.  It can,
;; in its open action, automatically open all of the files specified
;; and do this when the project is opened.  But, in the default
;; configuration, this does not happen.

;; Some may ask why I wrote this instead of using any of the half
;; dozen or so other "project" modes for Emacs that are listed on the
;; EmacsWiki.  None of the ones I looked at did quite what I wanted.
;; I wasn't looking for having common settings for my project --
;; though that is certainly a possibility.  I wasn't concerned about
;; something to generate Makefiles or handle compilation -- though
;; that would be nice.  I wasn't looking for a nice view of all of my
;; files -- though it would be helpful.  I hope that this way of doing
;; projects is a little more flexible and will eventually allow many
;; of these other things.

;; I welcome comments, critiques, patches, whatever.  This is my first
;; major Emacs Lisp code beyond what I have in my ~/.emacs and so I'm
;; learning how to do things.  Any help and improvement would be
;; welcome!

;; Usage:
;; TODO: Explain usage.

;;; History:
;; Version: 0.01
;; Date: May 7, 2009

;;; Code:
(eval-when-compile (setq byte-compile-dynamic t))

; The following suggested by (info "(elisp)Coding Conventions")
(eval-when-compile
  (require 'cl))

;;;; Constants
(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration files for each project.")

(add-to-list 'auto-mode-alist '("\\.metaproject$" . emacs-lisp-mode))

(defconst metaproject-version "0.02-SNAPSHOT"
  "This is the current version of metaproject.")

;; Error messages
(defconst metaproject-error-directory-not-found
  "The specified directory '%s' does not exist or is not a directory")

(defconst metaproject-error-not-valid-file
  "The file '%s' is not a valid file or doesn't exist under project directory")

;;;; Customization items
(defgroup metaproject nil
  "Project support and utilities."
  :group 'tools)

(defcustom metaproject-project-dirs nil
  "This is a list of the directories that contain project directories."
  :type '(repeat directory)
  :group 'metaproject)

;;;; Currently Open Projects
(defvar metaproject-current-projects (make-hash-table :test 'equal)
  "This is the group of all of the projects that are currently open.
It is a hash table where the keys are the top level directories of
each project and the values are property lists containing the configuration
information and state of each project.")

(defun metaproject-current-projects-get-project-by-path (path)
  "Get the currently open project that has the top level directory PATH.
Returns nil if a project from that path is not currently open."
  (gethash path metaproject-current-projects nil))

(defun metaproject-current-projects-remove-project (project)
  "Remove the project specified by PROJECT from the current projects group.
This does not close the project or any of its buffers, this may be
done elsewhere."
  (let ((project-base-dir (metaproject-project-state-get project 'project-base-dir)))
    (remhash project-base-dir metaproject-current-projects)))

(defun metaproject-current-projects-add-project (project)
  "Add the project specified by PROJECT to the current projects group.
It is assumed that the project itself is opened and any related operations
are done elsewhere."
  (let ((project-base-dir (metaproject-project-state-get project 'project-base-dir)))
    (assert (not (null project-base-dir)))
    (puthash project-base-dir project metaproject-current-projects)))

;;;; General Project support
(defconst metaproject-project-parts (list (make-symbol "state") (make-symbol "config"))
  "These are the symbols that are used in the in-memory project structures.
'state' will contain the current state of the project: open buffers, etc.
'config' will contain the configuration to be persisted across project loads.

Note this constant is primarily for documentation and symbol creation
purposes and is not explicitly referenced elsewhere at the moment.")

(defconst metaproject-project-empty-template '((state . nil) (config . nil))
  "Template for empty in-memory project structures.")

(defun metaproject-project-p (dir)
  "Return t if the directory DIR is the top level of a Metaproject project."
  (file-exists-p (expand-file-name metaproject-config-file-name dir)))

(defun metaproject-project-create (project-base-dir &optional name)
  "Create a project with the root PROJECT-BASE-DIR and optionally, named NAME.
If PROJECT-BASE-DIR is not an absolute path name, it is assumed to be relative
to the current buffer's `default-directory', which could be
unexpected, so it is preferred to call this with an absolute path.
If NAME is not specified, the directory name of PROJECT-BASE-DIR (not the full
path) is used.  If the directory PROJECT-BASE-DIR does not exist, or is not a
directory, an error is signaled."
  (let ((project-base-dir (expand-file-name project-base-dir)))
    (if (and (file-exists-p project-base-dir)
	     (file-directory-p project-base-dir))
	(let* ((name (if (null name)
			 (file-name-directory project-base-dir)
		       name))
	       (new-project (copy-alist metaproject-project-empty-template)))
	  (setq new-project (metaproject-project-config-put new-project 'name name)
		new-project (metaproject-project-state-put new-project 'project-base-dir project-base-dir))
	  (metaproject-current-projects-add-project new-project))
      (error metaproject-error-directory-not-found project-base-dir))))

;;;; Project state and configuration handling
(defun metaproject-project-data-get (project sym variable)
  "Return from PROJECT in SYM's list the value of VARIABLE.
Note that this does not differentiate between a variable having a null value
and the variable not existing.  Use `metaproject-project-data-member' if
concerned about null values."
  (let ((data (cdr (assoc sym project))))
    (plist-get data variable)))

(defun metaproject-project-data-member (project sym variable)
  "Return from PROJECT in SYM's list the cons of VARIABLE and its value or nil if not found."
  (let ((data (cdr (assoc sym project))))
    (plist-member data variable)))

(defun metaproject-project-data-put (project sym variable value)
  "On PROJECT in SYM's list, set VARIABLE to VALUE.
If VARIABLE exists, overwrite the existing value.  Returns the updated
project."
  (let* ((data-assoc (assoc sym project))
	 (data (cdr data-assoc)))
    (setcdr data-assoc (plist-put data variable value))
    project))

(defmacro metaproject-project-data-accessors (sym)
  "Generate the accessor functions for SYM in a project.
This includes `metaproject-project-SYM-get', `metaproject-project-SYM-member',
and `metaproject-project-*-put'."
  (let* ((name (symbol-name sym))
	 (prefix "metaproject-project-")
	 (get-fn-name (intern (concat prefix name "-get")))
	 (member-fn-name (intern (concat prefix name "-member")))
	 (put-fn-name (intern (concat prefix name "-put"))))
    `(progn
       (defun ,get-fn-name (project variable)
	 ,(concat "Return from the PROJECT " name " the value of VARIABLE.
Note that this does not differentiate between a variable having a null value
and the variable not existing.  Use `metaproject-project-" name "-member' if
concerned about null values.")
	 (metaproject-project-data-get project ',sym variable))
       (defun ,member-fn-name (project variable)
	 ,(concat "Return from the PROJECT " name " the cons of VARIABLE and its value or nil if not found.")
	   (metaproject-project-data-get project ',sym variable))
       (defun ,put-fn-name (project variable value)
	 ,(concat "On PROJECT, set VARIABLE to VALUE in the " name ".
If VARIABLE exists, overwrite the existing value.  Returns the updated
project.")
	 (metaproject-project-data-put project ',sym variable value)))))

(metaproject-project-data-accessors config)
(metaproject-project-data-accessors state)

(defun metaproject-project-config-store (project)
  "Store the configuration for PROJECT to disk."
  (let* ((config (cdr (assoc 'config project)))
	 (project-base-dir (metaproject-project-state-get project 'project-base-dir))
	 (config-file-name (expand-file-name metaproject-config-file-name project-base-dir))
	 (config-buffer (find-file config-file-name)))
    (save-excursion
      (set-buffer config-buffer)
      (erase-buffer)
      (print config (current-buffer))
      (save-buffer)
      (kill-buffer))))

(defun metaproject-project-config-load (project-file-name)
  "Load the configuration for a project from PROJECT-FILE-NAME."
  (let ((project-base-dir (or (file-name-directory project-file-name) default-directory))
	(new-project (copy-alist metaproject-project-empty-template)))
    (if (metaproject-project-p project-base-dir)
	(save-excursion
	  (find-file project-file-name)
	  (let ((project-config (read (current-buffer))))
	    (setcdr (assoc 'config new-project) project-config)
	    (metaproject-project-state-put new-project 'project-base-dir project-base-dir)
	    (metaproject-current-projects-add-project new-project)
	    (kill-buffer)
	    new-project)))))

;; Module helpers
(defconst metaproject-module-config-parts (list (make-symbol "autoload"))
  "These are the symbols that are common in all of the module definitions.
'autoload' indicates whether this module's open function is to be run
when the project is opened.

Note this constant is primarily for documentation and symbol creation
purposes and is not explicitly referenced elsewhere at the moment.")

(defconst metaproject-module-config-empty-template '((autoload . nil))
  "Template for empty in-memory module configuration with default values.
'autoload' defaults to nil, the user should explicitly turn on loading.")

;;;; Metaproject Files module definition.
;; The Files module specifies the files in a project.
;; Its configuration is a plist with the following properties:
;; - files - list of file paths, all relative to the project project-base-dir
;; - find-all-at-open - find all files at project open if t
;; - TODO: Define other keys as needed, possible are include-regexp and exclude-regexp
;;

;; It is one of the default modules that is always loaded and core Metaproject
;; depends upon, but for simplicity, in many cases, such as project load and store
;; time, it is treated like any other module.  It just happens to live in the same file.

;; TODO-MAYBE: Rename metaproject-project-get-open-files to something more apropos when implementing.
(defun metaproject-files-get-from-project (project)
  "Return the list of files belonging to PROJECT.
These files are not necessarily currently open.  Use
`metaproject-project-get-open-files' to get a list of the project files
that are currently open."
  (let ((files-config (metaproject-project-config-get project 'files)))
	 (plist-get files-config 'files)))

(defun metaproject-files-put-to-project (project files)
  "Set on PROJECT the list of FILES."
  (let ((files-config (metaproject-project-config-get project 'files)))
    (setq files-config (plist-put files-config 'files files))
    (metaproject-project-config-put project 'files files-config)))

(defun metaproject-files-valid-file-in-project-p (file project)
  "Return t if FILE exists, is a regular file, and is under the PROJECT's directory."
  (let* ((project-base-dir (metaproject-project-state-get project 'project-base-dir))
	 (default-directory project-base-dir)
	 (expanded-file-name (file-relative-name (expand-file-name file project-base-dir) project-base-dir)))
    (and
     (not (null project-base-dir))
     ;; The result of `file-relative-name' will start with "../" when the
     ;; file is not under PROJECT-BASE-DIR.  Not sure if this is portable to Windows.
     (not (string= "../" (substring expanded-file-name 0 3)))
     (file-exists-p expanded-file-name)
     (file-regular-p expanded-file-name))))

(defun metaproject-files-add-file-to-project (project file)
  "Add to the project PROJECT the file FILE.
Only add FILE if it isn't already a member of PROJECT.  FILE is
assumed to be an absolute pathname or relative to PROJECT's project-base-dir.
If FILE is not a valid file (i.e., not a valid file or under the
project's directory), an error is signaled."
  (if (metaproject-files-valid-file-in-project-p file project)
      (let* ((project-base-dir (metaproject-project-state-get project 'project-base-dir))
	     (expanded-file-name (expand-file-name file project-base-dir))
	     (relative-file-name (file-relative-name expanded-file-name project-base-dir))
	     (project-files (metaproject-files-get-from-project project)))
	(when (not (member relative-file-name project-files))
	  (metaproject-files-put-to-project
	   project
	   (append project-files (list relative-file-name)))))
    (error metaproject-error-not-valid-file file)))

(provide 'metaproject)
;;; metaproject.el ends here
