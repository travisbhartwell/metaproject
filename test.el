(toggle-debug-on-error)
(require 'metaproject)
(setq myproj (metaproject-project-create "/home/nafai" "HOME"))
(metaproject-files-add-file-to-project myproj "/home/nafai/.emacs")
(metaproject-files-add-file-to-project myproj "/home/nafai/.bashrc")
(metaproject-files-add-file-to-project myproj "/home/nafai/.bash_history")
(metaproject-project-config-store myproj)

(setq myproj2 (metaproject-project-create "/home/nafai/Projects/metaproject/metaproject-test" "Metaproject"))

(mapc '(lambda (file) (metaproject-files-add-file-to-project myproj2 file)) '("metaproject.el" "project-utils.el" "metaproject-ido.el" "metaproject-magit.el" "metaproject-project-buffer-mode.el" "README.markdown" "notes.org"))

(metaproject-project-config-store myproj2)

(metaproject-project-config-load "/home/nafai/Projects/metaproject/metaproject-test/.metaproject")
(setq myproj3 (metaproject-current-projects-get-project-by-path "/home/nafai/Projects/metaproject/metaproject-test/"))
