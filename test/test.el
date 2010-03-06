(toggle-debug-on-error)
(require 'metaproject)
(setq myproj (metaproject-project-create-project "/home/nafai" "HOME"))
(metaproject-files-add-file-to-project myproj "/home/nafai/.gitconfig")
(metaproject-files-add-file-to-project myproj "/home/nafai/.bashrc")
(metaproject-files-add-file-to-project myproj "/home/nafai/.bash_history")
(metaproject-project-store-config myproj)

(setq myproj2 (metaproject-project-create-project "/home/nafai/Projects/metaproject/metaproject-test" "Metaproject"))

(mapc '(lambda (file) (metaproject-files-add-file-to-project myproj2 file)) '("metaproject.el" "project-utils.el" "metaproject-ido.el" "metaproject-magit.el" "metaproject-project-buffer-mode.el" "README.markdown" "notes.org"))

(metaproject-project-store-config myproj2)

(metaproject-project-load-config "/home/nafai/Projects/metaproject/metaproject-test/.metaproject")
(setq myproj3 (metaproject-current-projects-get-project-by-path "/home/nafai/Projects/metaproject/metaproject-test/"))
