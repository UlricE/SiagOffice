;; Common stuff for the Tools menu

(add-submenu "Tools" "Command")
(add-submenu-entry "Tools" "Command" "SIOD" "(exec-siod)")
(add-submenu-entry "Tools" "Command" "C" "(exec-c)")
(add-submenu-entry "Tools" "Command" "Guile" "(exec-guile)")
(add-submenu-entry "Tools" "Command" "Python" "(exec-python)")
(add-submenu-entry "Tools" "Command" "Ruby" "(exec-ruby)")
(add-submenu-entry "Tools" "Command" "Tcl" "(exec-tcl)")

(add-menu-entry "Tools" "-" "-")

(if (stat (string-append datadir "/pw/pw.scm"))
  (add-menu-entry "Tools" "Pathetic Writer" "(spawn \"pw\")"))
(if (stat (string-append datadir "/siag/siag.scm"))
  (add-menu-entry "Tools" "Scheme In A Grid" "(spawn \"siag\")"))
(if (stat (string-append datadir "/egon/egon.scm"))
  (add-menu-entry "Tools" "Egon Animator" "(spawn \"egon\")"))
(add-menu-entry "Tools" "Terminal" "(spawn terminal-command)")
(add-menu-entry "Tools" "Editor" "(spawn editor-command)")
(add-menu-entry "Tools" "File Manager" "(spawn filer-command)")
(add-menu-entry "Tools" "Calculator" "(spawn calculator-command)")

(add-submenu "Tools" "Environment")
(add-submenu-entry "Tools" "Environment" "Defaults" "(set-environment \"Defaults\")")
(add-submenu-entry "Tools" "Environment" "KDE" "(set-environment \"KDE\")")
(add-submenu-entry "Tools" "Environment" "KDE2" "(set-environment \"KDE2\")")
(add-submenu-entry "Tools" "Environment" "CDE" "(set-environment \"CDE\")")
(add-submenu-entry "Tools" "Environment" "Gnome" "(set-environment \"Gnome\")")

(add-submenu-entry "Tools" "Environment" "Custom" "(edit-applications)")
;(add-menu-entry "Tools" "Edit Applications" "(edit-applications)")
; Environment is saved automatically
;(add-menu-entry "Tools" "Save Applications" "(save-applications)")

