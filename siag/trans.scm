;;
;; Bindings using _ rather than - in commands, also docs for the commands.
;;

;!add_menu(label)
;!
;!
;!
(define add_menu add-menu)

;!add_menu_entry(label, entry, command)
;!
;!
;!
(define add_menu_entry add-menu-entry)

;!add_submenu(label, sublabel)
;!
;!
;!
(define add_submenu add-submenu)

;!add_submenu_entry(label, sublabel, entry, command)
;!
;!
;!
(define add_submenu_entry add-submenu-entry)

;!add_keybinding(keys, command)
;!Defines a command to run on a certain sequence of keypresses.
;!add_keybinding("C-q" "quit_program()") makes the program exit
; when the user presses Ctrl-Q.
;!
(define add_keybinding add-keybinding)

;!new_buffer()
;!
;!
;!
(define new_buffer new-buffer)

;!load_buffer()
;!
;!
;!
(define load_buffer load-buffer)

;!save_buffer()
;!
;!
;!
(define save_buffer save-buffer)

;!save_buffer_as()
;!
;!
;!
(define save_buffer_as save-buffer-as)

;!preview()
;!
;!
;!

;!kill_buffer(x)
;!
;!
;!
(define kill_buffer kill-buffer)

;!make_backups(x)
;!
;!
;!
(define make_backups make-backups)

;!change_margins(x)
;!
;!
;!
(define change_margins change-margins)

;!change_paper_size(x)
;!
;!
;!
(define change_paper_size change-paper-size)

;!change_headfoot(x)
;!
;!
;!
(define change_headfoot change-headfoot)

;!print()
;!
;!
;!

;!print_format()
;!
;!
;!
(define print_format print-format)

;!put_property(b, key, value)
;!
;!
;!
(define put_property put-property)

;!load_external()
;!
;!
;!
(define load_external load-external)

;!save_external()
;!
;!
;!
(define save_external save-external)

;!quit_program()
;!
;!
;!
(define quit_program quit-program)

;!undo_restore()
;!
;!
;!
(define undo_restore undo-restore)

;!delete_cell()
;!
;!
;!
(define delete_cell delete-cell)

;!insert_line()
;!
;!
;!
(define insert_line insert-line)

;!remove_line()
;!
;!
;!
(define remove_line remove-line)

;!insert_col()
;!
;!
;!
(define insert_col insert-col)

;!remove_col()
;!
;!
;!
(define remove_col remove-col)

;!select_all()
;!
;!
;!
(define select_all select-all)

;!cut_block_to_string()
;!
;!
;!
(define cut_block_to_string cut-block-to-string)

;!copy_block_to_string()
;!
;!
;!
(define copy_block_to_string copy-block-to-string)

;!paste_block_from_string()
;!
;!
;!
(define paste_block_from_string paste-block-from-string)

;!search_forward()
;!
;!
;!
(define search_forward search-forward)

;!search_backward()
;!
;!
;!
(define search_backward search-backward)

;!edit_label()
;!
;!
;!
(define edit_label edit-label)

;!edit_expression()
;!
;!
;!
(define edit_expression edit-expression)

;!edit_c()
;!
;!
;!
(define edit_c edit-c)

;!edit_siod()
;!
;!
;!
(define edit_siod edit-siod)

;!edit_tcl()
;!
;!
;!
(define edit_tcl edit-tcl)

;!edit_guile()
;!
;!
;!
(define edit_guile edit-guile)

;!edit_python()
;!
;!
;!
(define edit_python edit-python)

;!define(form1, form2)
;!
;!define(foo, "bar") defines a variable foo with the value "bar".
;
; define(twice(x), 2*x) defines a function which returns the value
; of its argument multiplied by two.
;!

;!change_interpreter()
;!
;!
;!
(define change_interpreter change-interpreter)

;!add_property()
;!
;!
;!
(define add_property add-property)

;!set_mark_command()
;!
;!
;!
(define set_mark_command set-mark-command)

;!set_block()
;!
;!
;!
(define set_block set-block)

;!unset_block()
;!
;!
;!
(define unset_block unset-block)

;!copy_block()
;!
;!
;!
(define copy_block copy-block)

;!delete_block()
;!
;!
;!
(define delete_block delete-block)

;!fill_block()
;!
;!
;!
(define fill_block fill-block)

;!smart_fill_block()
;!
;!
;!
(define smart_fill_block smart-fill-block)

;!sort_block(a, b, c, d)
;!
;!
;!
(define sort_block sort-block)

;!block_sum()
;!
;!
;!
(define block_sum block-sum)

;!block_min()
;!
;!
;!
(define block_min block-min)

;!block_max()
;!
;!
;!
(define block_max block-max)

;!block_avg()
;!
;!
;!
(define block_avg block-avg)

;!block_borders(x)
;!Change the borders in or around the block:
;
;1 Borders
;
;2 Grid
;
;3 Underline
;
;4 Top
;
;5 Left
;
;6 Right
;
;0 None
;!
;!
(define block_borders block-borders)

;!change_first_page()
;!
;!
;!
(define change_first_page change-first-page)

;!set_cell_width()
;!
;!
;!
(define set_cell_width set-cell-width)

;!fit_block_width()
;!
;!
;!
(define fit_block_width fit-block-width)

;!set_standard_width()
;!
;!
;!
(define set_standard_width set-standard-width)

;!set_cell_height()
;!
;!
;!
(define set_cell_height set-cell-height)

;!fit_block_height()
;!
;!
;!
(define fit_block_height fit-block-height)

;!set_standard_height()
;!
;!
;!
(define set_standard_height set-standard-height)

;!set_standard_format()
;!
;!
;!
(define set_standard_format set-standard-format)

;!new_attribute(a)
;!Change the format of a cell or the block. a is any of:
;
;family = font family (Courier, Helvetica et al)
;
;style = cell style (Default, Currency et al)
;
;fg = foreground colour
;
;bg = background colour
;!
;!define_style
(define new_attribute new-attribute)

;!define_style()
;!
;!
;!new_attribute
(define define_style define-style)

;!data_entry()
;!
;!
;!
(define data_entry data-entry)
(define data_record_edit data-record-edit)
(define position_row position-row)
(define position_col position-col)
(define get_point get-point)

;!siag_net(p)
;!Starts a simple data server which listens to port p over tcp and
; understands the following commands:
;
; GET r1 c1 r2 c1 : Returns contents of r1c1..r2c2 with one cell per line
;
; PUT r1 c1 r2 c2 : Reads one entry per line and inserts into r1c1..r2c2 as labels
;
; QUIT : Quit the server and resume normal operation.
;!
;!
(define (siag_net) (siag-net))

;!enter_date()
;!
;!
;!
(define enter_date enter-date)

;!enter_time()
;!
;!
;!
(define enter_time enter-time)

;!what_cursor_position()
;!Identify Cell
;!
;!
(define what_cursor_position what-cursor-position)

;!recalc_matrix()
;!Recalculate
;!
;!
(define recalc_matrix recalc-matrix)

;!set_zoom(x)
;!Adjust the size at which the grid is displayed. Legal values are from
; 0.1 (10%) to 10 (1000%). This does not affect how documents are printed.
;!set_zoom(1.3) sets zoom to 130%.
;!zoom_adjust
(define set_zoom set-zoom)

;!zoom_adjust()
;!Asks for a new zoom factor.
;!
;!set_zoom
(define zoom_adjust zoom-adjust)

;!switch_to_buffer()
;!Change buffer
;!
;!
(define switch_to_buffer switch-to-buffer)

;!kill_buffer(x)
;!Kill a buffer
;!
;!
(define kill_buffer kill-buffer)

;!split_window_vertically()
;!Split the window and display the current buffer in both.
;!
;!
(define split_window_vertically split-window-vertically)

;!delete_window()
;!Remove this window and merge the space with the next one.
; The last window can't be removed.
;!
;!
(define delete_window delete-window)

;!delete_other_windows()
;!Remove all windows except this one.
;!
;!
(define delete_other_windows delete-other-windows)

;!add_sheet()
;!Add a sheet to the current buffer. It is automatically given a
; unique name which can be changed with rename_sheet().
;!
;!remove_sheet rename_sheet
(define add_sheet add-sheet)

;!remove_sheet()
;!Remove the current sheet from the buffer. The last sheet can't
; be removed.
;!
;!add_sheet
(define remove_sheet remove-sheet)

;!rename_sheet()
;!Rename the current sheet.
;!
;!
(define rename_sheet rename-sheet)

;!move_sheet_up()
;!Move the current one step closer to the top.
;!
;!move_sheet_down move_sheet_top move_sheet_bottom
(define move_sheet_up move-sheet-down)

;!move_sheet_down()
;!
;!
;!move_sheet_up move_sheet_top move_sheet_bottom
(define move_sheet_down move-sheet-down)

;!move_sheet_top()
;!
;!
;!move_sheet_up move_sheet_down move_sheet_bottom
(define move_sheet_top move-sheet-top)

;!move_sheet_bottom()
;!Move the current sheet to the bottom.
;!
;!move_sheet_up move_sheet_down move_sheet_top
(define move_sheet_bottom move-sheet-bottom)

;!protect_cells()
;!Make it so that all rows above and all columns to the left of the
; current cell are always displayed, even if the grid scrolls.
;!
;!remove_protection
(define protect_cells protect-cells)

;!remove_protection()
;!Scroll the grid normally.
;!
;!protect_cells
(define remove_protection remove-protection)

;!beginning_of_buffer()
;!Move to the first row and first column of the current sheet.
;!
;!end_of_buffer
(define beginning_of_buffer beginning-of-buffer)

;!end_of_buffer()
;!Move to the last column of the last row of the current sheet.
;!
;!beginning_of_buffer
(define end_of_buffer end-of-buffer)

;!top_of_buffer()
;!Move to the first row of the current sheet without changing the column.
;!
;!bottom_of_buffer
(define top_of_buffer top-of-buffer)

;!bottom_of_buffer()
;!Move to the last riw of the current sheet without changing the column.
;!
;!top_of_buffer
(define bottom_of_buffer bottom-of-buffer)

;!go_to()
;!Move to a specified row and column.
;!
;!
(define go_to go-to)

;!tooltip_set(mode)
;!Change the way tooltips act:
;
;0 No tips
;
;1 Display on the status line
;
;2 Display in popups
;
;3 Both
;!
;!
;(define tooltip_set tooltip-set)
(define tooltip_mode tooltip-mode)

;!grid_lines(x)
;!If x is 0, the grid lines are hidden, else they are displayed.
;!
;!
(define grid_lines grid-lines)

;!a1_refs_set(x)
;!If x is 0, R1C1 style references are assumed in formulas, otherwise
; A1 style references are assumed.
;!
;!
(define a1_refs_set a1-refs-set)

;!plot_plugin(mode)
;!Plot a graph of the data in the block and embed the result in the sheet.
;!
;!splot plot_wizard
(define plot_plugin plot-plugin)

;!splot(mode)
;!Make a surface (3D) plot of the data in the block.
;!
;!plot_plugin plot_wizard

;!plot_wizard()
;!Make a customizable plot of the data in the block.
;!
;!plot_plugin splot
(define plot_wizard plot-wizard)

;!webserver()
;!Run a simple, single-shot webserver which listens on port 8080.
;!
;!

;!mailto()
;!Send the current sheet using e-mail in HTML table format.
;!
;!

;!filemgr()
;!Run a simple file manager. This is a demo application.
;!
;!

;!form_test()
;!Demo the form interface.
;!
;!
(define form_test form-test)
(define form_test2 form-test2)
(define form_test3 form-test3)

;!plugin_import(x)
;!Import a plugin, i.e. copy the data and embed the corresponding
; application.
;!
;!plugin_link plugin_export
(define plugin_import plugin-import)

;!plugin_export()
;!Export a plugin, i.e. save its data in a separate file.
;!
;!plugin_import
(define plugin_export plugin-export)

;!plugin_link(x)
;!Link a plugin, i.e. embed the corresponding application using
; the existing data file.
;!
;!plugin_import
(define plugin_link plugin-link)

;!plugin_delete()
;!Remove a plugin.
;!
;!
(define plugin_delete plugin-delete)

;!plugin_move()
;!Move a plugin
;!
;!
(define plugin_move plugin-move)

;!plugin_resize()
;!Resize a plugin. This can more conveniently be done by clicking
; and dragging its lower right corner.
;!
;!
(define plugin_resize plugin-resize)

;!help_contents()
;!Display the table of contents for Siag's online help.
;!
;!
(define help_contents help-contents)

;!help_copyright()
;!Display the Gnu public licence.
;!
;!
(define help_copyright help-copyright)

;!help_for_help()
;!Display instructions for the help system.
;!
;!
(define help_for_help help-for-help)

;!do_link(url)
;!Pops up the specified URL in the help browser.
;!
;!help_link
(define do_link do-link)

;!help_link(page)
;!Pops up a page from Siag's online documentation in the help browser.
;!
;!
;(define help_link help-link)

;!aboutbox()
;!Display the current version and basic info about Siag.
;!
;!aboutsiag

;!aboutsiag()
;!Display basic info about Siag Office.
;!
;!aboutbox

;!beginning_of_line()
;!Move to the beginning of the line.
;!
;!end_of_line
(define beginning_of_line beginning-of-line)

;!backward_cell()
;!Move one cell to the left.
;!
;!forward_cell
(define backward_cell backward-cell)

;!end_of_line()
;!Move to the last used column of the line.
;!
;!beginning_of_line
(define end_of_line end-of-line)

;!forward_cell()
;!
;!
;!
(define forward_cell forward-cell)

;!keyboard_quit()
;!
;!
;!
(define keyboard_quit keyboard-quit)

;!recenter()
;!
;!
;!

;!next_line()
;!
;!
;!
(define next_line next-line)

;!previous_line()
;!
;!
;!
(define previous_line previous-line)

;!universal_argument()
;!
;!
;!
(define universal_argument universal-argument)

;!scroll_up()
;!
;!
;!
(define scroll_up scroll-up)

;!scroll_down()
;!
;!
;!
(define scroll_down scroll-down)

;!execute_extended_command()
;!
;!
;!
(define execute_extended_command execute-extended-command)

;!prefix()
;!
;!
;!

;!scroll_right()
;!
;!
;!
(define scroll_right scroll-right)

;!scroll_left()
;!
;!
;!
(define scroll_left scroll-left)

;!argument_sign()
;!
;!
;!
(define argument_sign argument-sign)

;!argument_digit()
;!
;!
;!
(define argument_digit argument-digit)

;!exchange_point_and_mark()
;!
;!
;!
(define exchange_point_and_mark exchange-point-and-mark)

;!call_last_kbd_macro()
;!
;!
;!
(define call_last_kbd_macro call-last-kbd-macro)

;!other_window()
;!
;!
;!
(define other_window other-window)

;!start_kbd_macro()
;!
;!
;!
(define start_kbd_macro start-kbd-macro)

;!end_kbd_macro()
;!
;!
;!
(define end_kbd_macro end-kbd-macro)

;!interpreter_test(intp)
;!
;!
;!
(define interpreter_test interpreter-test)

;!llpr(text)
;! Print a text on the status line.
;!
;!

;!print_version()
;!
;!
;!
(define print_version print-version)

;!hyperlink()
;!
;!
;!

;!suspend_function()
;!
;!
;!
(define (suspend_function) (suspend-function))

