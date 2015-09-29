
; Arguments to plugin-register:
; 1. Plaintext description
; 2. Filename extension
; 3. Command string

; To avoid having the application pop up on the screen before it is
; reparented, add "-geometry +4000+4000" or some other hideously
; remote location. This works for most civilized applications and
; window managers; exceptions are Gtk and MWM.
; For plugins handled by the dummy plugin, the geometry option
; should be added in dummy.scm, not here.

(plugin-register "Siag" "siag"
	"siag -plugin -gridonly -geometry 400x100+4000+4000 %s")
(plugin-register "Pathetic Writer" "pw"
	"pw -plugin -geometry 300x150+4000+4000 %s")
(plugin-register "Egon Animator" "egon"
	"egon -plugin -geometry 300x150+4000+4000 %s")

(plugin-register "Plot" "plot"
  (string-append libdir "/plugins/plot -geometry +4000+4000 %s"))

(plugin-register "Text" "txt"
  (string-append libdir "/plugins/text -geometry +4000+4000 %s"))

(define (plugin-register-image desc ext)
  (plugin-register desc ext
    (string-append libdir "/plugins/image -plugin -geometry +4000+4000 %s")))

(plugin-register-image "GIF image" "gif")
(plugin-register-image "Jpeg image" "jpg")
(plugin-register-image "Jpeg image" "jpeg")
(plugin-register-image "Tagged Image File" "tif")
(plugin-register-image "Tagged Image File" "tiff")
(plugin-register-image "Portable Network Graphics" "png")
(plugin-register-image "BMP image" "bmp")
(plugin-register-image "X pixmap" "xpm")
(plugin-register-image "Portable pixmap" "ppm")
(plugin-register-image "Portable bitmap" "pbm")
(plugin-register-image "Portable graymap" "pgm")
(plugin-register-image "Portable anymap" "pnm")
(plugin-register-image "X bitmap" "xbm")
(plugin-register-image "X window dump" "xwd")

(plugin-register "PostScript" "ps"
  (string-append libdir "/plugins/clipart -plugin %s"))
(plugin-register "Encapsulated PostScript" "eps"
  (string-append libdir "/plugins/clipart -plugin %s"))
(plugin-register "LaTeX" "tex"
  (string-append libdir "/plugins/clipart -plugin %s"))
(plugin-register "DVI" "dvi"
  (string-append libdir "/plugins/clipart -plugin %s"))


(plugin-register "Run" "run"
  (string-append libdir "/plugins/dummy %s"))
(plugin-register "HTML" "html"
  (string-append libdir "/plugins/dummy %s"))

(plugin-register "Gnuplot" "cmd"
  (string-append libdir "/plugins/dummy %s"))

(plugin-register "Hello" "hello"
  (string-append libdir "/plugins/hello +4000+4000 %s"))

