;; sourced from https://github.com/stepmania/stepmania/wiki/ssc as of 11-18-19
(setq stepmania-mode-header-tags
			(let ((tag-symbol-list
						 '(version title subtitle artist titletranslit
											 subtitletranslit artisttranslit genre origin
											 credit banner background previewvid jacket
											 cdimage discimage lyricspath cdtitle music
											 preview offset samplestart samplelength
											 selectable bpms displaybpm stops delays warps
											 timesignatures tickcounts combos speeds scrolls
											 fakes labels lastsecondhint bgchanges keysounds
											 attacks notedata chartname stepstype description
											 chartstyle difficulty meter radarvalues notes)))
				(mapcar #'upcase (mapcar #'symbol-name tag-symbol-list))))

(setq stepmania-mode-header-tag-regex
			(apply #'concat
						(cons "^#\\("
										(append (mapcar (lambda (tag) (format "%s\\|" tag)) (cdr stepmania-mode-header-tags)) (cons (car stepmania-mode-header-tags) '("\\):"))))))

(setq stepmania-mode-highlights
			`((,stepmania-mode-header-tag-regex . font-lock-variable-name-face)))

(defun stepmania-mode-insert-tag ()
	"Insert one of the acceptable header tags for an SSC simfile"
	(interactive)
	(insert "#")
	(insert (completing-read "Choose a tag:" stepmania-mode-header-tags))
	(insert ":")
	(insert ";")
	(backward-char))

(defvar stepmania-mode-map nil "Keymap for `stepmania-mode'")

(progn
	(setq stepmania-mode-map (make-sparse-keymap))

	(define-key stepmania-mode-map (kbd "#") #'stepmania-mode-insert-tag))

(define-derived-mode stepmania-mode fundamental-mode "Simfile"
	"Major mode for making Stepmania charts.

\\{stepmania-mode-map}"
	(setq font-lock-defaults '(stepmania-mode-highlights)))

(add-to-list 'auto-mode-alist '("\\.ssc\\'" . stepmania-mode))

(provide 'stepmania-mode)
