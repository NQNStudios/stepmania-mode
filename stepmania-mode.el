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

(setq stepmania-mode-difficulty-values
			'("Beginner" "Easy" "Medium" "Hard" "Challenge" "Edit"))

(defun stepmania-mode-insert-tag ()
	"Insert one of the acceptable header tags for an SSC simfile"
	(interactive)
	(when (not (= (current-column) 0))
		(end-of-line)
		(open-line 1)
		(next-line))
	(insert "#")
	(let ((tag (completing-read "Choose a tag:" stepmania-mode-header-tags)))
		(insert tag)
		(insert ":")
		(pcase tag
			("DIFFICULTY" (insert (completing-read "Choose a difficulty:" stepmania-mode-difficulty-values))))
		(insert ";"))
	(backward-char))

(defun stepmania-mode--tag-value (tag)
	"Get the value of the given header tag (searching backwards from point for the most recent one)"
	(condition-case nil
			(save-excursion
				(unless (string= (word-at-point) (upcase tag))
					(search-backward tag))
				(let* ((value-start (search-forward ":")) (value-end (- (search-forward ";") 1)))
					(buffer-substring value-start value-end)))
		(error "")))

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
