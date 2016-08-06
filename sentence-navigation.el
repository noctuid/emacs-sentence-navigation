;;; sentence-navigation.el --- Commands to navigate one-spaced sentences.

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/emacs-sentence-navigation
;; Keywords: sentence evil
;; Package-Requires: ((ample-regexps "0.1") (cl-lib "0.5") (emacs "24.4"))
;; Version: 0.1

;;; Commentary:
;; This package gives commands for sentence navigation and manipulation
;; that ignore abbreviations. For example, in a sentence that contains
;; "Mr. MacGyver", "MacGyver" will not be considered to be the start of
;; a sentence. One-spaced sentences are the target of this plugin, but
;; it will also work properly with two-spaced sentences.

;; This package is inspired by vim-textobj-sentence and uses a modified
;; version of its default regex list. Evil is an optional dependency
;; that is used for the text objects this package provides.

;; For more information see the README in the github repo.

;;; Code:
(eval-and-compile (require 'ample-regexps))
(require 'cl-lib)

(defgroup sentence-navigation nil
  "Gives commands for navigating sentences and sentence text objects."
  :group 'editing
  :prefix 'sentence-nav-)

(defcustom sentence-nav-abbreviation-list
  '("[ABCDIMPSUabcdegimpsv]"
    "l[ab]" "[eRr]d" "Ph" "[Ccp]l" "[Ll]n" "[c]o"
    "[Oe]p" "[DJMSh]r" "[MVv]s" "[CFMPScfpw]t"
    "alt" "[Ee]tc" "div" "es[pt]" "[Ll]td" "min"
    "[MD]rs" "[Aa]pt" "[Aa]ve?" "[Ss]tr?" "e\\.g"
    "[Aa]ssn" "[Bb]lvd" "[Dd]ept" "incl" "Inst" "Prof" "Univ")
  "List containing abbreviations that should be ignored."
  :group 'sentence-navigation
  :type '(repeat :tag "Abbreviations" regexp))

(defcustom sentence-nav-jump-to-syntax t
  "When non-nil, jump to quotes or other markup syntax around sentences.
Otherwise the jump commands will always jump to the capital letter or period."
  :group 'sentence-navigation
  :type 'boolean)

(defcustom sentence-nav-syntax-text-objects nil
  "When non-nil, the behavior of text objects will be based around syntax.
The inner text object will exclude any quotes or syntax while the outer text
object will include any quotes or syntax. This option is only useful for evil
users."
  :group 'sentence-navigation
  :type 'boolean)

;; TODO unfortunately \\s$ and similar don't work in org
(define-arx sentence-nav--rx
  '((left-quote (in "\"“`'"))
    (right-quote (in "\"”`'"))
    ;; characters that can be used for italic, literal, etc. in markdown and org
    (left-markup-char (in "*+/~=_["))
    (right-markup-char (in "*+/~=_]"))
    (0+-sentence-before-chars (0+ (or left-quote left-markup-char)))
    (0+-sentence-after-chars (0+ (or right-quote right-markup-char)))
    (sentence-ending-char (in ".!?…。？"))
    (sentence-final-char (or sentence-ending-char
                             right-quote right-markup-char))
    (maybe-sentence-start (seq 0+-sentence-before-chars upper))
    (maybe-sentence-end (seq sentence-ending-char 0+-sentence-after-chars))
    (bol-ignoring-ws (seq bol (0+ space)))))

(defvar sentence-nav--not-a-sentence nil)
(defun sentence-nav-reload-abbreviations ()
  "Create regexp for a non-sentence from `sentence-nav-abbreviation-list'."
  (setq sentence-nav--not-a-sentence
        (concat
         (sentence-nav--rx (or bol space) 0+-sentence-before-chars)
         "\\(?:"
         (mapconcat #'identity sentence-nav-abbreviation-list "\\|")
         "\\)"
         ;; optional so will work at end or beginning of sentence
         (rx (optional ". ")))))

(sentence-nav-reload-abbreviations)

;; regex for searching forward/backward
(defconst sentence-nav--sentence-search
  (sentence-nav--rx
   (or
    (and maybe-sentence-end (1+ space))
    (and bol (0+ space) (0+ (syntax comment-start)) (0+ space)))
   ;; submatch so can jump directly here
   (submatch maybe-sentence-start)))

(defconst sentence-nav--sentence-end-search
  (sentence-nav--rx (submatch maybe-sentence-end)
                    (or (and " " (optional " ") maybe-sentence-start)
                        (and (0+ space) eol))))

;; helpers
(defun sentence-nav--maybe-at-bol-sentence-p ()
  "Return true when possibly at the start of a sentence at the start of a line.
A helper function for `sentence-nav-forward'."
  (let ((case-fold-search nil))
    (and (looking-back (sentence-nav--rx bol-ignoring-ws)
                       (line-beginning-position))
         (looking-at (sentence-nav--rx maybe-sentence-start)))))

(defun sentence-nav--maybe-at-sentence-end-p ()
  "Return true when possibly at the end of a sentence.
A helper function for `sentence-nav-forward-end' and for
`sentence-nav-backward-end'."
  (looking-at (sentence-nav--rx sentence-final-char (or " " eol))))

(defmacro sentence-nav-incf (var)
  "Like `cl-incf' but nil will be changed to 1."
  `(if ,var
       (cl-incf ,var)
     (setq ,var 1)))

;; actual commands
;;;###autoload
(defun sentence-nav-forward (&optional arg)
  "Move to the start of the next sentence ARG times."
  (interactive "p")
  (let ((final-pos (point))
        count
        case-fold-search)
    ;; move back so don't skip next sentence if right before it
    (when (and (not (looking-at (sentence-nav--rx maybe-sentence-start)))
               (looking-back
                (sentence-nav--rx maybe-sentence-end (0+ space))
                (line-beginning-position)))
      (goto-char (match-beginning 0)))
    (cl-dotimes (_ arg)
      ;; so won't stay on current sentence if at beginning of line
      (when (sentence-nav--maybe-at-bol-sentence-p)
        (forward-char))
      (while (progn
               ;; don't move at all if search fails
               (unless (re-search-forward sentence-nav--sentence-search nil t)
                 (cl-return))
               (goto-char (match-beginning 1))
               (save-match-data
                 (looking-back sentence-nav--not-a-sentence
                               (line-beginning-position)))))
      (sentence-nav-incf count)
      (setq final-pos (if sentence-nav-jump-to-syntax
                          (point)
                        (1- (match-end 1)))))
    (goto-char final-pos)
    count))

;;;###autoload
(defun sentence-nav-backward (&optional arg)
  "Move to the start of the previous sentence ARG times."
  (interactive "p")
  (let ((final-pos (point))
        count
        case-fold-search)
    (cl-dotimes (_ arg)
      (while (progn
               (unless (re-search-backward sentence-nav--sentence-search nil t)
                 (cl-return))
               (goto-char (match-beginning 1))
               (save-match-data
                 (looking-back sentence-nav--not-a-sentence
                               (line-beginning-position)))))
      (sentence-nav-incf count)
      (setq final-pos (if sentence-nav-jump-to-syntax
                          (point)
                        (1- (match-end 1)))))
    (goto-char final-pos)
    count))

;;;###autoload
(defun sentence-nav-forward-end (&optional arg)
  "Move to the start of the next sentence end ARG times."
  (interactive "p")
  (let ((final-pos (point))
        count
        case-fold-search)
    (cl-dotimes (_ arg)
      (while (progn
               ;; if already at a potential sentence end, move past it
               (when (sentence-nav--maybe-at-sentence-end-p)
                 (goto-char (match-end 0)))
               (unless (re-search-forward sentence-nav--sentence-end-search
                                          nil t)
                 (cl-return))
               (goto-char (match-beginning 1))
               (save-match-data
                 (looking-back sentence-nav--not-a-sentence
                               (line-beginning-position)))))
      (sentence-nav-incf count)
      (setq final-pos (if sentence-nav-jump-to-syntax
                          (1- (match-end 1))
                        (point))))
    (goto-char final-pos)
    count))

;;;###autoload
(defun sentence-nav-backward-end (&optional arg)
  "Move to the start of the previous sentence end ARG times."
  (interactive "p")
  (let ((final-pos (point))
        count
        case-fold-search)
    ;; move forward so don't skip previous sentence if right after it
    (skip-chars-forward "[[:blank:]]")
    (when (looking-back (sentence-nav--rx maybe-sentence-end (0+ space))
                        (line-beginning-position))
      (goto-char (1+ (match-end 0))))
    (cl-dotimes (_ arg)
      (while (progn
               (unless (re-search-backward sentence-nav--sentence-end-search
                                           nil t)
                 (cl-return))
               (goto-char (match-beginning 1))
               (save-match-data
                 (looking-back sentence-nav--not-a-sentence
                               (line-beginning-position)))))
      (sentence-nav-incf count)
      (setq final-pos (if sentence-nav-jump-to-syntax
                          (1- (match-end 1))
                        (point))))
    (goto-char final-pos)
    count))

;; add evil motions and text-objects if/when evil loads
;; with-eval-after-load is the 24.4 dependency
(with-eval-after-load 'evil
  (evil-define-motion sentence-nav-evil-forward (count)
    "Move to the start of the COUNT-th next sentence."
    :jump t
    :type exclusive
    ;; TODO
    ;; (evil-signal-at-bob-or-eob count)
    (sentence-nav-forward (or count 1)))

  (evil-define-motion sentence-nav-evil-backward (count)
    "Move to the start of the COUNT-th previous sentence."
    :jump t
    :type exclusive
    ;; (evil-signal-at-bob-or-eob (- (or count 1)))
    (sentence-nav-backward (or count 1)))

  (evil-define-motion sentence-nav-evil-forward-end (count)
    "Move to the end of the COUNT-th next sentence."
    :jump t
    :type inclusive
    (sentence-nav-forward-end (or count 1)))

  (evil-define-motion sentence-nav-evil-backward-end (count)
    "Move to the end of the COUNT-th previous sentence."
    :jump t
    :type inclusive
    (sentence-nav-backward-end (or count 1)))

  (put 'sentence-nav-evil-inner-sentence 'beginning-op
       (lambda (&optional count)
         (let ((sentence-nav-jump-to-syntax
                (not sentence-nav-syntax-text-objects)))
           (sentence-nav-evil-backward count))))
  (put 'sentence-nav-evil-inner-sentence 'forward-op
       (lambda (&optional count)
         (let ((sentence-nav-jump-to-syntax
                (not sentence-nav-syntax-text-objects)))
           (sentence-nav-evil-forward-end count))
         (forward-char)))

  (put 'sentence-nav-evil-a-sentence 'beginning-op
       (lambda (&optional count)
         (let ((sentence-nav-jump-to-syntax t))
           (sentence-nav-evil-backward count))))
  (put 'sentence-nav-evil-a-sentence 'forward-op
       (lambda (&optional count)
         (let ((sentence-nav-jump-to-syntax t))
           (sentence-nav-evil-forward-end count)
           (forward-char)
           (skip-chars-forward "[[:blank:]]"))))

  (evil-define-text-object sentence-nav-evil-inner-sentence
    (count &optional beg end type)
    "Select a sentence excluding spaces after it."
    (evil-select-inner-object 'sentence-nav-evil-inner-sentence
                              beg end type count))

  (evil-define-text-object sentence-nav-evil-a-sentence
    (count &optional beg end type)
    "Select a sentence up to the start of the next sentence after it."
    (evil-select-inner-object 'sentence-nav-evil-a-sentence beg end type count)))

;;;###autoload
(with-eval-after-load 'evil
  (autoload 'sentence-nav-evil-forward "sentence-navigation" nil t)
  (autoload 'sentence-nav-evil-forward-end "sentence-navigation" nil t)
  (autoload 'sentence-nav-evil-backward "sentence-navigation" nil t)
  (autoload 'sentence-nav-evil-backward-end "sentence-navigation" nil t)
  (autoload 'sentence-nav-evil-a-sentence "sentence-navigation" nil t)
  (autoload 'sentence-nav-evil-inner-sentence "sentence-navigation" nil t))

(provide 'sentence-navigation)
;;; sentence-navigation.el ends here
