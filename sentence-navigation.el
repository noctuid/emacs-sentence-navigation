;;; sentence-navigation.el --- Commands to navigate one-spaced sentences.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/emacs-sentence-navigation
;; Keywords: sentence evil
;; Package-Requires: ((ample-regexps "0.1") (emacs "24.4"))
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

(defgroup sentence-navigation nil
  "Gives commands for navigating sentences and evil text objects for manipulating them."
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
  :type 'list)

(define-arx sentence-nav--rx
  '((left-quote (in "\"“`'"))
    (right-quote (in "\"”`'"))
    ;; characters that can be used for italic, literal, etc. in markdown and org
    (left-markup-char (in "*+/~=_["))
    (right-markup-char (in "*+/~=_]"))
    (0+-sentence-before-chars (0+ (or left-quote left-markup-char)))
    (0+-sentence-after-chars (0+ (or right-quote right-markup-char)))
    (sentence-ending-char (in ".!?…。？"))
    (sentence-final-char (or sentence-ending-char right-quote right-markup-char))
    (maybe-sentence-start (seq 0+-sentence-before-chars upper))
    (maybe-sentence-end (seq sentence-ending-char 0+-sentence-after-chars))
    (bol-ignoring-ws (seq bol (0+ space)))))

(defvar sentence-nav--not-a-sentence nil)
(defun sentence-nav-reload-abbreviations ()
  (setq sentence-nav--not-a-sentence
        (concat
         (sentence-nav--rx (or bol " ") (0+ (in "\(\"'`“")))
         "\\("
         (mapconcat (lambda (x) x) sentence-nav-abbreviation-list "\\|")
         "\\)"
         ;; optional so will work at end or beginning of sentence
         (sentence-nav--rx (optional ". ")))))

(sentence-nav-reload-abbreviations)

;; regex for searching forward/backward
(defconst sentence-nav--maybe-sentence-search
  (sentence-nav--rx (or
           (and maybe-sentence-end " " (optional " "))
           ;; non-precise but hopefully comprehensive way to deal with comments
           (and bol (0+ space) (and (0+ (not letter)) (1+ space))))
          maybe-sentence-start))

(defconst sentence-nav--maybe-sentence-end-search
  (sentence-nav--rx maybe-sentence-end
          (or
           (and " " (optional " ")
                maybe-sentence-start)
           (and (0+ space) eol))))

;; helpers for correcting positioning
(defconst sentence-nav--maybe-sentence-start (sentence-nav--rx maybe-sentence-start))
(defconst sentence-nav--maybe-sentence-end (sentence-nav--rx maybe-sentence-end))

(defun sentence-nav--maybe-at-bol-sentence-p ()
  (let ((case-fold-search nil))
    (and (looking-back (sentence-nav--rx bol-ignoring-ws))
         (looking-at sentence-nav--maybe-sentence-start))))

(defun sentence-nav--maybe-before-sentence-start-p ()
  (let ((case-fold-search nil))
    (and
     (looking-at sentence-nav--maybe-sentence-start)
     (looking-back (sentence-nav--rx (or " "
                                         (and bol (0+ space))))))))

(defun sentence-nav--maybe-at-sentence-end-p ()
  (looking-at (sentence-nav--rx sentence-final-char (or " " eol))))

;; actual commands
(defun sentence-nav-forward (&optional arg)
  "Move to the start of the next sentence ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    ;; save point in case there is no match
    (point-to-register 'sentence-nav-saved-point)
    ;; so won't stay on current sentence if at beginning of line
    (when (sentence-nav--maybe-at-bol-sentence-p)
      (right-char))
    (while (let ((case-fold-search nil))
             ;; move back so that don't skip next sentence if right before it
             (while (looking-back sentence-nav--maybe-sentence-end)
               (left-char))
             ;; don't move at all if search fails
             (unless (re-search-forward sentence-nav--maybe-sentence-search nil t)
               (jump-to-register 'sentence-nav-saved-point)
               (return nil))
             (while (not (sentence-nav--maybe-before-sentence-start-p))
               (left-char))
             (looking-back sentence-nav--not-a-sentence)))))

(defun sentence-nav-forward-end (&optional arg)
  "Move to the start of the next sentence end ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sentence-nav-saved-point)
    (while (let ((case-fold-search nil))
             ;; move to start of next possible sentence
             ;; if already at end of current or after an abbrev
             (when (sentence-nav--maybe-at-sentence-end-p)
               (unless (re-search-forward sentence-nav--maybe-sentence-start nil t)
                 ;; prevents infinite loop at end of file after abbrev
                 ;; because the next search will actually succeed in this case
                 (return nil)))
             (unless (re-search-forward sentence-nav--maybe-sentence-end-search nil t)
               (jump-to-register 'sentence-nav-saved-point)
               (return nil))
             (while (not (sentence-nav--maybe-at-sentence-end-p))
               (left-char))
             (looking-back sentence-nav--not-a-sentence)))))

(defun sentence-nav-backward (&optional arg)
  "Move to the start of the previous sentence ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (while (let ((case-fold-search nil))
             (unless (re-search-backward sentence-nav--maybe-sentence-search nil t)
               (return nil))
             (while (not (looking-at sentence-nav--maybe-sentence-start))
               (right-char))
             (looking-back sentence-nav--not-a-sentence)))))

(defun sentence-nav-backward-end (&optional arg)
  "Move to the start of the previous sentence end ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sentence-nav-saved-point)
    ;; move forward so don't skip prevous sentence if right in front of it
    (while (let ((case-fold-search nil))
             (looking-at sentence-nav--maybe-sentence-start))
      (right-char))
    (while (let ((case-fold-search nil))
             (unless (re-search-backward sentence-nav--maybe-sentence-end-search nil t)
               (jump-to-register 'sentence-nav-saved-point)
               (return nil))
             (while (not (sentence-nav--maybe-at-sentence-end-p))
               (right-char))
      (looking-back sentence-nav--not-a-sentence)))))

;; add evil motions and text-objects if/when evil loads
(with-eval-after-load 'evil
  (evil-define-motion sentence-nav-evil-forward (count)
    "Move to the start of the COUNT-th next sentence."
    (sentence-nav-forward (or count 1)))

  (evil-define-motion sentence-nav-evil-forward-end (count)
    "Move to the end of the COUNT-th next sentence."
    (sentence-nav-forward-end (or count 1)))

  (evil-define-motion sentence-nav-evil-backward (count)
    "Move to the start of the COUNT-th previous sentence."
    (sentence-nav-backward (or count 1)))

  (evil-define-motion sentence-nav-evil-backward-end (count)
    "Move to the end of the COUNT-th privous sentence."
    (sentence-nav-backward-end (or count 1)))

  (put 'sentence-nav-evil-a-sentence 'beginning-op
       'sentence-nav-evil-backward)
  (put 'sentence-nav-evil-a-sentence 'forward-op
       'sentence-nav-evil-forward)

  (put 'sentence-nav-evil-inner-sentence 'beginning-op
       'sentence-nav-evil-backward-sentence)
  (put 'sentence-nav-evil-inner-sentence
       'forward-op (lambda (count)
                     (sentence-nav-evil-forward-end count)
                     (right-char)))

  (evil-define-text-object sentence-nav-evil-outer-sentence (count &optional beg end type)
    "Select a sentence including spaces after it."
    (evil-select-inner-object 'sentence-nav-evil-a-sentence beg end type count))

  (evil-define-text-object sentence-nav-evil-inner-sentence (count &optional beg end type)
    "Select a sentence excluding spaces after it."
    (evil-select-inner-object 'sentence-nav-evil-inner-sentence beg end type count)))

(provide 'sentence-navigation)
;;; sentence-navigation.el ends here
