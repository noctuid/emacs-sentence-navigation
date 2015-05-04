;;; sentence-navigation.el --- Commands to navigate one-spaced sentences.

;; Author: Lit Wakefield <nocturnal.artifice@gmail.com>
;; URL: https://github.com/angelic-sedition/emacs-sentence-navigation
;; Keywords: sentence evil
;; Package-Requires: ((cl-lib "0.5") (ample-regexps "0.1") (emacs "24.4"))
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
(require 'cl-lib)
(eval-and-compile (require 'ample-regexps))

(defgroup sentence-navigation nil
  "Gives commands for navigating sentences and evil text objects for manipulating them."
  :group 'editing
  :prefix 'sn/)

(defcustom sn/abbreviation-list
  '("[ABCDIMPSUabcdegimpsv]"
    "l[ab]" "[eRr]d" "Ph" "[Ccp]l" "[Ll]n" "[c]o"
    "[Oe]p" "[DJMSh]r" "[MVv]s" "[CFMPScfpw]t"
    "alt" "[Ee]tc" "div" "es[pt]" "[Ll]td" "min"
    "[MD]rs" "[Aa]pt" "[Aa]ve?" "[Ss]tr?" "e\\.g"
    "[Aa]ssn" "[Bb]lvd" "[Dd]ept" "incl" "Inst" "Prof" "Univ")
  "List containing abbreviations that should be ignored."
  :group 'sentence-navigation
  :type 'list)

(define-arx sn--rx
  '((left-quotes (in "\"“`'"))
    (right-quotes (in "\"”`'"))
    (0+-left-quotes (0+ left-quotes))
    (0+-right-quotes (0+ right-quotes))
    (sentence-ending-char (in ".!?…。？"))
    (sentence-final-char (or sentence-ending-char right-quotes))
    (maybe-sentence-start (seq 0+-left-quotes upper))
    (maybe-sentence-end (seq sentence-ending-char 0+-right-quotes))
    (bol-ignoring-ws (seq bol (0+ space)))))

(defvar sn--not-a-sentence nil)
(defun sn/reload-non-sentence-regex ()
  (setq sn--not-a-sentence
        (concat
         (sn--rx (or bol " ") (0+ (in "\(\"'`“")))
         "\\("
         (cl-reduce #'(lambda (x y) (concat x "\\|" y)) sn/abbreviation-list)
         "\\)"
         ;; optional so will work at end or beginning of sentence
         (sn--rx (optional ". ")))))

(sn/reload-non-sentence-regex)

;; regex for searching forward/backward
(defconst sn--maybe-sentence-search
  (sn--rx (or
           (and maybe-sentence-end " " (optional " "))
           ;; non-precise but hopefully comprehensive way to deal with comments
           (and bol (0+ space) (0+ (not letter)) (0+ space)))
          maybe-sentence-start))

(defconst sn--maybe-sentence-end-search
  (sn--rx maybe-sentence-end
          (or
           (and " " (optional " ")
                maybe-sentence-start)
           (and (0+ space) eol))))

;; helpers for correcting positioning
(defconst sn--maybe-sentence-start (sn--rx maybe-sentence-start))
(defconst sn--maybe-sentence-end (sn--rx maybe-sentence-end))

(defun sn--maybe-at-bol-sentence-p ()
  (let ((case-fold-search nil))
    (and (looking-back (sn--rx bol-ignoring-ws))
         (looking-at sn--maybe-sentence-start))))

(defun sn--maybe-before-sentence-start-p ()
  (let ((case-fold-search nil))
    (and
     (looking-at sn--maybe-sentence-start)
     (looking-back (sn--rx (or " " bol))))))

(defun sn--maybe-at-sentence-end-p ()
  (looking-at (sn--rx sentence-final-char (or " " eol))))

;; actual commands
(defun sn/forward-sentence (&optional arg)
  "Move to the start of the next sentence ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    ;; save point in case there is no match
    (point-to-register 'sn-saved-point)
    ;; so won't stay on current sentence if at beginning of line
    (when (sn--maybe-at-bol-sentence-p)
      (right-char))
    (while (let ((case-fold-search nil))
             ;; move back so that don't skip next sentence if right before it
             (while (looking-back sn--maybe-sentence-end)
               (left-char))
             ;; don't move at all if search fails
             (unless (re-search-forward sn--maybe-sentence-search nil t)
               (jump-to-register 'sn-saved-point)
               (return nil))
             (while (not (sn--maybe-before-sentence-start-p))
               (left-char))
             (looking-back sn--not-a-sentence)))))

(defun sn/forward-sentence-end (&optional arg)
  "Move to the start of the next sentence end ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sn-saved-point)
    (while (let ((case-fold-search nil))
             ;; move to start of next possible sentence
             ;; if already at end of current or after an abbrev
             (when (sn--maybe-at-sentence-end-p)
               (unless (re-search-forward sn--maybe-sentence-start nil t)
                 ;; prevents infinite loop at end of file after abbrev
                 (return nil)))
             (unless (re-search-forward sn--maybe-sentence-end-search nil t)
               (jump-to-register 'sn-saved-point)
               (return nil))
             (while (not (sn--maybe-at-sentence-end-p))
               (left-char))
             (looking-back sn--not-a-sentence)))))

(defun sn/backward-sentence (&optional arg)
  "Move to the start of the previous sentence ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (while (let ((case-fold-search nil))
             (unless (re-search-backward sn--maybe-sentence-search nil t)
               (return nil))
             (while (not (looking-at sn--maybe-sentence-start))
               (right-char))
             (looking-back sn--not-a-sentence)))))

(defun sn/backward-sentence-end (&optional arg)
  "Move to the start of the previous sentence end ARG times."
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sn-saved-point)
    ;; move forward so don't skip prevous sentence if right in front of it
    (while (let ((case-fold-search nil))
             (looking-at sn--maybe-sentence-start))
      (right-char))
    (while (let ((case-fold-search nil))
             (unless (re-search-backward sn--maybe-sentence-end-search nil t)
               (jump-to-register 'sn-saved-point)
               (return nil))
             (while (not (sn--maybe-at-sentence-end-p))
               (right-char))
      (looking-back sn--not-a-sentence)))))

;; add evil motions and text-objects if/when evil loads
(with-eval-after-load 'evil
  (evil-define-motion sn/evil-forward-sentence (count)
    "Move to the start of the COUNT-th next sentence."
    (sn/forward-sentence (or count 1)))

  (evil-define-motion sn/evil-forward-sentence-end (count)
    "Move to the end of the COUNT-th next sentence."
    (sn/forward-sentence-end (or count 1)))

  (evil-define-motion sn/evil-backward-sentence (count)
    "Move to the start of the COUNT-th previous sentence."
    (sn/backward-sentence (or count 1)))

  (evil-define-motion sn/evil-backward-sentence-end (count)
    "Move to the end of the COUNT-th privous sentence."
    (sn/backward-sentence-end (or count 1)))

  (put 'sn/evil-a-sentence 'beginning-op 'sn/evil-backward-sentence)
  (put 'sn/evil-a-sentence 'forward-op   'sn/evil-forward-sentence)

  (put 'sn/evil-inner-sentence 'beginning-op 'sn/evil-backward-sentence)
  (put 'sn/evil-inner-sentence 'forward-op (lambda (count)
                                             (sn/evil-forward-sentence-end count)
                                             (right-char)))

  (evil-define-text-object sn/evil-outer-sentence (count &optional beg end type)
    "Select a sentence including spaces after it."
    (evil-select-inner-object 'sn/evil-a-sentence beg end type count))

  (evil-define-text-object sn/evil-inner-sentence (count &optional beg end type)
    "Select a sentence excluding spaces after it."
    (evil-select-inner-object 'sn/evil-inner-sentence beg end type count)))

(provide 'sentence-navigation)
;;; sentence-navigation.el ends here
