;;; Commentary:
;;
;; Gives commands for sentence navigation ignoring abbreviations and such.
;;
;; The regex is modified from vim-textobj-sentence's regex.

;;; Code:
(require 'cl-lib)

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

(defvar sn--not-a-sentence nil)
(defun sn/reload-non-sentence-regex ()
  (setq sn--not-a-sentence
        (concat
         " [\(\"'`“]?\\("
         (cl-reduce #'(lambda (x y) (concat x "\\|" y)) sn/abbreviation-list)
         "\\)\\. ")))

(sn/reload-non-sentence-regex)

(defvar sn--maybe-sentence-regex
  "\\(\\.[\"”`]?  ?\\|^[[:space:]]*\\)[[:upper:]\"“`]")
(defvar sn--maybe-sentence-end "\\.[\"”`]?")
(defvar sn--maybe-after-sentence-end-regex "\\.[\"”`]?\\( \\|$\\)")

(defun sn/forward-sentence (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    ;; save point in case there is no match
    (point-to-register 'sn-saved-point)
    ;; so won't stay on current sentence if at beginning of line
    (when (and (looking-at "^[[:space:]]*[[:upper:]\"“`]")
               (looking-at "[[:upper:]\"“`]"))
      (right-char))
    (while (progn
             (let ((case-fold-search nil))
               ;; move back so that don't skip next sentence if right before it
               (while (looking-back sn--maybe-sentence-end)
                 (left-char))
               ;; don't move at all if search fails
               (unless (re-search-forward sn--maybe-sentence-regex nil t)
                 (jump-to-register 'sn-saved-point)
                 (return nil))
               (while (not (and (looking-at "[[:upper:]\"“`]")
                                (looking-back "\\(\\.[\"”`]?  ?\\|^[[:space:]]*\\)")))
                 (left-char)))
             (looking-back sn--not-a-sentence)))))

(defun sn/forward-sentence-end (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sn-saved-point)
    (while (progn
             ;; move to start of next possible sentence
             ;; if already at end of current or after an abbrev
             (when (and (looking-at "[\\.”\"`]")
                        ;; " is ambigious for sentence start or end
                        (not (looking-at "[\"`][[:upper:]]")))
               (re-search-forward sn--maybe-sentence-regex nil t))
             (unless (re-search-forward sn--maybe-after-sentence-end-regex nil t)
               (jump-to-register 'sn-saved-point)
               (return nil))
             (while (not (looking-at "[\\.\"”`]"))
               (left-char))
             (looking-back (concat (cl-subseq sn--not-a-sentence 0 -1) "?"))))))

(defun sn/backward-sentence (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    (while (progn
             (let ((case-fold-search nil))
               (unless (re-search-backward sn--maybe-sentence-regex nil t)
                 (return nil))
               (while (and
                       (looking-at "[\\.\"”`[:space:]]")
                       (not (looking-at "[\"“`][[:upper:]]")))
                 (right-char)))
             (looking-back sn--not-a-sentence)))))

(defun sn/backward-sentence-end (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    (point-to-register 'sn-saved-point)
    ;; move forward so don't skip prevous sentence if right in front of it
    (while (and (looking-at "[[:upper:]\"“`]")
                (not (looking-at "[\"`]$")))
      (right-char))
    (while (progn
             (unless (re-search-backward sn--maybe-after-sentence-end-regex nil t)
               (jump-to-register 'sn-saved-point)
               (return nil))
             (when (looking-at "\\.[\"”`]")
               (right-char))
             (looking-back (cl-subseq sn--not-a-sentence 0 -3))))))

;; add evil motions and text-objects if evil exists
(when (require 'evil nil :noerror)

  (evil-define-motion sn/evil-forward-sentence (count)
    (sn/forward-sentence (or count 1)))

  (evil-define-motion sn/evil-forward-sentence-end (count)
    (sn/forward-sentence-end (or count 1)))

  (evil-define-motion sn/evil-backward-sentence (count)
    (sn/backward-sentence (or count 1)))

  (evil-define-motion sn/evil-backward-sentence-end (count)
    (sn/backward-sentence-end (or count 1)))

  (put 'sn/evil-a-sentence 'beginning-op 'sn/evil-backward-sentence)
  (put 'sn/evil-a-sentence 'forward-op   'sn/evil-forward-sentence)

  (put 'sn/evil-inner-sentence 'beginning-op 'sn/evil-backward-sentence)
  (put 'sn/evil-inner-sentence 'forward-op (lambda (count)
                                             (sn/evil-forward-sentence-end count)
                                             (right-char)))

  (evil-define-text-object sn/evil-outer-sentence (count &optional beg end type)
    (evil-select-inner-object 'sn/evil-a-sentence beg end type count))

  (evil-define-text-object sn/evil-inner-sentence (count &optional beg end type)
    (evil-select-inner-object 'sn/evil-inner-sentence beg end type count)))

;; (defun sn/start-of-sentence-p ()
;;   (and (looking-back "\\(\\.  ?\\|^[[:space:]]*\\)")
;;        (looking-at "[[:upper:]\"“`]")
;;        (not (looking-back sn/not-a-sentence))))

;; (defun sn/sentence-begin ()
;;   (interactive)
;;   (unless (sn/start-of-sentence-p)
;;     (sn/backward-sentence)))

(provide 'sentence-navigation)
