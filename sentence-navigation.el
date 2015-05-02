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
         (rx (or bol " ") (0+ (in "\(\"'`“")))
         "\\("
         (cl-reduce #'(lambda (x y) (concat x "\\|" y)) sn/abbreviation-list)
         "\\)"
         ;; optional so will work at end or beginning of sentence
         (rx (optional ". ")))))

(sn/reload-non-sentence-regex)

;; as suggested by emacs wiki
(defmacro sn--rx-extra (&rest body-forms)
   (let ((add-ins (list
                   ;; wish could use previous ones..
                   `(left-quotes . ,(rx (in "\"“`'")))
                   `(sentence-end-char . ,(rx (in ".\"”`'")))
                   `(0+-left-quotes . ,(rx (0+ (in "\"“`'"))))
                   `(0+-right-quotes . ,(rx (0+ (in "\"”`'"))))
                   `(maybe-sentence-start . ,(rx (0+ (in "\"“`'"))
                                                 upper))
                   `(maybe-sentence-end . ,(rx "." (0+ (in "\"”`'"))))
                   `(bol-ignoring-ws . ,(rx bol (0+ space))))))
     `(let ((rx-constituents (append ',add-ins rx-constituents nil)))
        ,@body-forms)))

;; regex for searching forward/backward
(defvar sn--maybe-sentence-search
      (sn--rx-extra (rx (or
                         (and maybe-sentence-end " " (optional " "))
                         ;; non-precise but hopefully comprehensive way to deal with comments
                         (and bol (0+ space) (0+ (not letter)) (0+ space)))
                        maybe-sentence-start)))

(defvar sn--maybe-sentence-end-search
      (sn--rx-extra (rx maybe-sentence-end
                        (or
                         (and " " (optional " ")
                              maybe-sentence-start)
                         (and (0+ space) eol)))))

;; helpers for correcting positioning
(defvar sn--maybe-sentence-start (sn--rx-extra (rx maybe-sentence-start)))
(defvar sn--maybe-sentence-end (sn--rx-extra (rx maybe-sentence-end)))

(defun sn--maybe-at-bol-sentence-p ()
  (let ((case-fold-search nil))
    (and (looking-back (sn--rx-extra (rx bol-ignoring-ws)))
         (looking-at sn--maybe-sentence-start))))

(defun sn--maybe-before-sentence-start-p ()
  (let ((case-fold-search nil))
    (and
     (looking-at sn--maybe-sentence-start)
     (looking-back (rx (or " " bol))))))

(defun sn--maybe-at-sentence-end-p ()
  (looking-at (sn--rx-extra (rx sentence-end-char (or " " eol)))))

;; actual commands
(defun sn/forward-sentence (&optional arg)
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
  (interactive)
  (dotimes (_ (or arg 1))
    (while (let ((case-fold-search nil))
             (unless (re-search-backward sn--maybe-sentence-search nil t)
               (return nil))
             (while (not (looking-at sn--maybe-sentence-start))
               (right-char))
             (looking-back sn--not-a-sentence)))))

(defun sn/backward-sentence-end (&optional arg)
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

(provide 'sentence-navigation)
