;;; Commentary:
;;
;; Gives commands for sentence navigation ignoring abbreviations and such.
;;
;; The regex is modified from vim-textobj-sentence's regex.

;;; Code:
(require 'cl-lib)

(setq sn/non-sentence-list
      '("[ABCDIMPSUabcdegimpsv]"
        "l[ab]" "[eRr]d" "Ph" "[Ccp]l" "[Ll]n" "[c]o"
        "[Oe]p" "[DJMSh]r" "[MVv]s" "[CFMPScfpw]t"
        "alt" "[Ee]tc" "div" "es[pt]" "[Ll]td" "min"
        "[MD]rs" "[Aa]pt" "[Aa]ve?" "[Ss]tr?" "e\\.g"
        "[Aa]ssn" "[Bb]lvd" "[Dd]ept" "incl" "Inst" "Prof" "Univ"))

(defun sn/reload-non-sentence-regex ()
  (setq not-a-sentence
        (concat
         " [\(\"'`“]?\\("
         (cl-reduce #'(lambda (x y) (concat x "\\|" y)) sn/non-sentence-list)
         "\\)\\. ")))

(sn/reload-non-sentence-regex)

(setq sn/maybe-sentence-regex "\\(\\.[\"”`]?  ?\\|^[[:space:]]*\\)[[:upper:]\"“`]")

(defun sn/forward-sentence (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    ;; so won't stay on current sentence if at beginning of line
    (when (and (looking-at "^[[:space:]]*[[:upper:]\"“`]")
               (looking-at "[[:upper:]\"“`]"))
      (right-char))
    (while (progn
             (let ((case-fold-search nil))
               (re-search-forward sn/maybe-sentence-regex)
               (while (not (and (looking-at "[[:upper:]\"“`]")
                                (looking-back "\\(\\.[\"”`]?  ?\\|^[[:space:]]*\\)")))
                 (left-char)))
             (looking-back not-a-sentence)))))

(defun sn/forward-sentence-end (&optional arg)
  (interactive)
  ;; wtf elisp, no default args
  (sn/forward-sentence (or (when arg (+ 1 arg)) 2))
  (while (not (looking-at "\\."))
    (left-char)))

(defun sn/backward-sentence (&optional arg)
  (interactive)
  (dotimes (_ (or arg 1))
    (while (progn
             (let ((case-fold-search nil))
               (re-search-backward sn/maybe-sentence-regex)
               (while (looking-at "[\\.\"”` ]")
                 (right-char)))
             (looking-back not-a-sentence)))))

(defun sn/backward-sentence-end (&optional arg)
  (interactive)
  (sn/backward-sentence (or arg 1))
  (while (not (looking-at "\\."))
    (left-char)))

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
  (put 'sn/evil-inner-sentence 'forward-op 'sn/evil-forward-sentence-end)

  (evil-define-text-object sn/evil-outer-sentence (count &optional beg end type)
    (evil-select-inner-object 'sn/evil-a-sentence beg end type count))

  (evil-define-text-object sn/evil-inner-sentence (count &optional beg end type)
    (evil-select-inner-object 'sn/evil-inner-sentence beg end type count)))

;; (defun sn/start-of-sentence-p ()
;;   (and (looking-back "\\(\\.  ?\\|^[[:space:]]*\\)")
;;        (looking-at "[[:upper:]\"“`]")
;;        (not (looking-back not-a-sentence))))

;; (defun sn/sentence-begin ()
;;   (interactive)
;;   (unless (sn/start-of-sentence-p)
;;     (sn/backward-sentence)))

(provide 'sentence-navigation)
