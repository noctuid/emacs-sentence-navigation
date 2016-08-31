;;; sentence-navigation-tests.el --- Tests for sentence-navgiation.el.
;;; Commentary:
;;; Code:
(require 'sentence-navigation)
(require 'org)
(require 'evil)

(defmacro sentence-nav-with (in &rest body)
  "This is `lispy-with' modified for sentence-navigation.
Note that | is considered to be \"on\" a character, meaning that it is included
in a visual selection. ~ on the other hand is not considered to be on a
character, so when it represents the region end, the character before/after it
is not considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (org-mode)
             (transient-mark-mode 1)
             (evil-mode)
             (define-key evil-motion-state-map ")" #'sentence-nav-evil-forward)
             (define-key evil-motion-state-map "(" #'sentence-nav-evil-backward)
             (define-key evil-motion-state-map
               "g)" #'sentence-nav-evil-forward-end)
             (define-key evil-motion-state-map
               "g(" #'sentence-nav-evil-backward-end)
             (define-key evil-outer-text-objects-map
               "s" #'sentence-nav-evil-a-sentence)
             (define-key evil-inner-text-objects-map
               "s" #'sentence-nav-evil-inner-sentence)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             (setq current-prefix-arg nil)
             ,@(mapcar (lambda (x)
                         (if (or (stringp x)
                                 (and (listp x)
                                      (eq (car x) 'kbd)))
                             `(evil-execute-macro 1 ,x)
                           x))
                       body)
             (insert "|")
             (when (region-active-p)
               (exchange-point-and-mark)
               ;; because not considering ~ as "on" like |
               (when (= (point) (region-end))
                 (forward-char))
               (insert "~"))
             (buffer-substring-no-properties
              (point-min)
              (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(ert-deftest sentence-nav-forward ()
  ;; basic case
  (should (string= (sentence-nav-with "|This is a sentence. This is another."
                     ")")
                   "This is a sentence. |This is another."))
  ;; two-spaced
  (should (string= (sentence-nav-with "|This is a sentence.  This is another."
                     ")")
                   "This is a sentence.  |This is another."))
  ;; sentence end
  (should (string= (sentence-nav-with "This is a sentence.| This is too."
                     ")")
                   "This is a sentence. |This is too."))
  ;; in between two-spaced
  (should (string= (sentence-nav-with "This is a sentence. | This is too."
                     ")")
                   "This is a sentence.  |This is too."))
  ;; abbreviation
  (should (string= (sentence-nav-with "|Mr. Bob likes pie. I like pie."
                     ")")
                   "Mr. Bob likes pie. |I like pie."))
  ;; multi-line
  (should (string= (sentence-nav-with "|\nHello world." ")")
                   "\n|Hello world."))
  (should (string= (sentence-nav-with "|* Org Heading\nHello bird." ")")
                   "* Org Heading\n|Hello bird."))
  ;; quotes/special syntax
  (should (string= (sentence-nav-with "|Sentence one. \"Sentence two.\"" ")")
                   "Sentence one. |\"Sentence two.\""))
  (should (string= (sentence-nav-with "|Sentence one. /“Sentence two.”/" ")")
                   "Sentence one. |/“Sentence two.”/"))
  (let (sentence-nav-jump-to-syntax)
    (should (string= (sentence-nav-with "|Sentence one. \"Sentence two.\"" ")")
                     "Sentence one. \"|Sentence two.\""))
    (should (string= (sentence-nav-with "|Sentence one. /“Sentence two.”/" ")")
                     "Sentence one. /“|Sentence two.”/")))
  ;; comment
  (should (string= (sentence-nav-with "|;; A commented sentence."
                     (emacs-lisp-mode)
                     ")")
                   ";; |A commented sentence."))
  ;; check that doesn't move on failure
  (should (string= (sentence-nav-with "|Mr. Smith says this is one sentence."
                     ")")
                   "|Mr. Smith says this is one sentence."))
  ;; counts
  (should (string= (sentence-nav-with "|First. Second. Third. Fourth." "2)")
                   "First. Second. |Third. Fourth."))
  (should (string= (sentence-nav-with "|One. Two." "99)")
                   "One. |Two."))
  ;; test sentence-nav-hard-wrapping
  (let ((sentence-nav-hard-wrapping t))
    ;; sentences
    (should (string= (sentence-nav-with "|Sentence 1.\n\nSentence 2." ")")
                     "Sentence 1.\n\n|Sentence 2."))
    (should (string= (sentence-nav-with "|* Org Heading\nA sentence." ")")
                     "* Org Heading\n|A sentence."))
    (should (string= (sentence-nav-with "|Full sentence 1.\nFull sentence 2" ")")
                     "Full sentence 1.\n|Full sentence 2"))
    ;; non-sentences
    (should (string= (sentence-nav-with "|Split\nSentence." ")")
                     "|Split\nSentence."))
    (should (string= (sentence-nav-with "|Mr.\nSmith's sentence got split." ")")
                     "|Mr.\nSmith's sentence got split."))))

(ert-deftest sentence-nav-backward ()
  ;; basic case
  (should (string= (sentence-nav-with "Sentence 1. |Sentence 2." "(")
                   "|Sentence 1. Sentence 2."))
  ;; two-spaced
  (should (string= (sentence-nav-with "Sentence 1.  |Sentence 2." "(")
                   "|Sentence 1.  Sentence 2."))
  ;; just after sentence start
  (should (string= (sentence-nav-with "W|oah." "(")
                   "|Woah."))
  ;; multi-line
  (should (string= (sentence-nav-with "* Org Heading\nHello bird.|" "(")
                   "* Org Heading\n|Hello bird."))
  ;; abbreviation
  (should (string= (sentence-nav-with "I am not Dr. Brown|." "(")
                   "|I am not Dr. Brown."))
  ;; quotes/special syntax
  (should (string= (sentence-nav-with "\"A sentence.\"|" "(")
                   "|\"A sentence.\""))
  (should (string= (sentence-nav-with "/“A sentence.”/|" "(")
                   "|/“A sentence.”/"))
  (let (sentence-nav-jump-to-syntax)
    (should (string= (sentence-nav-with "\"A sentence.\"|" "(")
                     "\"|A sentence.\""))
    (should (string= (sentence-nav-with "/“A sentence.”/|" "(")
                     "/“|A sentence.”/")))
  ;; comment
  (should (string= (sentence-nav-with ";; A commented sentence.|"
                     (emacs-lisp-mode)
                     "(")
                   ";; |A commented sentence."))
  ;; check that doesn't move on failure
  (should (string= (sentence-nav-with "incomplete sentence. |Full sentence."
                     "(")
                   "incomplete sentence. |Full sentence."))
  ;; shouldn't error
  (should (string= (sentence-nav-with "|Bob." "(")
                   "|Bob."))
  ;; counts
  (should (string= (sentence-nav-with "First. Second. Third. Fourth|." "2(")
                   "First. Second. |Third. Fourth."))
  (should (string= (sentence-nav-with "One. |Two." "99(")
                   "|One. Two.")))

(ert-deftest sentence-nav-forward-end ()
  (should (string= (sentence-nav-with "|One fish. Two fish." "g)")
                   "One fish|. Two fish."))
  ;; case where already at sentence end; should move to next
  (should (string= (sentence-nav-with "Red fish|. Blue fish." "g)")
                   "Red fish. Blue fish|."))
  ;; at abbrev
  (should (string= (sentence-nav-with
                       "Gold Ave|. is a place. I have not been there."
                     "g)")
                   "Gold Ave. is a place|. I have not been there."))
  ;; abbrev
  (should (string= (sentence-nav-with
                       "|Gold Ave. is a place. I have not been there."
                     "g)")
                   "Gold Ave. is a place|. I have not been there."))
  ;; final sentence end; shouldn't move or error
  (should (string= (sentence-nav-with "End|. junk text" "g)")
                   "End|. junk text"))
  (should (string= (sentence-nav-with "End|." "g)")
                   "End|."))
  ;; two-spaced
  (should (string= (sentence-nav-with "|Black fish.  Blue fish." "g)")
                   "Black fish|.  Blue fish."))
  ;; just before sentence end
  (should (string= (sentence-nav-with "Old fis|h. New fish." "g)")
                   "Old fish|. New fish."))
  ;; quotes/special syntax
  (should (string= (sentence-nav-with "|\"A sentence.\"" "g)")
                   "\"A sentence.|\""))
  (should (string= (sentence-nav-with "|/“A sentence.”/" "g)")
                   "/“A sentence.”|/"))
  (let (sentence-nav-jump-to-syntax)
    (should (string= (sentence-nav-with "|\"A sentence.\"" "g)")
                     "\"A sentence|.\""))
    (should (string= (sentence-nav-with "|/“A sentence.”/" "g)")
                     "/“A sentence|.”/")))
  ;; counts
  (should (string= (sentence-nav-with "|First. Second. Third. Fourth." "2g)")
                   "First. Second|. Third. Fourth."))
  (should (string= (sentence-nav-with "|One. Two." "99g)")
                   "One. Two|.")))

(ert-deftest sentence-nav-backward-end ()
  (should (string= (sentence-nav-with "One fish. Two fish|." "g(")
                   "One fish|. Two fish."))
  ;; abbrev
  (should (string= (sentence-nav-with "Hello, Sam. This is Dr. Name|." "g(")
                   "Hello, Sam|. This is Dr. Name."))
  ;; first sentence end; shouldn't move or error
  (should (string= (sentence-nav-with "End|." "g(")
                   "End|."))
  ;; two-spaced
  (should (string= (sentence-nav-with "Red fish.  |Blue fish." "g(")
                   "Red fish|.  Blue fish."))
  (should (string= (sentence-nav-with "Red fish. | Blue fish." "g(")
                   "Red fish|.  Blue fish."))
  ;; just after sentence end
  (should (string= (sentence-nav-with "Black fish.|" "g(")
                   "Black fish|."))
  ;; in between sentences
  (should (string= (sentence-nav-with "Alfa. Bravo.| Charlie." "g(")
                   "Alfa. Bravo|. Charlie."))
  ;; quotes/special syntax
  (should (string= (sentence-nav-with "\"Sentence 1.\" |Sentence 2." "g(")
                   "\"Sentence 1.|\" Sentence 2."))
  (should (string= (sentence-nav-with "/“Sentence 1.”/ |Sentence 2." "g(")
                   "/“Sentence 1.”|/ Sentence 2."))
  (let (sentence-nav-jump-to-syntax)
    (should (string= (sentence-nav-with "\"Sentence 1.\" |Sentence 2." "g(")
                     "\"Sentence 1|.\" Sentence 2."))
    (should (string= (sentence-nav-with "/“Sentence 1.”/ |Sentence 2." "g(")
                     "/“Sentence 1|.”/ Sentence 2.")))
  ;; counts
  (should (string= (sentence-nav-with "First. Second. Third. Fourth.|" "2g(")
                   "First. Second. Third|. Fourth."))
  (should (string= (sentence-nav-with "One. Two.|" "99g(")
                   "One|. Two.")))

;; TODO on space in between sentence should select previous sentence
(ert-deftest sentence-nav-inner-text-object ()
  (should (string= (sentence-nav-with "|Inner sentence." "vis")
                   "~Inner sentence|."))
  (should (string= (sentence-nav-with "Inner |sentence." "vis")
                   "~Inner sentence|."))
  (should (string= (sentence-nav-with
                       "Sentence 1. |Sandwiched inner. Sentence 2."
                     "vis")
                   "Sentence 1. ~Sandwiched inner|. Sentence 2."))
  ;; normal with "syntax"
  (should (string= (sentence-nav-with "/“|Inner sentence.”/" "vis")
                   "~/“Inner sentence.”|/"))
  ;; "syntax" based
  (let ((sentence-nav-syntax-text-objects t))
    (should (string= (sentence-nav-with "/“|Inner sentence.”/" "vis")
                     "/“~Inner sentence|.”/"))))

(ert-deftest sentence-nav-a-text-object ()
  (should (string= (sentence-nav-with "|Outer sentence.  " "vas")
                   "~Outer sentence. | "))
  (should (string= (sentence-nav-with "Outer |sentence.  " "vas")
                   "~Outer sentence. | "))
  (should (string= (sentence-nav-with
                       "Sentence 1. |Sandwiched outer. Sentence 2."
                     "vas")
                   "Sentence 1. ~Sandwiched outer.| Sentence 2."))

  ;; normal with "syntax"
  (should (string= (sentence-nav-with "/“|Outer sentence.”/ Sentence 2." "vas")
                   "~/“Outer sentence.”/| Sentence 2."))
  (let ((sentence-nav-alternate-text-objects t))
    (should (string= (sentence-nav-with "/“|Outer sentence.”/ Sentence 2."
                       "vas")
                     "~/“Outer sentence.”/| Sentence 2."))))

;;; sentence-navigation-tests.el ends here
