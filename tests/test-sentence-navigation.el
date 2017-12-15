;;; test-sentence-navigation.el --- Tests for sentence-navgiation.el. -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests for sentence-navigation.el.

;;; Code:
;; * Setup
(require 'buttercup)
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

;; * Sentence Motions
(describe "sentence-nav-forward"
  (it "should go to the next sentence beginning"
    (expect (sentence-nav-with "|This is a sentence. This is another."
              ")")
            :to-equal "This is a sentence. |This is another."))
  (it "should work with two-spaced sentences"
    (expect (sentence-nav-with "|This is a sentence.  This is another."
              ")")
            :to-equal "This is a sentence.  |This is another."))
  (it "should not skip the next sentence when at the end of the previous one"
    (expect (sentence-nav-with "This is a sentence.| This is too."
              ")")
            :to-equal "This is a sentence. |This is too."))
  ;; in between two-spaced
  (it "should not skip the next sentence when in between whitespace following a
 two-spaced sentence"
    (expect (sentence-nav-with "This is a sentence. | This is too."
              ")")
            :to-equal "This is a sentence.  |This is too."))
  (it "should work with newlines"
    (expect (sentence-nav-with "|\nHello world." ")")
            :to-equal "\n|Hello world.")
    (expect (sentence-nav-with "|* Org Heading\nHello bird." ")")
            :to-equal "* Org Heading\n|Hello bird."))
  (it "should skip abbrevations"
    (expect (sentence-nav-with "|Mr. Bob likes pie. I like pie."
              ")")
            :to-equal "Mr. Bob likes pie. |I like pie."))
  (it "should go to (or optionally skip) quotes/preceding syntax"
    (expect (sentence-nav-with "|Sentence one. \"Sentence two.\"" ")")
            :to-equal "Sentence one. |\"Sentence two.\"")
    (expect (sentence-nav-with "|Sentence one. /“Sentence two.”/" ")")
            :to-equal "Sentence one. |/“Sentence two.”/")
    (let (sentence-nav-jump-to-syntax)
      (expect (sentence-nav-with "|Sentence one. \"Sentence two.\"" ")")
              :to-equal "Sentence one. \"|Sentence two.\"")
      (expect (sentence-nav-with "|Sentence one. /“Sentence two.”/" ")")
              :to-equal "Sentence one. /“|Sentence two.”/")))
  (it "should work with sentences in comments"
    (expect (sentence-nav-with "|;; A commented sentence."
              (emacs-lisp-mode)
              ")")
            :to-equal ";; |A commented sentence."))
  (it "should not move the point when no more sentences can be found"
    (expect (sentence-nav-with "|Mr. Smith says this is one sentence."
              ")")
            :to-equal "|Mr. Smith says this is one sentence."))
  (it "should optionally take a count"
    (expect (sentence-nav-with "|First. Second. Third. Fourth." "2)")
            :to-equal "First. Second. |Third. Fourth.")
    (expect (sentence-nav-with "|One. Two." "99)")
            :to-equal "One. |Two."))
  (it "should optionally support hard-wrapped sentences"
    (let ((sentence-nav-hard-wrapping t))
      ;; sentences
      (expect (sentence-nav-with "|Sentence 1.\n\nSentence 2." ")")
              :to-equal "Sentence 1.\n\n|Sentence 2.")
      (expect (sentence-nav-with "|* Org Heading\nA sentence." ")")
              :to-equal "* Org Heading\n|A sentence.")
      (expect (sentence-nav-with "|Full sentence 1.\nFull sentence 2" ")")
              :to-equal "Full sentence 1.\n|Full sentence 2")
      ;; non-sentences
      (expect (sentence-nav-with "|Split\nSentence." ")")
              :to-equal "|Split\nSentence.")
      (expect (sentence-nav-with "|Mr.\nSmith's sentence got split." ")")
              :to-equal "|Mr.\nSmith's sentence got split."))))

(describe "sentence-nav-backward"
  (it "should go to the previous sentence beginning"
    (expect (sentence-nav-with "Sentence 1.|" "(")
            :to-equal "|Sentence 1.")
    (expect (sentence-nav-with "Sentence 1. |Sentence 2." "(")
            :to-equal "|Sentence 1. Sentence 2."))
  (it "should work with two-spaced sentences"
    (expect (sentence-nav-with "Sentence 1.  |Sentence 2." "(")
            :to-equal "|Sentence 1.  Sentence 2."))
  (it "should not skip the current sentence when just after its beginning"
    (expect (sentence-nav-with "Sentence 1. S|entence 2." "(")
            :to-equal "Sentence 1. |Sentence 2."))
  (it "should work with newlines"
    (expect (sentence-nav-with "* Org Heading\nHello bird.|" "(")
            :to-equal "* Org Heading\n|Hello bird."))
  (it "should skip abbreviations"
    (expect (sentence-nav-with "I am not Dr. Brown|." "(")
            :to-equal "|I am not Dr. Brown."))
  (it "should go to (or optionally skip) quotes/preceding syntax"
    (expect (sentence-nav-with "\"A sentence.\"|" "(")
            :to-equal "|\"A sentence.\"")
    (expect (sentence-nav-with "/“A sentence.”/|" "(")
            :to-equal "|/“A sentence.”/")
    (let (sentence-nav-jump-to-syntax)
      (expect (sentence-nav-with "\"A sentence.\"|" "(")
              :to-equal "\"|A sentence.\"")
      (expect (sentence-nav-with "/“A sentence.”/|" "(")
              :to-equal "/“|A sentence.”/")))
  (it "should work with sentences in comments"
    (expect (sentence-nav-with ";; A commented sentence.|"
              (emacs-lisp-mode)
              "(")
            :to-equal ";; |A commented sentence."))
  (it "should not move the point when no more sentences can be found"
    (expect (sentence-nav-with "incomplete sentence. |Full sentence."
              "(")
            :to-equal "incomplete sentence. |Full sentence."))
  (it "should optionally take a count"
    (expect (sentence-nav-with "First. Second. Third. Fourth|." "2(")
            :to-equal "First. Second. |Third. Fourth.")
    (expect (sentence-nav-with "One. |Two." "99(")
            :to-equal "|One. Two."))
  ;; TODO test hard-wrapping
  )

(describe "sentence-nav-forward-end"
  (it "should go to the next sentence end"
    (expect (sentence-nav-with "|One fish. Two fish." "g)")
            :to-equal "One fish|. Two fish.")
    (expect (sentence-nav-with "Red fish|. Blue fish." "g)")
            :to-equal "Red fish. Blue fish|."))
  (it "should work with two-spaced sentences"
    (expect (sentence-nav-with "|Black fish.  Blue fish." "g)")
            :to-equal "Black fish|.  Blue fish."))
  (it "should not skip the current sentence end when just before it"
    (expect (sentence-nav-with "Old fis|h. New fish." "g)")
            :to-equal "Old fish|. New fish."))
  ;; TODO newlines
  (it "should skip abbreviations"
    (expect (sentence-nav-with
                "|Gold Ave. is a place. I have not been there."
              "g)")
            :to-equal "Gold Ave. is a place|. I have not been there.")
    (expect (sentence-nav-with
                "Gold Ave|. is a place. I have not been there."
              "g)")
            :to-equal "Gold Ave. is a place|. I have not been there."))
  (it "should go to after (or optionally to before) quotes/trailing syntax"
    (expect (sentence-nav-with "|\"A sentence.\"" "g)")
            :to-equal "\"A sentence.|\"")
    (expect (sentence-nav-with "|/“A sentence.”/" "g)")
            :to-equal "/“A sentence.”|/")
    (let (sentence-nav-jump-to-syntax)
      (expect (sentence-nav-with "|\"A sentence.\"" "g)")
              :to-equal "\"A sentence|.\"")
      (expect (sentence-nav-with "|/“A sentence.”/" "g)")
              :to-equal "/“A sentence|.”/")))
  (it "should not move the point when no more sentence ends can be found"
    (expect (sentence-nav-with "End|. junk text" "g)")
            :to-equal "End|. junk text")
    (expect (sentence-nav-with "End|." "g)")
            :to-equal "End|."))
  (it "should optionally take a count"
    (expect (sentence-nav-with "|First. Second. Third. Fourth." "2g)")
            :to-equal "First. Second|. Third. Fourth.")
    (expect (sentence-nav-with "|One. Two." "99g)")
            :to-equal "One. Two|.")))

(describe "sentence-nav-backward-end"
  (it "should go to the previous sentence end"
    (expect (sentence-nav-with "One fish. Two fish|." "g(")
            :to-equal "One fish|. Two fish."))
  (it "should work with two-spaced sentences"
    (expect (sentence-nav-with "Red fish.  |Blue fish." "g(")
            :to-equal "Red fish|.  Blue fish.")
    (expect (sentence-nav-with "Red fish. | Blue fish." "g(")
            :to-equal "Red fish|.  Blue fish."))
  (it "should not skip a sentence when just after its end"
    (expect (sentence-nav-with "Black fish.|" "g(")
            :to-equal "Black fish|.")
    (expect (sentence-nav-with "Alpha. Bravo.| Charlie." "g(")
            :to-equal "Alpha. Bravo|. Charlie."))
  ;; TODO newlines
  (it "should skip abbreviations"
    (expect (sentence-nav-with "Hello, Sam. This is Dr. Name|." "g(")
            :to-equal "Hello, Sam|. This is Dr. Name."))
  (it "should go to after (or optionally to before) quotes/trailing syntax"
    (expect (sentence-nav-with "\"Sentence 1.\" |Sentence 2." "g(")
            :to-equal "\"Sentence 1.|\" Sentence 2.")
    (expect (sentence-nav-with "/“Sentence 1.”/ |Sentence 2." "g(")
            :to-equal "/“Sentence 1.”|/ Sentence 2.")
    (let (sentence-nav-jump-to-syntax)
      (expect (sentence-nav-with "\"Sentence 1.\" |Sentence 2." "g(")
              :to-equal "\"Sentence 1|.\" Sentence 2.")
      (expect (sentence-nav-with "/“Sentence 1.”/ |Sentence 2." "g(")
              :to-equal "/“Sentence 1|.”/ Sentence 2.")))
  (it "should not move the point when no more sentence ends can be found"
    (expect (sentence-nav-with "End|." "g(")
            :to-equal "End|."))
  (it "should optionally take a count"
    (expect (sentence-nav-with "First. Second. Third. Fourth.|" "2g(")
            :to-equal "First. Second. Third|. Fourth.")
    (expect (sentence-nav-with "One. Two.|" "99g(")
            :to-equal "One|. Two.")))

;; * Text Objects
(describe "sentence-nav-inner-text-object"
  (it "should work in the basic case"
    (expect (sentence-nav-with "|Inner sentence." "vis")
            :to-equal "~Inner sentence|.")
    (expect (sentence-nav-with "Inner |sentence." "vis")
            :to-equal "~Inner sentence|.")
    (expect (sentence-nav-with
                "Sentence 1. |Sandwiched inner. Sentence 2."
              "vis")
            :to-equal "Sentence 1. ~Sandwiched inner|. Sentence 2."))
  (it "should by default include quotes/surrounding syntax"
    (expect (sentence-nav-with "/“|Inner sentence.”/" "vis")
            :to-equal "~/“Inner sentence.”|/"))
  (it "should optionally exclude quotes/surrounding syntax instead"
    (let ((sentence-nav-syntax-text-objects t))
      (expect (sentence-nav-with "/“|Inner sentence.”/" "vis")
              :to-equal "/“~Inner sentence|.”/"))))

(describe "sentence-nav-a-text-object"
  (it "should work in the basic case"
    (expect (sentence-nav-with "|Outer sentence.  " "vas")
            :to-equal "~Outer sentence. | ")
    (expect (sentence-nav-with "Outer |sentence.  " "vas")
            :to-equal "~Outer sentence. | ")
    (expect (sentence-nav-with
                "Sentence 1. |Sandwiched outer. Sentence 2."
              "vas")
            :to-equal "Sentence 1. ~Sandwiched outer.| Sentence 2."))
  (it "should always include quotes/surrounding syntax and trailing spaces"
    (expect (sentence-nav-with "/“|Outer sentence.”/ Sentence 2." "vas")
            :to-equal "~/“Outer sentence.”/| Sentence 2.")
    ;; value doesn't matter for outer
    (let ((sentence-nav-syntax-text-objects t))
      (expect (sentence-nav-with "/“|Outer sentence.”/ Sentence 2."
                "vas")
              :to-equal "~/“Outer sentence.”/| Sentence 2."))))

(provide 'test-sentence-navigation)
;;; test-sentence-navigation.el ends here
