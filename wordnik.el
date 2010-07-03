;;; wordnik.el --- Client library for the Wordnik API

;; Copyright (C) 2010  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs client library for Wordnik's API.

;; This currently only handles the public API methods; auth and the
;; private methods will come at a later date.

;;; Code:

(require 'json)
(require 'url)

(defgroup wordnik nil
  "Emacs interface to Wordnik."
  :group 'processes
  :prefix "wordnik-"
  :link '(url-link :tag "Wordnik" "http://www.wordnik.com/")
  :link '(url-link :tag "Wordnik API documentation"
                   "http://docs.wordnik.com/api"))

(defcustom wordnik-api-key nil
  "Your Wordnik API key.
Apply for one at <URL:http://api.wordnik.com/signup/>."
  :group 'wordnik
  :type '(string))

(defun wordnik-get-spellings (attempt)
  (wordnik-request "word" attempt nil :useSuggest t))

(defun wordnik-fix-spelling (attempt)
  (wordnik-request "word" attempt nil :useSuggest t :literal nil))

(defun wordnik-get-bigram-phrases (word)
  (wordnik-request "word" word "phrases"))

(defun wordnik-get-definitions (word &optional count &rest parts-of-speech)
  (let ((args '()))
    (when parts-of-speech
      (setq args (append (list :partOfSpeech
                               (mapconcat 'identity parts-of-speech ","))
                         args)))
    (when count
      (setq args (append (list :count count) args)))
    (apply 'wordnik-request "word" word "definitions" args)))

(defun wordnik-get-examples (word)
  (wordnik-request "word" word "examples"))

(defun wordnik-get-related (word &optional count &rest relations)
  (let ((args '()))
    (when relations
      (setq args (append (list :type
                               (mapconcat 'identity relations ","))
                         args)))
    (when count
      (setq args (append (list :count count) args)))
    (apply 'wordnik-request "word" word "related" args)))

(defun wordnik-get-frequency (word)
  (wordnik-request "word" word "frequency"))

(defun wordnik-get-punctuation-factor (word)
  (wordnik-request "word" word "punctuationFactor"))

(defun wordnik-get-autocompletions (fragment &optional count start-at)
  (let ((args '()))
    (when count
      (setq args (append (list :count count) args)))
    (when start-at
      (setq args (append (list :startAt start-at) args)))
    (apply 'wordnik-request "suggest" fragment nil args)))

(defun wordnik-get-word-of-the-day ()
  (wordnik-request "wordoftheday"))

(defun wordnik-get-random-word (&optional has-dictionary-reference)
  (let ((args '()))
    (when has-dictionary-reference
      (setq args (list :hasDictionaryRef t)))
    (apply 'wordnik-request "words" "randomWord" nil args)))

(defun wordnik-get-pronunciations (word)
  (wordnik-request "words" word "pronunciations"))



(defun wordnik-request (type &optional id field &rest arguments)
  "Make a request of the Wordnik API."
  (let ((url-package-name "wordnik.el")
        (endpoint (format "http://api.wordnik.com/api/%s.json/%s%s?%s"
                          type
                          (if id id "")
                          (if field (concat "/" field) "")
                          (wordnik-format-arglist
                           (append (list :api_key wordnik-api-key)
                                   arguments)))))
    (wordnik-response
     (url-retrieve-synchronously
      endpoint))))

(defun wordnik-stringify (object)
  "Return a string representation of OBJECT."
  (cond ((keywordp object) (substring (symbol-name object) 1))
        ((stringp object) object)
        ((symbolp object) (symbol-name object))
        ((numberp object) (number-to-string object))
        (t (pp-to-string object))))

(defun wordnik-format-arglist (plist)
  "Format PLIST as HTTP query parameters."
  (let ((alist (list)))
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    (mapconcat
     (lambda (cons)
       (format "%s=%s"
               (url-hexify-string (wordnik-stringify (car cons)))
               (url-hexify-string (wordnik-stringify (cdr cons)))))
     alist
     "&")))

(defvar wordnik-debug nil)
(defvar wordnik-last-respone nil)

(defun wordnik-response (buffer)
  (unwind-protect
      (with-current-buffer buffer
        (url-http-parse-response)
        (goto-char url-http-end-of-headers)
        (let ((json-object-type 'plist)
              (json-array-type 'list)
              (json-false nil))
          (json-read)))
    (if wordnik-debug
        (setq wordnik-last-response buffer)
      (kill-buffer buffer))))

(provide 'wordnik)
;;; wordnik.el ends here
