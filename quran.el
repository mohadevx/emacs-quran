;;; quran.el --- Browse and search Quran in Emacs -*- lexical-binding: t; -*-

;; Author: Mohamed Abutaleb
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: quran, islam, religion
;; URL: https://github.com/mohadevx/emacs-quran

;;; Commentary:
;; Simple Emacs package to view random ayat and search the Quran text.
;; It depends on a plain text file: quran.txt (format: surah|ayah|text)

;;; Code:

(defvar quran-data-dir
  (expand-file-name "quran-data"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory where Quran data is stored.")

(defvar quran--verses nil
  "List of all Quran verses as alists: (sura . N) (aya . M) (text . STR).")

(defun quran--load-all-verses ()
  "Load Quran verses from quran.txt if not already loaded."
  (unless quran--verses
    (let ((file (expand-file-name "quran.txt" quran-data-dir)))
      (unless (file-exists-p file)
        (error "Quran data file not found: %s" file))
      (setq quran--verses
            (with-temp-buffer
              (insert-file-contents file)
              (let ((lines (split-string (buffer-string) "\n" t))
                    (verses '()))
                (dolist (line lines)
                  (let ((parts (split-string line "|" t)))
                    (when (= (length parts) 3)
                      (let ((sura (string-to-number (nth 0 parts)))
                            (aya  (string-to-number (nth 1 parts)))
                            (text (nth 2 parts)))
                        (push `((sura . ,sura) (aya . ,aya) (text . ,text)) verses)))))
                (nreverse verses)))))))

(defvar quran--indexed-ayat nil
  "Pre-indexed list of all ayat as strings for fast completion search.")

(defun quran--index-verses ()
  "Create pre-indexed ayah strings from `quran--verses`."
  (quran--load-all-verses)
  (setq quran--indexed-ayat
        (mapcar (lambda (verse)
                  (let ((sura (alist-get 'sura verse))
                        (aya  (alist-get 'aya verse))
                        (text (alist-get 'text verse)))
                    (cons (format "📖 Surah %d Ayah %d: %s" sura aya text)
                          verse)))
                quran--verses)))
                


;;;###autoload
(defun quran-random-ayah ()
  "Display a random ayah from the Quran."
  (interactive)
  (quran--load-all-verses)
  (let* ((count (length quran--verses))
         (random-verse (nth (random count) quran--verses))
         (sura (alist-get 'sura random-verse))
         (aya  (alist-get 'aya  random-verse))
         (text (alist-get 'text random-verse)))
    (with-current-buffer (get-buffer-create "*Quran Random Ayah*")
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "📖 Surah %d, Ayah %d\n\n%s" sura aya text))
      (goto-char (point-min))
      (read-only-mode 1)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun quran-search ()
  "Fuzzy search ayat using completing-read and display selected one."
  (interactive)
  (quran--index-verses)
  (let* ((selection (completing-read "Search Quran: " quran--indexed-ayat))
         (verse (cdr (assoc selection quran--indexed-ayat)))
         (sura  (alist-get 'sura verse))
         (aya   (alist-get 'aya verse))
         (text  (alist-get 'text verse)))
    (with-current-buffer (get-buffer-create "*Quran Search Result*")
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "📖 Surah %d, Ayah %d\n\n%s" sura aya text))
      (goto-char (point-min))
      (read-only-mode 1)
      (display-buffer (current-buffer)))))


(provide 'quran)
;;; quran.el ends here
