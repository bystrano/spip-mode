;;; spip-mode.el --- Un mode mineur pour SPIP

;;; Commentary:

;;; Un mode mineur pour le CMS SPIP (http://spip.net).

;;; Configure web-mode, php-mode et javascript mode pour le CMS
;;; SPIP. PHP-mode utilise les règles de codage SPIPiennes,
;;; web-mode colore les boucles, filtres et balise SPIP.

;;; Fournit une commande Helm pour la gestion des surcharges de
;;; fichiers du core. La commande spip-overload permet de
;;; surcharger le fichier courant. On propose une liste de
;;; répertoires candidats pour la surcharge, et la commande se
;;; charge ensuite de créer un nouveau fichier au bon endroit,
;;; d'y copier le contenu du fichier actuel et de l'ouvrir dans
;;; un nouveau buffer.

;;; TODO:

;;; Fournit des commandes pour la gestion des chaînes de langue

;;; Code:

(require 'json)
(require 's)
(require 'dash)
(require 'ert)

(defvar spip-root nil
  "The root directory of the current SPIP project.")

(define-error 'spip-mode-error "SPIP mode error")
(define-error 'not-in-spip "Not in a SPIP installation" 'spip-mode-error)
(define-error 'no-spip-executable "Couldn't find the spip executable" 'spip-mode-error)
(define-error 'no-spip-eval-command "SPIP CLI does not support the php:eval command")

(define-minor-mode spip-mode
  "A minor mode for the SPIP CMS."
  nil " SPIP " nil

  (make-local-variable 'spip-root)

  (setq spip-root (spip-get-root))

  ;; If not in a SPIP project, get out.
  (if (not (stringp spip-root))
      (setq spip-mode nil)))

(defun spip-mode-web-mode-config ()
  "Configures web-mode for SPIP."
  (spip-mode)
  (if (spip-mode)
      (web-mode-set-engine "spip")))

(defun spip-mode-php-mode-config ()
  "Configures php-mode for SPIP."
  (spip-mode))

(defun spip-mode-nxml-mode-config ()
  "Configures nxml-mode for SPIP."
  (spip-mode))

;;;;;;;;;;;
;; Lang

(defun spip-font-block-at-point ()
  "Returns the biggest string around point that has the same font-face."

  (let ((start (point))
        (end (point))
        (face (get-text-property (point) 'face)))
    (save-excursion
      (let ((break nil))
        (while (and (not break) (> (point) (line-beginning-position)))
          (goto-char (- (point) 1))
          (if (equal face (get-text-property (point) 'face))
              (setq start (point))
            (setq break t)))))
    (save-excursion
      (let ((break nil))
        (while (and (not break) (< (point) (line-end-position)))
          (if (equal face (get-text-property (point) 'face))
              (setq end (+ 1 (point)))
            (setq break t))
          (goto-char (+ (point) 1)))))
    (cons start end)))

(defun spip-lang-string-at-point ()
  "Return the lang string at point, or nil if the point isn't on one.

If the lang string at point is abbreviated, like 'annuler', we
return an explicit version, like 'spip:annuler'."

  (let* ((reg (spip-font-block-at-point))
         (beg (car reg))
         (end (cdr reg))
         (lang-string nil))

    (cond
     ((and (equal mode-name "Web")
           (equal (get-text-property (point) 'face)'
                  web-mode-block-string-face))
      (let ((match (s-match "<:\\(.+\\):>"
                            (buffer-substring-no-properties beg end))))
        (setq lang-string (elt match 1))))
     ((and (equal mode-name "PHP/l")
           (equal (get-text-property (point) 'face)
                  font-lock-string-face))
      (let* ((match (s-match "['\"]\\(.+\\)['\"]"
                             (buffer-substring-no-properties beg end))))
        (setq lang-string (elt match 1))))
     ((and (equal mode-name "nXML")
           (equal (get-text-property (point) 'face)
                  '(nxml-attribute-value)))
      (setq lang-string (buffer-substring-no-properties beg end))))

    (when (and (stringp lang-string)
               (s-matches-p "^[[:ascii:]]+$" lang-string))
      (if (s-contains-p ":" lang-string)
          lang-string
        (spip-expand-lang-string lang-string)))))

(defun spip-expand-lang-string (lang-string)
  "Expand an abbreviated lang string.

'annuler' -> 'spip:annuler'"

  (when (> (length (spip-translate-lang-string lang-string))
           0)
    (let ((modules '("local" "spip" "ecrire"))
          (result nil))
      (while (> (length modules) 0)
        (let ((module (pop modules)))
          (when (s-matches-p
                 (format "['\"]%s['\"]" lang-string)
                 (-if-let* ((file (spip-find-in-path
                                  (format "lang/%s_fr.php" module)))
                            (fullpath (concat spip-root file)))
                     (with-temp-buffer
                       (insert-file-contents fullpath)
                       (buffer-string))
                   ""))
            (setq modules nil ;; exit the loop
                  result (format "%s:%s" module lang-string)))))
      result)))

(defun spip-group-lang-modules (module-files)

  (let ((modules '()))
    (while (not (equal 0 (length module-files)))
      (let ((file (pop module-files)))
        (if (not (s-matches-p "\.php$" file))
            (error "not a php file")
          (let ((words (reverse (s-split "_" (s-replace ".php" "" file))))
                (module nil)
                (lang nil)
                (lang_end nil))
            (while (not (equal 0 (length words)))
              (let ((word (pop words)))
                (if (or lang_end (> (length word) 3))
                    (progn
                      (setq lang_end t)
                      (add-to-list 'module word))
                  (add-to-list 'lang word))))

            (setq module (s-join "_" module))
            (setq lang (s-join "_" lang))

            (let ((existing-module (assoc module modules)))
              (if (not existing-module)
                  (add-to-list 'modules (cons module (list lang)))
                (setcdr (assoc module modules)
                        (-uniq
                         (append
                          (cdr existing-module) (list lang))))))))))
    modules))

(ert-deftest test-spip-group-lang-modules ()
  "lang module classification."
  (should (equal (spip-group-lang-modules
                  '("spip_fr.php" "spip_nl.php"))
                 '(("spip" . ("fr" "nl")))))
  (should (equal (spip-group-lang-modules
                  '("spip_fr.php" "spip_nl.php" "local_nl.php"))
                 '(("local" . ("nl")) ("spip" . ("fr" "nl")))))
  (should (equal (spip-group-lang-modules
                  '("spip_bonux_pt_br.php"))
                 '(("spip_bonux" . ("pt_br")))))
  (should (equal (spip-group-lang-modules
                  '("pb_selection_oc_mis_bla.php"))
                 '(("pb_selection" . ("oc_mis_bla")))))
  (should (equal (spip-group-lang-modules
                  '("spip_fr.php" "spip_fr.php"))
                 '(("spip" . ("fr"))))))

(defun spip-get-lang-module-files ()

  (-flatten
   (mapcar (lambda (dir)
             (directory-files dir nil ".*\.php$"))
           (-filter 'file-exists-p
                    (mapcar (lambda (dir)
                              (concat spip-root dir "lang/"))
                            (spip-get-path))))))

(defconst spip-lang-key-regexp
  "^[[:space:]]*['\"]\\(%s\\)['\"][[:space:]]*=>")

(defun spip-make-new-lang-string (reg-beg reg-end)
  "Make a new lang string with the current region."
  (interactive "r")

  (-if-let* ((text (buffer-substring-no-properties reg-beg reg-end))
             (lang-string (read-string
                           "insert lang string name (module:lang-string) : "))
             (is-valid (s-matches-p "^[-_a-z0-9]+:[-_a-z0-9]+$"
                                    lang-string)))

      (spip-jump-to-lang-string-definition lang-string
                                           (lambda ()
                                             (insert text)))
    (user-error "Not a valid lang string")))

(defun spip-jump-to-lang-string-definition (&optional lang-string callback)
  "Jump to the definition of the lang string at point."
  (interactive)

  (condition-case err
      (-if-let* ((full-string (or lang-string
                                  (spip-lang-string-at-point)))
                 (module (car (s-split ":" full-string)))
                 (lang-key (cadr (s-split ":" full-string)))
                 (lang (spip-select-lang))
                 (module-file
                  (or (spip-find-in-path
                       (format "lang/%s_%s.php" module lang))
                      (when (y-or-n-p
                             (format
                              "Module '%s_%s' doesn't exist. Create a new module file ?"
                              module lang))
                        (spip-insert-lang-module module lang lang-key)))))

          (with-current-buffer (find-file (concat spip-root module-file))
            (let ((selection-beg nil)
                  (selection-end nil))
              (goto-char (point-min))
              (if (not (s-matches-p
                          (format spip-lang-key-regexp lang-key)
                          (buffer-string)))
                  (if (y-or-n-p "Lang string doesn't exist. Create a new one ?")
                      (progn
                        (spip-insert-lang-string lang-key module-file)
                        (spip-select-lang-string lang-key module-file)
                        (funcall callback))
                    (kill-buffer))
                (progn
                  (spip-select-lang-string lang-key module-file)
                  (funcall callback)))))
        (user-error "Not a lang string"))
    ('spip-mode-error
     (spip-handle-error err))))

(defcustom spip-lang-module-template
  "<?php
// This is a SPIP language file  --  Ceci est un fichier langue de SPIP

$GLOBALS[$GLOBALS['idx_lang']] = array(
	'%s' => '',
);
"
  "Le template pour les fichiers de langues créés par spip-mode.")

(defun spip-insert-lang-module (module lang key)
  "Create a new lang module file in a user-chosen directory."

  (-when-let* ((module-file-name (format "lang/%s_%s.php" module lang))
               (dir (spip-select-overload-dir module-file-name))
               (file-path (concat spip-root dir module-file-name)))
    (find-file file-path)
    (insert (format spip-lang-module-template key))
    (make-directory (spip-get-directory file-path))
    (save-buffer)
    (kill-buffer)))

(defun spip-select-lang-string (key module-file)

  (goto-char (point-min))
  (re-search-forward (format spip-lang-key-regexp key))
  (let ((selection-beg (re-search-forward "['\"]"))
        (selection-end nil))
    (goto-char (- (point) 1))
    (setq selection-end (- (re-search-forward
                            (format "[^\\]%s"
                                    (buffer-substring (point)
                                                      (+ (point) 1))))
                           1))
    (goto-char selection-beg)
    (push-mark selection-end)
    (setq mark-active (not (equal selection-beg selection-end)))))

(defun spip-insert-lang-string (key module-file)

  (-if-let* ((fullpath (concat spip-root module-file))
             (current-keys (mapcar (lambda (match) (elt match 1))
                                   (s-match-strings-all
                                    (format spip-lang-key-regexp "[^'\"]+")
                                    (with-temp-buffer
                                      (insert-file-contents fullpath)
                                      (buffer-string))))))

      (let ((predecessor (car (reverse
                               (-filter (lambda (current-key)
                                          (s-less? current-key key))
                                        current-keys)))))
        (if predecessor
            (progn
              (re-search-forward (format spip-lang-key-regexp predecessor))
              (goto-char (line-end-position))
              (push-mark (line-end-position))
              (setq mark-active t)
              (insert (format "\n\t'%s' => ''," key)))
          (progn
            (re-search-forward (format spip-lang-key-regexp "[^'\"]+"))
            (goto-char (line-beginning-position))
            (insert (format "\t'%s' => '',\n" key)))))
    (error "Could't find a place to insert lang string")))

;;;;;;;;;;
;; Helm

(defvar spip-helm-env-file nil
  "The file that was open when spip-overload was called. internal
  use only")

(defvar spip-helm-env-root nil
  "The value of spip-root when spip-overload was called. internal
  use only")

(defvar spip-helm-env-lang nil
  "The currently selected language.")

(defvar spip-helm-env-overload-dir nil)

(defvar helm-source-spip-overload
  '((name . "SPIP overload")
    (candidates . spip-helm-overload-candidates)
    (init . spip-helm-env-init)
    (action . (("Overload" . spip-helm-overload-file))))
  "Configuration of the spip-overload Helm command.")

(defun spip-helm-env-init ()
  (setq spip-helm-env-root spip-root
        spip-helm-env-file buffer-file-name))

(defun spip-helm-overload-candidates ()

  (let ((spip-root spip-helm-env-root))
    (mapcar (lambda (dir)
              (cons (format "%s" dir) dir))
            (spip-get-overload-targets spip-helm-env-file))))

(defun spip-helm-overload-file (dir)

  (let* ((spip-root spip-helm-env-root)
         (file (plist-get
                (spip-split-on-path spip-helm-env-file)
                :file))
         (filepath (concat spip-root dir file)))

    (find-file filepath)
    (if (not (file-exists-p filepath))
        (insert-file-contents spip-helm-env-file))))

(defun spip-overload ()
  "Overload the current file.

Ask for a destination directory, then create a new file at the
right place and copy the original content inside it. If the
target file already exists, we simply open it."
  (interactive)

  (condition-case err
      (progn
        (spip-env-check)
        (helm :sources '(helm-source-spip-overload)))
    ('spip-mode-error
     (spip-handle-error err))))

(defvar helm-source-spip-active-lang
  '((name . "Langues actives")
    (init . spip-helm-env-init)
    (candidates . spip-get-active-languages)
    (action . (("Select" . spip-helm-select-lang))))
  "Configuration of the spip-select-lang-command.")

(defvar helm-source-spip-available-lang
  '((name . "Langues proposées")
    (init . spip-helm-env-init)
    (candidates . spip-get-available-languages)
    (action . (("Select" . spip-helm-select-lang))))
  "Configuration of the spip-select-lang-command.")

(defun spip-format-lang (lang)
  (cdr (assoc-string lang (spip-get-language-list))))

(defun spip-get-active-languages ()

  (let ((spip-root spip-helm-env-root))
    (mapcar (lambda (lang)
              (cons (spip-format-lang lang) lang))
            (if (> (length (spip-lire-config "langues_multilingue")) 0)
                (s-split "," (spip-lire-config "langues_multilingue"))
              (list (spip-lire-config "langue_site"))))))

(defun spip-get-available-languages ()

  (let* ((spip-root spip-helm-env-root)
         (lang-list (spip-get-language-list)))
    (mapcar (lambda (lang)
              (cons (cdr (assoc-string lang lang-list)) lang))
            (s-split "," (spip-lire-config "langues_proposees")))))

(defun spip-helm-select-lang (lang)

  (setq spip-helm-env-lang lang))

(defun spip-select-lang ()
  "Return a language code chosen by the user."

  (setq spip-helm-env-lang nil)
  (helm :sources '(helm-source-spip-active-lang
                   helm-source-spip-available-lang))
  spip-helm-env-lang)

(defvar helm-source-spip-select-overload-dir
  '((name . "Select directory")
    (candidates . spip-helm-overload-candidates)
    (action . (("Select" . spip-helm-select-overload-dir-action)))))

(defun spip-helm-select-overload-dir-action (dir)
  (setq spip-helm-env-overload-dir dir))

(defun spip-select-overload-dir (file)
  (setq spip-helm-env-root spip-root
        spip-helm-env-file file
        spip-helm-env-overload-dir nil)
  (helm :sources '(helm-source-spip-select-overload-dir))
  spip-helm-env-overload-dir)

;;;;;;;;;;;
;; Utils

(defun spip-get-overload-targets (filepath)
  "Return a list of directories that can be used to overload the
  current file."

  (let ((path (mapcar 'identity (spip-get-path)))
        (path-components (spip-split-on-path filepath))
        (result (list))
        (dir nil))
    (while path
      (setq dir (pop path))
      (if (equal dir (plist-get path-components :path))
          (setq path nil) ;; end the loop
        (setq result (push dir result))))
    result))

(defun spip-split-on-path (filepath)
  "Extract the path and the file component of FILENAME.

The path component is the path from the spip-root directory to
the directory in the path to which FILEPATH belongs, and the file
component is the path from this directory to FILENAME."

  (let ((path (mapcar 'identity (spip-get-path)))
        (result nil)
        (dir nil))
    (while path
      (setq dir (concat spip-root (pop path)))
      (if (s-starts-with-p dir filepath)
          (progn
            (setq path nil ;; end the loop
                  result (list :path (s-chop-prefix spip-root dir)
                               :file (s-chop-prefix dir filepath))))))
    result))

(defun spip-get-root ()
  "Return the root of the current SPIP project.
Returns nil if not in a SPIP project."

  (let* ((path (directory-file-name
                (expand-file-name default-directory)))
         (parents (list path)))

    (while (not (string= path ""))
      (setq path
            (mapconcat 'identity
                       (reverse (cdr (reverse (split-string path "/"))))
                       "/"))
      (add-to-list 'parents path))

    (spip-get-directory (locate-file "spip.php" parents))))

(defun spip-get-object (php-code)

  (if spip-root
      (let ((get-json (format "echo json_encode(%s);" php-code))
            (json-false nil)
            (json-null nil))
        (json-read-from-string (spip-eval-php get-json)))
    (signal 'not-in-spip nil)))

(defun spip-get-path ()
  "Return the vector of the directories that make the SPIP path."

  (spip-get-object "creer_chemin()"))

(defun spip-find-in-path (file)
  "Finds a file in SPIP's path."

  (spip-get-object (format "find_in_path('%s')" file)))

(defvar spip-language-list nil)

(defun spip-get-language-list ()

  (if (not (equal spip-language-list nil))
      spip-language-list
    (if spip-root
        (let ((get-json (concat
                         "include_spip('inc/lang_liste');"
                         "echo json_encode("
                         "array_map('html_entity_decode', $GLOBALS['codes_langues'])"
                         ");"
                         ))
              (json-false nil)
              (json-null nil))
          (setq spip-language-list
                (json-read-from-string (spip-eval-php get-json)))
          spip-language-list)
      (signal 'not-in-spip nil))))

(defun spip-translate-lang-string (lang-string &optional lang)

  (let* ((option-string (if (stringp lang)
                           (format ", array('spip_lang' => '%s')" lang)
                         ""))
         (command (format "echo _T('%s'%s);" lang-string option-string)))
    (if spip-root
        (spip-eval-php command)
      (signal 'not-in-spip nil))))

(defun spip-lire-config (meta)

  (if spip-root
      (spip-eval-php (format
                      "include_spip('inc/config'); echo lire_config('%s');"
                      meta))
    (signal 'not-in-spip nil)))

(defun spip-php-eval-region (reg-beg reg-end)
  "Evaluate php code in the active region in a SPIP context."
  (interactive "r")

  (let ((code (buffer-substring-no-properties reg-beg reg-end)))
    (condition-case err
        (pp (spip-eval-php code))
      ('spip-mode-error
       (spip-handle-error err)))))

(defun spip-eval-php (php-code)
  "Evaluates the given php code in the current SPIP instance
  using SPIP-CLI's php:eval command."

  (if (not (string= (shell-command-to-string "which spip") ""))
      (if (string= (shell-command-to-string "spip php:eval \"echo 'test';\"") "test")
          (shell-command-to-string
           (format "spip php:eval '%s'" (s-replace "'" "\"" php-code)))
        (signal 'no-spip-eval-command nil))
    (signal 'no-spip-executable nil)))

(defun spip-get-directory (filename)
  "Returns the directory component of FILENAME."

  (if (stringp filename)
      (s-append "/"
                (directory-file-name
                 (mapconcat  'identity
                             (reverse (cdr (reverse (split-string filename "/"))))
                             "/")))))

(defun spip-env-check ()
  "Return t if we are in a SPIP install with the CLI installed."

  (if spip-root
      (string= "1" (spip-eval-php "echo _ECRIRE_INC_VERSION;"))
    (signal 'not-in-spip nil)))

(defun spip-handle-error (err)
  (message "SPIP-mode: %s" (error-message-string err)))

(provide 'spip-mode)
;;; spip-mode.el ends here
