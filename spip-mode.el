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

(defvar spip-root nil
  "The root directory of the current SPIP project.")

(define-minor-mode spip-mode
  "A minor mode for the SPIP CMS."
  nil " SPIP " nil
  (setq spip-root (get-spip-root))
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

(defun get-spip-root ()
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

(defun get-spip-path ()
  "Return the vector of the directories that make the SPIP path."

  (condition-case err
      (json-read-from-string
       (spip-eval-php "echo json_encode(creer_chemin());"))
    (end-of-file
     (error "Couldn't compute path"))
    (json-readtable-error
     (error "Couldn't compute path"))))

(defun spip-find-in-path (file)
  "Finds a file in SPIP's path."

  (condition-case err
      (json-read-from-string
       (spip-eval-php (format "echo json_encode(find_in_path('%s'));"
                              file)))
    (end-of-file
     (error "Could not find file"))
    (json-readtable-error
     (error "Could not find file"))))

(defun split-on-path (filepath)
  "Extract the path and the file component of FILENAME.

The path component is the path from the spip-root directory to
the directory in the path to which FILEPATH belongs, and the file
component is the path from this directory to FILENAME."

  (let ((path (mapcar 'identity (get-spip-path)))
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

(defun get-spip-overload-targets (filepath)
  "Return a list of directories that can be used to overload the
  current file."

  (let ((path (mapcar 'identity (get-spip-path)))
        (path-components (split-on-path filepath))
        (result (list))
        (dir nil))
    (while path
      (setq dir (pop path))
      (if (equal dir (plist-get path-components :path))
          (setq path nil) ;; end the loop
        (setq result (push dir result))))
    result))

;;;;;;;;;;
;; Helm

(defvar spip-helm-original-file nil
  "The file that was open when spip-overload was called. internal
  use only")

(defvar helm-source-spip-overload
  '((name . "SPIP overload")
    (candidates . spip-helm-overload-candidates)
    (init . spip-helm-overload-init)
    (action . (("Overload" . spip-helm-overload-file))))
  "Configuration of the spip-overload Helm command.")

(defun spip-helm-overload-init ()
  (setq spip-helm-original-file buffer-file-name))

(defun spip-helm-overload-candidates ()

  (mapcar (lambda (dir)
            (cons (format "%s" dir) dir))
          (get-spip-overload-targets spip-helm-original-file)))

(defun spip-helm-overload-file (dir)

  (let* ((file (plist-get
                (split-on-path spip-helm-original-file)
                :file))
         (filepath (concat spip-root dir file)))

    (find-file filepath)
    (if (not (file-exists-p filepath))
        (insert-file-contents spip-helm-original-file))))

(defun spip-overload ()
  "Overload the current file.

Ask for a destination directory, then create a new file at the
right place and copy the original content inside it. If the
target file already exists, we simply open it."
  (interactive)
  (helm :sources '(helm-source-spip-overload)))

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
  "Return the lang string at point, or nil if the point isn't on one."

  (let* ((reg (spip-font-block-at-point))
         (beg (car reg))
         (end (cdr reg)))
    (cond
     ((and (equal mode-name "Web")
           (equal (get-text-property (point) 'face)'
                  web-mode-block-string-face))
      (let ((match (s-match "<:\\(.+\\):>"
                            (buffer-substring-no-properties beg end))))
        (elt match 1)))
     ((and (equal mode-name "PHP/l")
           (equal (get-text-property (point) 'face)
                  font-lock-string-face))
      (let ((match (s-match "['\"]\\(.+\\)['\"]"
                            (buffer-substring-no-properties beg end))))
        (if (s-contains-p ":" (elt match 1))
            (elt match 1)))))))

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
                            (get-spip-path))))))

(defun spip-jump-to-lang-string-definition ()
  "Jump to the definition of the lang string at point."
  (interactive)

  (let* ((full-string (spip-lang-string-at-point))
         (module (car (s-split ":" full-string)))
         (lang-key (cadr (s-split ":" full-string)))
         (module-file (spip-find-in-path
                       (format "lang/%s_fr.php" module))))

    (with-current-buffer (find-file (concat spip-root module-file))
      (let ((selection-beg nil)
            (selection-end nil))
        (goto-line 1)
        (save-excursion
          (re-search-forward (format "['\"]%s['\"]" lang-key))
          (setq selection-beg (re-search-forward "['\"]"))
          (setq selection-end (- (re-search-forward
                                  (format "[^\\]%s"
                                          (buffer-substring (- (point) 1)
                                                            (point))))
                                 1)))
        (goto-char selection-beg)
        (push-mark selection-end)
        (setq mark-active t)))))

;;;;;;;;;;;
;; Utils

(defun spip-eval-php (php-code)
  "Evaluates the given php code in the current SPIP instance
  using SPIP-CLI's php:eval command."

  (if (not (string= (shell-command-to-string "which spip") ""))
      (shell-command-to-string (format "spip php:eval \"%s\"" php-code))
    (error "Couldn't find the spip executable.")))

(defun spip-get-directory (filename)
  "Returns the directory component of FILENAME."

  (if (stringp filename)
      (s-append "/"
                (directory-file-name
                 (mapconcat  'identity
                             (reverse (cdr (reverse (split-string filename "/"))))
                             "/")))))

(provide spip-mode)
;;; spip-mode.el ends here
