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
