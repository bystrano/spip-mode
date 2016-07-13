;;; spip-mode.el --- Un mode mineur pour SPIP

;;; Commentary:

;;; Un mode mineur pour le CMS SPIP (http://spip.net).

;;; Configure web-mode, php-mode et javascript mode pour le CMS
;;; SPIP. PHP-mode utilise les règles de codage SPIPiennes,
;;; web-mode colore les boucles, filtres et balise SPIP.

;;; TODO:

;;; Fournit des commandes pour la gestion des chaînes de langue

;;; Fournit des commandes pour la gestion des surcharges de
;;; fichiers du core.

;;; Code:

(require 'json)

(defvar spip-root nil
  "The root directory of the current SPIP project.")

(define-minor-mode spip-mode
  "A minor mode for the SPIP CMS."
  nil " SPIP " nil
  (if (not (stringp spip-root))
      (setq spip-root (get-spip-root)))
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

(defun spip-eval-php (php-code)
  "Evaluates the given php code in the current SPIP instance
  using SPIP-CLI's php:eval command."

  (if (not (string= (shell-command-to-string "which spip") ""))
      (shell-command-to-string (format "spip php:eval \"%s\"" php-code))
    (error "Couldn't find the spip executable."))
  )

(defun spip-get-directory (filename)
  "Returns the directory component of FILENAME."

  (if (stringp filename)
      (directory-file-name
       (mapconcat  'identity
                   (reverse (cdr (reverse (split-string filename "/"))))
                   "/"))))

(provide spip-mode)
;;; spip-mode.el ends here
