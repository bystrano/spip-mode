SPIP-mode
=========

Un mode mineur pour le CMS SPIP (http://spip.net).

Configure Emacs pour le CMS SPIP. Web-mode colore les boucles, filtres
et balises SPIP.

On fournit également des commandes utiles pour la surcharge de
fichiers et la gestion des chaînes de langue.

Installation
------------

Il faut activer SPIP mode sur les modes que l'on souhaite utiliser, en
ajoutant le code suivant dans sont fichier `.emacs` :


``` elisp
(add-hook 'web-mode-hook  'spip-mode-web-mode-config)
(add-hook 'php-mode-hook  'spip-mode-php-mode-config)
(add-hook 'nxml-mode-hook 'spip-mode-nxml-mode-config)
(add-hook 'css-mode-hook  'spip-mode-css-mode-config)
```

Commandes
--------

La commande `spip-overload` permet de surcharger le fichier courant.
On propose une liste de répertoires candidats pour la surcharge, et
la commande se charge ensuite de créer un nouveau fichier au bon
endroit, d'y copier le contenu du fichier actuel et de l'ouvrir
dans un nouveau buffer.

La commande `spip-eval-region` permet d'évaluer le résultat d'un code
php dans un contexte SPIPien, i.e. de charger les fichiers et
librairies de SPIP avant l'exécution.

La commande `spip-jump-to-lang-string-definition` s'utilise après
avoir placé le curseur sur une chaîne de langue. On choisit alors
la langue pour laquelle on souhaite voir la définition de la chaîne
de langue.
