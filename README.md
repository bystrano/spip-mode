SPIP-mode
=========

Un mode mineur pour le CMS SPIP (http://spip.net).

Active automatiquement la coloration syntaxique des squelettes SPIP
dans web-mode, et ajoute des commandes utiles.

Installation
------------

Pour que toutes les commandes fonctionnent correctement, il faut avoir installé
[spip-cli](https://contrib.spip.net/SPIP-Cli).

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
avoir placé le curseur sur une chaîne de langue. On choisit alors la
langue pour laquelle on souhaite voir la définition de la chaîne de
langue. Si cette chaîne de langue n'existe pas encore, on propose de
la créer automatiquement au bon endroit.

La commande `spip-make-new-lang-string` permet de créer
automatiquement une chaîne de langue à partir de la région
sélectionnée. On saisit alors le nom de la chaîne de langue, qui est
alors ajoutée dans le bon fichier.
