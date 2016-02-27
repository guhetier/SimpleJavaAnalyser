---
title : INF565 - Simple Java Analysis
author : Guillaume Hétier
header-includes:
- \usepackage{fullpage}
---

# Documentation

## Compilation

Après avoir récupéré les sources (dans l'archive ou sur <https://github.com/xNephe/SimpleJavaAnalyser>), on peut compiler le projet à l'aide de ocamlbuild :

```bash
ocamlbuild analyzer.native
```

## Utilisation

```bash
analyzer.native --[option] [fichier]
```

Les options acceptées sont :

--print
:   Affiche le code du programme généré à partir de l'arbre de syntaxe abstaite

--interpret
:   Interprete le fichier passé en argument

--vinit
:   Recherche les éventuelles erreurs d'initialisations

--vtype
:   Recherche les éventuelles erreurs de typage

--constant
:   Execute une propagation des variables en utilisant le treillis des constantes comme type abstrait

--interval
:   Execute une propagation des variables en utilisant le treillis des intervalles comme type abstrait

Le fichier est un fichier contenant du code java dans les restrictions fixées par le sujet (pas de variables autre que statiques, uniquement des procédures, uniquement des ints et des booléens).
Je n'ai pas réalisé d'extension du langage.

Un jeu de tests est disponible dans le dossier "example".

#Implémentation

## Trois analyses indépendantes

J'ai réalisé l'affichage, l'interprétation, l'analyse de type et l'analyse de l'initialisation de manière indépendante :
dans chacun des cas, l'analyse parcours l'arbre de syntaxe abstraite en mettant un jour un environnement (une table de hachage) qui relie chaque variable aux données pertinentes la concernant.
Cet environnement contient donc la valeur d'une variable dans le cas de l'interprétation, son type ou son état (défini, peut être défini, non défini) selon les analyses.

## Du code factorisé

Dans le cas des analyses par propagation des variables (constantes et intervalles), j'ai factorisé le code pour l'organiser plus proprement et éviter les répétitions.

Le code utilise ainsi deux foncteurs.

- le premier prend en paramètre un treillis abstrait de valeurs des variables et génère un module "environnement", qui gère le lien entre chaque variable et une valeur du treillis
- le second prend en paramètre un environnement et génère un module capable de réaliser l'analyse d'un arbre de syntaxe abstraite.

Cette approche m'a permis d'effectuer une division claire du code.
De plus, il suffit de définir le treillis (un type et un ensemble d'opération de base) afin d'obtenir l'analyse sur un autre ensemble abstrait, ce qui permet de passer simplement des constantes aux intervalles.

On peut remarquer que avec un environnement et un treillis adapté, on pourrait éventuellement réutiliser ces foncteurs pour réaliser l'analyse des types et des définitions.
Il ne m'a cependant pas semblé judicieux de le faire, pour des raisons de clarté. En effet, l'analyse par propagation étant plus complexe, une partie du foncteur n'aurait pas été utile et il aurait fallut s'assurer que l'analyse soit valide.

##Choix de méthodes

Je vais ici expliquer les choix que j'ai réalisé, dans le cadre de l'analyse pas propagation des variables. En effet, les premières questions laissent assez peut de place aux variations.

Concernant la plupart des nœuds de l'arbre de syntaxe abstrait, il suffit d'actualiser l'environnement en fonction de l'instruction traitée, de manière directe.
Seuls les conditions ($if$) et les boucles ($while$) demandent une approche plus complexe.

Dans le cas des tests, deux cas se présentent selon l'évaluation de la condition : si sa valeur est certaine ($true$ ou $false$), on vérifie la branche correspondante en marquant l'autre comme inaccessible.

Dans le cas de la boucle, on procède par élargissement des variables.
Tout d'abord, on exécute un certain nombre de fois le corps de la boucle, en vérifiant si l'on sort de la boucle dans ces premières itérations.
J'ai choisi arbitrairement d'exécuter ainsi 10 fois le corps d'une boucle.
Ensuite, a chaque itération, on élargie les variables modifiées vers Top. Si du code inaccessible a été découvert dans l'itération précédente, on n'élargit pas les variables car ce nouveau code peut permettre de conclure directement.

Enfin, j'ai choisi de ne pas faire la distinction entre les booléens et les entiers dans l'analyse par propagation. Il m'a parut plus clair de séparer complètement cette dernière de l'analyse des types. Les booléens y sont donc représentés par les entiers 0 et 1.

# Mise en perspective

Après avoir terminé l'implémentation de ce projet, j'ai pu faire les remarques suivantes :

- avoir utilisé un gestionnaire de version dès le départ a été extrêmement utile afin d'améliorer mon code sans risquer de perdre mes résultats précédents
- au cours du projet, j'ai factorisé mon code plusieurs fois afin d'en clarifier l'organisation et éviter les répétitions. Ce travail sur le code m'a aussi permis de mieux comprendre et séparer les représentations concrètes et abstraites des états possibles d'une variable.
- ayant utilisé initialement des tables de hachage avant de plutôt opter pour des maps, j'ai pu vérifier que le code (au niveau de la concision, de la clarté, mais aussi du risque d'erreurs) est très dépendant des structures de données utilisées.



