## Description

Kawai est un langage de programmation orienté objet inspiré de Java, compilé avec OCaml. Il propose des fonctionnalités similaires à Java avec des classes, l'héritage, des méthodes, des tableaux, et un système d'exécution.

## Structure du projet

kawai-project/
├── src/

│   ├── kawai.ml          # Point d'entrée principal

│   ├── kawaparser.mly    # Fichier de grammaire pour le parser

│   ├── kawalexer.mll     # Définition du lexer

│   ├── kawa.ml           # Définition des types AST

│   ├── interpreter.ml    # Interpréteur pour exécuter le code

│   └── typechecker.ml    # Vérificateur de types

├── Makefile              # Fichier de compilation

├── README.md             # Ce fichier

└── examples/             # Dossier avec des exemples de code Kawai

    └── example.kw
