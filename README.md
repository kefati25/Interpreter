## 1.Description

Kawai est un langage de programmation orienté objet inspiré de Java, compilé avec OCaml. Il propose des fonctionnalités similaires à Java avec des classes, l'héritage, des méthodes, des tableaux, et un système d'exécution.

### 1.1 Structure du projet

kawai-project/
├── src/
│   ├── kawai.ml          # Point d'entrée principal
│   ├── kawaparser.mly    # Fichier de grammaire pour le parser
│   ├── kawalexer.mll     # Définition du lexer
│   ├── kawa.ml           # Définition des types AST
│   ├── interpreter.ml    # Interpréteur pour exécuter le code
│   └── typechecker.ml    # Vérificateur de types
├── Makefile              # Fichier de compilation
└── README.md             # Ce fichier


## 2.PROBLEMES RENCONTRES
### 2.1 Erreur liée à l'écriture de nombres négatifs

Des difficultés sont apparues lors de l'écriture de plusieurs nombres négatifs dans une partie du code. 
Le programme ne reconnaît pas correctement tous les nombres écrits avec un espace inséré entre le signe moins 
et le chiffre comme dans l'exemple « - 7 ». Par conséquent, la syntaxe précise est "-7" sans aucun espace.

### 2.2 Erreur dans la fonction check

Une erreur manifestement critique dans la fonction de vérification du vérificateur de types empêche le bon 
fonctionnement du programme. Ce code a pour fonction de vérifier que chaque type d'expression correspond 
à un type attendu.
Malgré cela, l'exécution du fichier var.kwa produit une erreur systématique qui affiche ce message :

Anomaly: Dune__exe__Typechecker.Error("expected int, got int")

Une anomalie dans la gestion de plusieurs types de données du programme semble causer cette erreur. 
Le message d'erreur indique que un seul type de donnée entier est attendu par le programme, 
alors qu'un type de donnée entier a déjà été fourni. 
Avant même l’exécution du fichier « var.kwa », cette situation intervient. 
L’origine de cette erreur est restée indéterminée, ce qui a par conséquent empêché le test 
de plusieurs autres fonctions du programme.

## 3. Extensions traitées
### 3.1  Implémentation du type array
L'extension du type array a été ajoutée à la fois dans le parser et l'interpréteur. 
Le type array est défini dans le parser de la manière suivante :

t = typ LB RB { TArray(t) }

Dans l'interpréteur, le type array est représenté par :

| VArray of typ * value array

3.1.1 Déclaration d'un tableau
Il existe deux façons de déclarer un tableau dans ce langage :
Déclaration sans valeur initiale (dans la fonction main) :

var int[] arr2;  // Déclaration du tableau
main {
  arr2 = {1, 2, 3};  // Initialisation du tableau
}

Déclaration avec valeur initiale :

var int[] arr2;  
main {
  arr2 = new int[5];  // Création d'un tableau de 5 éléments
}

#### 3.1.2 Affectation et accès aux éléments du tableau
L'affectation et l'accès aux éléments du tableau se font de la 
manière suivante :
 - Affectation d'une valeur dans un tableau : arr2[5] = 2
 - Accès à un élément du tableau : arr2[5]

3.1.3 Tableaux multidimensionnels
Il est également possible de créer des tableaux multidimensionnels.
Voici deux exemples :
    - Définition d'un tableau de tableaux :
var int[][] arr2 = new int[7][5];  // Tableau de 7 tableaux de 5 éléments

    - Déclaration et initialisation d'un tableau de tableaux :
var int[][] arr2;  
main {
  arr2 = {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}
  }
}

#### 3.1.4 Accès et modification des éléments d'un tableau multidimensionnel


Les éléments d'un tableau multidimensionnel peuvent être accédés et modifiés de 
la manière suivante :
    - Accès à un élément : arr[5][1]
    - Modification d'un élément : arr[5][1] = 0


### 3.2 Limites de la déclaration avec valeur initiale

L'extension de la déclaration avec une valeur initiale est limitée à trois cas :
    - Déclaration d'une variable simple :
        var int a = 2;
    - Déclaration d'un tableau sans initialisation :
        var int[] arr2 = new int[7];  // Création d'un tableau de 7 éléments
    - Déclaration d'une variable booléenne :
        var bool b = true;

### 3.3 Limitations supplémentaires

Il est important de noter qu'il n'est pas possible de déclarer un tableau d'objets dans ce langage.

## 4. Conclusion

Ce projet a permis d'implémenter des extensions liées au type array et de résoudre certains problèmes liés à la syntaxe et à la gestion des types. Cependant, une erreur persistante dans le type checker a empêché de tester l'intégralité des fonctionnalités du programme. Des efforts seront nécessaires pour résoudre cette erreur et valider l'ensemble des fonctionnalités.
