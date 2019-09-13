## Description

Représenter la matrice de flux dans son ensemble donne un résultat illisible (dans le cas francilien, on observe plus de 300000 couples de lieux à représenter). L'analyse des structures permet d'extraire de la matrice de flux un squelette lisible représentant la structuration principale des mobilités.

Deux méthodes de représentation sont utilisées : les **flux dominants** et la **modularité**. 

La méthode des **flux dominants** consiste à sélectionner pour chaque commune uniquement le flux principal à destination d'une commune plus grande que soi. Si Poincy envoie son flux principal vers Meaux, seul ce flux est représenté à l'origine de Poincy, représentant une domination de Poincy par Meaux. Ensuite, si Meaux envoie son flux principal vers Paris, seul ce flux est représenté à l'origine de Meaux, représentant une domination de Meaux par Paris. On affiche ainsi une structure spatiale hierarchisée de la mobilité. Cette méthode crée ce qui est appelé en théorie des graphes une **forêt**, c'est-à-dire un ensemble d'arbres avec trois types de noeuds : dominant, intermédiaire, dominé.

La méthode de la **modularité** produit une partition cohérente du territoire en fonction de la matrice de flux. Cela consiste à former des ensembles de communes (*clusters*) qui tissent entre elles de fortes relations et qui sont reliées au reste du territoire par de faibles relations. En d'autres termes c'est un découpage du territoire qui maximise la mobilité intragroupe et qui minimise la mobilité intergroupe

