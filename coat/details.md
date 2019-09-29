## Structure de l'application

Chaque onglet propose des méthodes et des indicateurs permettant d'appréhender les mobilités franciliennes. Les onglets présentent un choix d'indicateurs dans le panneau de gauche et un choix de filtres dans le panneau de droite.
- **Scénario :** indicateurs agrégés à la commune et surtout **Choix du scénario** (panneau central).
- **Oursins :** visualisation des flux pour une origine ou une destination donnée.
- **Structure :** visualisation synthétique des territoires dessinés par les flux.
- **Synthèse :** indicateurs globaux des flux et des distances parcourues, permettant de comparer l'actuel avec le scénario choisi dans le premier onglet.


## Détails techniques

Tous les calculs sont effectués à partir des navettes domicile-travail produites par le recensement de la population 2014, données [accessibles sur le site de l'Insee](https://www.insee.fr/fr/statistiques/2866308?sommaire=2866354). Une navette domicile-travail est définie comme le lien entre la commune de résidence d'un individu actif occupé et sa commune de travail. La seule information spatiale disponible dans le recensement est donc un couple de communes. Cette limitation implique plusieurs considérations concernant la précision et la portée de ces données.


### Signification du lieu de résidence et du lieu de travail

De nombreux actifs ont un seul domicile et un seul lieu de travail, qui est à la fois le siège officiel de l'employeur et le lieu d'exercice du travail. Mais il existe plusieurs exceptions à cette situation simple, exceptions qui posent problème dans la détermination des distances parcourues. En voici quelques unes :

- la multirésidence : les individus qui ont plusieurs domiciles ne peuvent pas déclarer leur multirésidence (par exemple les jeunes actifs de familles recomposées résidant aux domiciles des deux parents)
- les lieux de résidence communautaires : les individus qui ont un lieu de résidence individuel et un lieu de résidence communautaire (par exemple les militaires qui résident en casernes, casernes qui sont aussi leur lieu de travail)
- les métiers sans lieu d'exercice fixe : les individus dont le travail est effectué en itinérance (par exemple les chauffeurs de taxi, les livreurs, etc.)
- les salariés à domicile : les individus qui travaillent à domicile pour une entreprise extérieure, c'est-à-dire qui ne sont pas auto-entrepreneurs (par exemple les opérateurs téléphoniques à domicile). Cette catégorie explique une bonne partie des "déplacements immobiles". Pour un actif qui est opérateur téléphonique à domicile, le recensement peut enregister la déclaration de la commune où siège l'entreprise, possiblement lointaine de la résidence de l'actif, mais enregistrer comme déclaration du mode de transport la catégorie "immobile".

### Question de la population étudiée

Par définition les navettes domicile-travail concernent la **population adulte active et occupée**. Elle exclut donc les enfants, les retraités, les chômeurs et les inactifs. 

L'application permet d'explorer les mobilités à l'intérieur de l'Île-de-France, elle affiche les déplacements qui ont leur origine et leur destination en Île-de-France, c'est-à-dire les actifs qui résident et travaillent dans la région. 

### Question de la fréquence

On considère que les navettes domicile-travail sont une approximation des déplacements quotidiens. En effet, de nombreux actifs ont un seul domicile et un seul lieu de travail, et font quotidiennement l'aller-retour (c'est le sens du terme "navette") entre lieu de résidence et lieu de travail. Cependant, il existe des exceptions et le recensement de la population ne permet pas de les saisir. La question de la fréquence de déplacement se pose en particulier dans deux situations :

- le travail à temps partiel : un individu déclare un lieu de travail mais il n'y exerce que 1 jour par semaine, auquel cas la navette n'est pas un déplacement quotidien mais un déplacement hebdomadaire.
- le télétravail : un actif peut se rendre sur son lieu de travail 4 jours par semaine et faire du télétravail 1 jour par semaine, auquel cas il ne s'agit pas non plus d'un déplacement quotidien.

### Question de la distance

On peut définir au moins trois types de distances à partir d'un couple de lieux :

- la distance parcourue sur le réseau par l'individu qui recherche le chemin le plus court en temps (c'est souvent l'option de l'automobiliste). Ainsi, un actif qui réside à Montreuil et qui se rend à Clichy en voiture empruntera probablement le périphérique. Ce chemin n'est pas le plus court en distance mais c'est le plus court en temps, il représente une distance de 13 km.
- la distance parcourue sur le réseau par l'individu qui recherche le plus court chemin en distance (c'est souvent l'option du piéton). L'actif qui réside à Montreuil et se rend à Clichy traverserait le Nord-Est parisien (20e, 19e, 10e, 18e et 17e arrondissements) et atteindrait Clichy au bout de 11,8 km.
- la distance à vol d'oiseau entre le centroïde de la commune de Montreuil et le centroïde de la commune de Clichy. Cette dernière distance est appelée **portée**. Elle ne correspond pas à un parcours réel sur le réseau mais elle s'en rapproche et elle est simple à calculer pour toutes les paires de communes. Dans cet exemple, la distance à vol d'oiseau entre Montreuil et Clichy est de 11,4 km. 

Les distances indiquées dans l'application sont des **portées entre commune de résidence et commune de travail**. Il faut donc prendre en compte deux caractéristiques importantes de cette mesure : 1/ la portée est en général plus courte que la distance parcourue sur le réseau et 2/ la portée ne tient compte que de l'aller et non de l'aller-retour. Ainsi, le cumul des distances associées à tous les flux franciliens donne un total de 50 millions de kilomètres. La distance réellement parcourue sur le réseau, pour l'aller et pour le retour, est plus proche du double, soit environ **100 millions de kilomètres**.

Pour une estimation plus précise des portées et des distances réellement parcourues en Île-de-France, il est plus intéressant d'utiliser l'Enquête Globale Transport. Le calcul des portées des déplacements domicile-travail et des déplacements liés au travail indique un total de 98 millions de kilomètres sur un total de **180 millions de kilomètres** parcourus quotidiennement (incluant tous les motifs de déplacements et toutes les catégories de population). Ces navettes domicile-travail représentent ainsi 54 % de la distance totale parcourue, tous motifs confondus, par les Franciliens.

### Bibliographie

**Sur les navettes domicile-travail**

- Berroir S., Mathian H., Saint-Julien Th., Sanders L. (2004), *Mobilités et polarisations : vers des métropoles polycentriques*, Étude réalisée pour le PUCA, 145 p.
- Commenges H., Fen-Chong J. (2017) "Navettes domicile-travail : naissance et développement d'un objet statistique structurant", *Annales de géographie*, nº715, pp.333-355.

**Sur le calcul des distances**

- Courel J. (2008) *170 millions de kilomètres*, Institut d'Aménagement et d'Urbanisme de la Région Île-de-France.
- Héran F. (2009) "Des distances à vol d'oiseau aux distances réelles ou de l'origine des détours", *Flux*, nº 76-77, pp.110-121.

**Sur les méthodes de construction de structures**

- Nystuen J., Dacey M. (1961), "A graph theory interpretation of nodal regions", *Papers and Proceedings of the Regional Science Association*, vol.7, pp.29-42.
- Newman M.E.J. (2006) "Modularity and community structure in networks", *PNAS*, vol.103, nº23, pp.8577-8582.






