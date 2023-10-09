---
title: "Qualite de l'air"
author: "Renée Le Clech"
date: '2023-09-27'
output:
  pdf_document: default
  html_document: default
editor_options:
  markdown:
    wrap: 72
---

# Analyse de la qualité de l'air à rennes.


```r
# Chargement des packages utilisé dans la suite du script
library("data.table")
library("dplyr")
library("car")
library("FactoMineR")
```

## a) Présentation des données

Les données météorologiques issues de Meteostat que nous avons
présentées dans les onglets 'Présentation' et illustrées dans les
onglets 'Carte interactive' et 'Données météo' représentent un ensemble
de 10 variables de type Float64.

| Variables |             Description              | Unité |
|:---------:|:------------------------------------:|:-----:|
|   tavg    |         Température moyenne          |  °C   |
|   tmin    |      Températrure minimum en °C      |  °C   |
|   tmax    |      Températrure maximum en °C      |  °C   |
|   prcp    |      Précipitation quotidienne       |  mm   |
|   snow    |        Profondeur de la neige        |  mm   |
|   wdir    |     la direction moyenne du vent     |   °   |
|   wspd    |     La vitesse moyenne du vente      | km/h  |
|   wpgt    |       Rafale de vent maximale        | km/h  |
|   pres    | Pression moyenne au niveau de la mer |  hPa  |
|   tsun    | Le total d'ensoleillement quotidien  |  min  |

Nous avons parallèlement récupéré des données de qualité de l'air issues
de Atmo France (Fédération des Associations agréées de surveillance de
la qualité de l'air).

Nous nous sommes particulièrement interessés à l'indice ATMO, qui est un
facteur à 6 modalités catégorielles : "Bon" , "Moyen", "Degradé",
"Mauvais", "Très mauvais" et "Extrèmement mauvais"

*" L'indice ATMO est calculé quotidiennement à l'échelle de chaque
commune ou au maximum à l'échelle intercommunale.\
Il est déterminé, à partir des concentrations de 5 [polluants
réglementés](https://www.atmo-france.org/article/air-exterieur "Air extérieur")
:*

-   *Les particules fines inférieures à 10 micromètres : les PM~10~ ;*

-   *Les particules fines inférieures à 2,5 micromètres : PM~2.5~ ;*

-   *Le dioxyde d'azote (NO~2~)*

-   *L'ozone (O~3~) ;*

-   *Le dioxyde de soufre (SO~2~)."* - Site Atmo-france.org

![](images/tableau.JPG){width="539"}

[Table 1:]{.underline} tableau des seuil de calcul des indices ATMO pour
les 5 polluants indicateurs de la pollution - source: Site
Atmo-france.org

Nos but est de mettre en relation les données météorologiques avec
l'indice de la qualité de l'air; de voir si certaines variables
météorologiques permettent d'expliquer l'indice ATMO.

## b) Importation des données


```r
df <- read.csv("C:/Users/renax/Desktop/ACO/S9/Programmation_R/Projet_meteo/Projet_Shiny/quality_index_rennes.csv", header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file 'C:/Users/renax/Desktop/ACO/S9/Programmation_R/Projet_meteo/Projet_Shiny/quality_index_rennes.csv': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
set.seed(45L)
dt_qualite_air <- data.table(df) # transformation en datatable

summary(dt_qualite_air[,23:31]) # résumé pour les données meteo, pour verifier les classes des colonnes
```

```
## Error in `[.data.table`(dt_qualite_air, , 23:31): Item 1 of j is 23 which is outside the column number range [1,ncol=1]
```

La variable à expliquer est la colonne code_qual'. Il s'agit d'une
variable catégorielle avec 4 modalités : "1", "2", "3", "4",
correspondant respectivement aux catégories d'air "Bon", "Moyen",
"Dégradé" et "Mauvais".

Il est important de noter que dans notre jeu de données, l'indice ATMO
ne va que de Bon" à "Mauvais". Les modalités "Très mauvais" et
"Extrêmement mauvais" ne sont donc pas résentes pour la ville de Rennes
entre septembre 2021 et septembre 2022.

## c) Preparation des données

En vue de la création d'un modèle logistique, il nous faut traiter au
préalable nos données.

### i. Variable réponse

Gestion des NA:

On remarque dans le summary des données meteo qu'on retrouve des données
manquantes pour les colonnes rafales de vent (wgpt), precipitaition
(prcp), direction du vent (wdir) et pression (pres).

Nous decidons d'enlever les lignes qui présente des données manquantes.


```r
# pour wdir
dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$wpgt)),]
dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$prcp)),]

# summary(dt_qualite_air[,23:31])
```

Les recommandations comportementales établies par le ministère en charge
de la Santé sont associées aux différents qualificatifs de l'indice
ATMO. Les recommandations distinguent deux situations: les indices "Bon
(1)" ou "Mauvais (2)" et les indices "Degradé (3)" à "Extrèmement
Mauvais (6)". Dans le premier cas, il existe des recommandations
d'avantage préventive, et dans le deuxième cas il y a des
recommandations plus rudes pour reduire l'impact de la pollution sur la
santé et sur l'environnement.

A des fins de traitement statistique, nous décidons alors de regrouper
les catégories 1 et 2 entre elles et les catégories 3 et 4 entre elles
dans la nouvelle colonne *qualite_air_groupe:*


```r
# creation de la colonnes qualite_air_groupe
dt_qualite_air[, qualite_air_groupe := ifelse(code_qual %in% c(1, 2), "Groupe 1-2",
                                   ifelse(code_qual %in% c(3, 4), "Groupe 3-4", "Other"))]
```

```
## Error in eval(jsub, SDenv, parent.frame()): object 'code_qual' not found
```

```r
dt_qualite_air$qualite_air_groupe<- as.factor(dt_qualite_air$qualite_air_groupe)
```

Notre variable à expliquer est désormais la colonne *qualite_air_groupe
l'air* qui a 2 modalités: "Groupe 1-2" et "Groupe 3-4".

### ii. Variables explicatives

La variable de direction du vent *wdir* ( ° ) nous interpelle car son
unité étant des degrés, nous avons une situation dans laquelle notre
modèle va éloigner les valeur 0° et 360° alors que ce sont les meme.
Idem pour les valeurs 5° et 355°, le modèle va les considérer comme très
éloignées alors qu'en réalité la direction du vent n'est différente que
de 10° .

Pour palier à cas, nous convertissons les degrés en une représentation
plus adaptée pour notre modèle logistique. Nous récuperons les
composantes x et y de la direction du vent par quelques traitements
trigonométriques:


```r
# Convertir les degrés en radians
dt_qualite_air <- dt_qualite_air %>%
  mutate(Angle_radians = wdir * pi / 180)
```

```
## Error in `mutate()`:
## ℹ In argument: `Angle_radians = wdir * pi/180`.
## Caused by error:
## ! object 'wdir' not found
```

```r
# Calculer les composantes x et y de la direction du vent
dt_qualite_air <- dt_qualite_air %>%
  mutate(Vent_x_est = cos(Angle_radians),
         Vent_y_nord = sin(Angle_radians))
```

```
## Error in `mutate()`:
## ℹ In argument: `Vent_x_est = cos(Angle_radians)`.
## Caused by error:
## ! object 'Angle_radians' not found
```

Avant de créer notre premier modèle nous remarquons que parmis nos
données explicatives potentielles, certaines sont liées. La température
moyenne est instrinsèquement liée à la température minimale et la
température maximale. Ainsi, par principe de parcimonie nous ne gardons
que la température moyenne sur les 3 variables de température.

## d) Visualisation des données en anayse factorielle

Afin d'avoir une vue d'ensemble des variables explicatives et de la
Variable réponse, nous faisons une ACP sur variables explicatives. Nous
rajoutons en supplémentaire la variables *qualite_air_groupe* afin de
determiner si elle se positioner de manière particulière face aux
varibles et aux individus.


```r
# Selection des colonnes qui nous interesse i.e les variables explicatives et la variable réponse
res<- PCA(dt_qualite_air[, c(22,23,24,25,28,29,30,32,34,35)],scale.unit = TRUE, quali.sup = 8)
```

```
## Error in `[.data.table`(dt_qualite_air, , c(22, 23, 24, 25, 28, 29, 30, : Item 1 of j is 22 which is outside the column number range [1,ncol=2]
```

```r
# enlever wdir

# Afficher le graphique PCA
plot(res, habillage = 8, label = "none")
```

```
## Error in eval(expr, envir, enclos): object 'res' not found
```

On vois sur le graph des individus colorés par groupe de qualité de
l'air que le groupe 1-2 se réparti le long des deux première dimension
de manière assez homogène. Les individus des groupes 3-4 sont legèrement
plus vers la gauche du graphique, et se répartisse de manière homogène
le long de la deuxième dimension. On retouve cela avec le barycentre des
groupes 1-2 et 3-4 qui sont environ aux meme coordonées sur la deuxième
dimension mais qui sont un peu différents sur la première

On peut donc supposer que les variables contribuant le plus à la
première dimension pourraient nous donner des informations sur la
qualité de l'air. Il est cependant difficile de séparer complètement les
deux groupes d'individus.

## e) Création des modèles

Nous faisons dans un premier temps un modèle logistique complet avec
l'ensemble des variables explicatives, et nous cherchons un modèle final
par le biais d'une selection de modèle par élimination descendante avec
un critère de test de rapport de vraissemblance (Likelihood Ratio Test).

ichez les résultats de l'ANOVA

print(resultat_anova)


```r
# 1. Ajustez le modèle complet
mod_complet <- glm(qualite_air_groupe ~ tavg + prcp + wspd + wpgt + pres + Vent_x_est + Vent_y_nord, data = dt_qualite_air, family = "binomial")
```

```
## Error in eval(predvars, data, env): object 'tavg' not found
```

```r
# 2. Ajustez le modèle 2 (sans la variable 'wpgt')
mod_2 <- glm(qualite_air_groupe ~ tavg + prcp + wspd + pres + Vent_x_est + Vent_y_nord, data = dt_qualite_air, family = "binomial")
```

```
## Error in eval(predvars, data, env): object 'tavg' not found
```

```r
# 3. Comparez les deux modèles en utilisant la fonction anova()
anova(mod_complet, mod_2, test="F")
```

```
## Error in eval(expr, envir, enclos): object 'mod_complet' not found
```

```r
summary(mod_2)
```

```
## Error in eval(expr, envir, enclos): object 'mod_2' not found
```

```r
## On recommence avec un autre modèle où on enlève pres
mod_3 <- glm(qualite_air_groupe ~ tavg + prcp + wspd + Vent_x_est + Vent_y_nord, data = dt_qualite_air, family = "binomial")
```

```
## Error in eval(predvars, data, env): object 'tavg' not found
```

```r
anova(mod_2, mod_3, test="F")
```

```
## Error in eval(expr, envir, enclos): object 'mod_2' not found
```

```r
# on garde le modèle 3
summary(mod_3)
```

```
## Error in eval(expr, envir, enclos): object 'mod_3' not found
```

Tous les paramètres de notre modèles sont significatifs.

Regardons en detail les effets de ces variables.D'après le résumé, on
retrouve que:

-   La pression atmosphérique et les bourrasques de vent ne sont pas
    associées à la probabilité de bonne ou mauvaise qualité de l'air
-   L'augmentation de la température moyenne (tavg) est associée à
    l'augmentation de la probabilité de mauvaise qualité de l'air par
    rapport à la bonne qualité de l'air
-   A l'inverse, une augmentation des précipitation et de la vitesse du
    vent est associée à une augmentation de la probabilité de bonne
    qualité de l'air
-   Pour les valeurs de vents, on reprend les transformations
    trigonométriques qui nous avaient permis de créer ces variables, et
    on retrouve qu'une augmentation de vent_x et de vent_y, donc un vent
    vers l'est et vers le nord est associé à une augmentation de la
    probabilité de bonne qualité de l'air. Aussi, puisque le coefficient
    est deux fois superieur pour vent_y, on peut dire que la direction
    du vent associé à une augmentation de la probabilité de bonne
    qualité de l'air est NNE.

## Conclusion

Notre modèle suggère que la pluie, la température et le vent (la
direction et la vitesse) ont une influence sur l'indice ATMO, qui rend
compte de la qualité de l'air. La météo semble ainsi pouvoir expliquer
du moins en partie la qualité de l'air. Les bourasques de vents et la
pression atmosphérique n'ont pas d'influence sur l'indice ATMO.

Les informations que nous avons mises en évidence sont des premiers
resultats et de nombreux facteurs, d'avantage liés à la cause des
emission des gaz polluants pourraient permettre de mieux comprendre
l'indice de ATMO.

La meteo nous permet ainsi d'expliquer en partie la qualité de l'air, et
ainsi, il semblerait être possible de prévoir la qualité de l'air, avec
des informations sur les emissions de polluants croisées avec des
données météo prévues par les institutions spécialisées.
