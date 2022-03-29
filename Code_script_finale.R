###### Projet introduction à R    ######
#####******************************#####
#####    installation des packages #####
#####***************************** #####
#install.packages("factoextra")
#install.packages("normtest")
#install.packages("FactoMineR")
#install.packages("ggplot2")
#install.packages("lmtest")
#install.packages("readtext")
#install.packages("ggcorrplot")
#install.packages("readxl")
#install.packages("carData")
#install.packages("car")


#####* #### Nettoyer la "mémoire vive" de R: supprimer tous les objets crées
rm(list = ls()) # supprimer tous les objets
ls() #afficher tous les objets qui ont été créés

########################################
#####******************************#####
#####      les librairies          #####
#####******************************#####
library(lmtest)
library(ggplot2)
library(normtest)
library(factoextra)
library(FactoMineR)
library(car)
library(dplyr,quietly = T)
library(questionr)
library(data.table)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
library(RColorBrewer)

##***********************************************##
############## Importation de la base de données ###############
##***********************************************##
##*## Working Directory (répertoire de travail)
setwd("C:/Users/fofna/OneDrive/Bureau/Cour_2021_2022/EWCS_2015")

getwd()
#

ewcs_2015 <-  read.csv("ewcs_2015.csv",header = T, sep=",")
attach(ewcs_2015)

####### Choix des variables pour notre études ##########
ewcs_2015_new = subset(ewcs_2015,select=c(Country,Q2a,Q2b,Q2d,Q7,Q14,Q16b,Q17,Q24,Q88,Q104_euro,ISCED,isco_08_1 ))


### Ligne de code ligné aux chioix de couleur
coul <- brewer.pal(8, "Set2")

###########################################################
############# Pretraitement de la base de donnée ##########
###########################################################

## Recodage de la variable Q2a
ewcs_2015_new$Q2a <- ewcs_2015_new$Q2a
ewcs_2015_new$Q2a[ewcs_2015_new$Q2a == "Don't know (spontaneous)"] <- NA
ewcs_2015_new$Q2a <- factor(ewcs_2015_new$Q2a)


## Recodage de la variable Q2b
ewcs_2015_new$Q2b <- ewcs_2015_new$Q2b
ewcs_2015_new$Q2b[ewcs_2015_new$Q2b == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q2b[ewcs_2015_new$Q2b == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q2b <- as.numeric(ewcs_2015_new$Q2b)

## Recodage de ewcs_2015_new$Q16b en ewcs_2015_new$Q16b_rec
ewcs_2015_new$Q16b <- ewcs_2015_new$Q16b
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "1 (interviewee works alone)"] <- "Pet_entreprise"
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "10-249"] <- "moy_entreprise"
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "2-9"] <- "Pet_moy_entreprise"
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "250+"] <- "Grde_entreprise"
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q16b[ewcs_2015_new$Q16b == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q16b <- factor(ewcs_2015_new$Q16b)


## Recodage de la variable Q2d
ewcs_2015_new$Q2d[ewcs_2015_new$Q2d == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q2d[ewcs_2015_new$Q2d == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q2d <- factor(ewcs_2015_new$Q2d)## Recodage de ewcs_2015_new$Q2d


## Recodage de la variable isco_08_1
ewcs_2015_new$isco_08_1[ewcs_2015_new$isco_08_1 == ""] <- NA
ewcs_2015_new$isco_08_1 <- factor(ewcs_2015_new$isco_08_1)


## Recodage de la variable Q104_euro
ewcs_2015_new$Q104_euro <- ewcs_2015_new$Q104_euro
ewcs_2015_new$Q104_euro[ewcs_2015_new$Q104_euro == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q104_euro[ewcs_2015_new$Q104_euro == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q104_euro <- as.numeric(ewcs_2015_new$Q104_euro)



## Recodage de la variable Q7
ewcs_2015_new$Q7 <- ewcs_2015_new$Q7
ewcs_2015_new$Q7[ewcs_2015_new$Q7 == "Don't know (spontaneous)"] <- NA
ewcs_2015_new$Q7[ewcs_2015_new$Q7 == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q7 <- factor(ewcs_2015_new$Q7)


## Recodage de la variable Q14
ewcs_2015_new$Q14 <- ewcs_2015_new$Q14
ewcs_2015_new$Q14[ewcs_2015_new$Q14 == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q14[ewcs_2015_new$Q14 == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q14 <- factor(ewcs_2015_new$Q14)

## Recodage de la variable Country (pays)
ewcs_2015_new$Country <- factor(ewcs_2015_new$Country)#irec(ewcs_2015_new, "Q17")

## Recodage de la variable Q17
ewcs_2015_new$Q17 <- ewcs_2015_new$Q17
ewcs_2015_new$Q17[ewcs_2015_new$Q17 == "DK"] <- NA
ewcs_2015_new$Q17[ewcs_2015_new$Q17 == "Less than 1 year"] <- "0"
ewcs_2015_new$Q17[ewcs_2015_new$Q17 == "Not applicable (spontaneous)"] <- NA
ewcs_2015_new$Q17[ewcs_2015_new$Q17 == "Refusal"] <- NA
ewcs_2015_new$Q17 <- as.numeric(ewcs_2015_new$Q17)


## Recodage de la variable Q24
ewcs_2015_new$Q24 <- ewcs_2015_new$Q24
ewcs_2015_new$Q24[ewcs_2015_new$Q24 == "DK/no opinion"] <- NA
ewcs_2015_new$Q24 <- as.numeric(ewcs_2015_new$Q24)


## Recodage de de la variable Q88
ewcs_2015_new$Q88 <- ewcs_2015_new$Q88
ewcs_2015_new$Q88[ewcs_2015_new$Q88 == "DK/no opinion (spontaneous)"] <- NA
ewcs_2015_new$Q88[ewcs_2015_new$Q88 == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q88 <- factor(ewcs_2015_new$Q88)


## Recodage de la variable Q104_euro
ewcs_2015_new$Q104_euro <- ewcs_2015_new$Q104_euro
ewcs_2015_new$Q104_euro[ewcs_2015_new$Q104_euro == "DK (spontaneous)"] <- NA
ewcs_2015_new$Q104_euro[ewcs_2015_new$Q104_euro == "Refusal (spontaneous)"] <- NA
ewcs_2015_new$Q104_euro <- as.numeric(ewcs_2015_new$Q104_euro)

## Recodage de la variable ISCED
ewcs_2015_new$ISCED <- ewcs_2015_new$ISCED
ewcs_2015_new$ISCED[ewcs_2015_new$ISCED == "DK"] <- NA
ewcs_2015_new$ISCED[ewcs_2015_new$ISCED == "Refusal"] <- NA
ewcs_2015_new$ISCED <- factor(ewcs_2015_new$ISCED)


### Boxplot de la variable salaire afin de supprimé les valeurs extrème
boxplot(ewcs_2015_new$Q104_euro)
ewcs_2015_new <- na.omit(ewcs_2015_new)


# Renommé les variables pour mieux comprendre notre étude
ewcs_2015_ren <- ewcs_2015_new%>% rename(Pays = Country,genre = Q2a, age = Q2b,Taille_entreprise = Q16b,salaire = Q104_euro,ancienneté = Q17,
                                         heure_par_semaine = Q24, secteur_trav = Q14, satisfaction=Q88,type_contrat = Q2d, situation = Q7, 
                                         nivx_etude = ISCED, profession = isco_08_1)
summary(ewcs_2015_new$Q104_euro)

## rempacements par des valeurs manquantes les valeurs qui sont 
## inferieur à 10% du quantile et superieur à 99% du quantile 
## pour une bonne analyse de notre base de données
d <- quantile(ewcs_2015_ren$salaire, probs=seq(0, 1, 0.01),na.rm = T)
ewcs_2015_ren$salaire[ewcs_2015_ren$salaire <= d[["2%"]]] <-NA
ewcs_2015_ren$salaire[ewcs_2015_ren$salaire >= d[["99%"]]] <-NA

##### suppression des NA
ewcs_2015_ren <- na.omit(ewcs_2015_ren)

boxplot(ewcs_2015_ren$salaire)
summary(ewcs_2015_ren$salaire)

#######################"
## Rajout des colonnes (heures par mois et taux horraires)

##************Heure par mois ****************##
heures_mois <- 4*ewcs_2015_ren$heure_par_semaine
names(heures_mois) <- "heures_mois"
ewcs_2015_ren <-cbind(ewcs_2015_ren,heures_mois)

## Taux horaires
##************Taux horaires****************##
taux_horaires <- ewcs_2015_ren$salaire/ewcs_2015_ren$heures_mois
names(taux_horaires) <- "taux_horaires"
ewcs_2015_ren <-cbind(ewcs_2015_ren,taux_horaires)

## ordonner la base de données
## Réordonnancement de ewcs_2015_ren$niveau_etude
ewcs_2015_ren$nivx_etude <- factor(ewcs_2015_ren$nivx_etude,
                                   levels = c("Doctorate or equivalent", "Master or equivalent", "Bachelor or equivalent",
                                              "Short-cycle tertiary education", "Post-secondary non-tertiary education",
                                              "Upper secondary education", "Lower secondary education", "Early childhood education",
                                              "Primary education"))


## Renommé le niveau d'étude
ewcs_2015_ren$nivx_etude <- as.character(ewcs_2015_ren$nivx_etude)
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Doctorate or equivalent"] <- "Doctorat"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Master or equivalent"] <- "Master"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Bachelor or equivalent"] <- "Licence"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Short-cycle tertiary education"] <- "BTS_DUT"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Post-secondary non-tertiary education"] <- "Post_secondaire"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Upper secondary education"] <- "Lycée"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Lower secondary education"] <- "Collège"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Early childhood education"] <- "Maternelle"
ewcs_2015_ren$nivx_etude[ewcs_2015_ren$nivx_etude == "Primary education"] <- "Primaire"
ewcs_2015_ren$nivx_etude <- factor(ewcs_2015_ren$nivx_etude)

## Recodage de la variable satisfaction
ewcs_2015_ren$satisfaction <- as.character(ewcs_2015_ren$satisfaction)
ewcs_2015_ren$satisfaction[ewcs_2015_ren$satisfaction == "Not at all satisfied"] <- "Pas_du_tout_satisfait"
ewcs_2015_ren$satisfaction[ewcs_2015_ren$satisfaction == "Not very satisfied"] <- "Pas_très_satisfait"
ewcs_2015_ren$satisfaction[ewcs_2015_ren$satisfaction == "Satisfied"] <- "Satisfait"
ewcs_2015_ren$satisfaction[ewcs_2015_ren$satisfaction == "Very satisfied"] <- "Très_satisfait"
ewcs_2015_ren$satisfaction <- factor(ewcs_2015_ren$satisfaction)


## Recodage de la variable type_contrat
ewcs_2015_ren$type_contrat <- as.character(ewcs_2015_ren$type_contrat)
ewcs_2015_ren$type_contrat[ewcs_2015_ren$type_contrat == ""] <- NA
ewcs_2015_ren$type_contrat[ewcs_2015_ren$type_contrat == "Full time"] <- "Temps_plein"
ewcs_2015_ren$type_contrat[ewcs_2015_ren$type_contrat == "Part time"] <- "Temps_partiel"
ewcs_2015_ren$type_contrat <- factor(ewcs_2015_ren$type_contrat)

##############################################################

## Recodage de la variable genre
ewcs_2015_ren$genre <- as.character(ewcs_2015_ren$genre)
ewcs_2015_ren$genre[ewcs_2015_ren$genre == "Female"] <- "Femme"
ewcs_2015_ren$genre[ewcs_2015_ren$genre == "Male"] <- "Homme"
ewcs_2015_ren$genre <- factor(ewcs_2015_ren$genre)


## Recodage de la variable situation
ewcs_2015_ren$situation <- as.character(ewcs_2015_ren$situation)
ewcs_2015_ren$situation[ewcs_2015_ren$situation == "Employee"] <- "Employé"
ewcs_2015_ren$situation[ewcs_2015_ren$situation == "Self-employed"] <- "Travailleur_Indép"
ewcs_2015_ren$situation <- factor(ewcs_2015_ren$situation)

## Decoupage des tranches d'age par quantile
ewcs_2015_ren$age_rec <- cut(ewcs_2015_ren$age,
                             include.lowest = FALSE,
                             right = FALSE,
                             dig.lab = 4,
                             breaks = c(18, 27, 37, 46, 56, 65,75,100))


## Recodage de la variable annee_experience en intervalle qui sera notée annee_experience_rec
ewcs_2015_ren$ancienneté_rec <- cut(ewcs_2015_ren$ancienneté,
                                    include.lowest = FALSE,
                                    right = FALSE,
                                    dig.lab = 4,
                                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45,50))


## Recodage de la variable secteur
ewcs_2015_ren$secteur_trav <- as.character(ewcs_2015_ren$secteur_trav)
ewcs_2015_ren$secteur_trav[ewcs_2015_ren$secteur_trav == "A joint private-public organisation or company"] <- "entre_mixte"
ewcs_2015_ren$secteur_trav[ewcs_2015_ren$secteur_trav == "Other, please specify:"] <- "Autre"
ewcs_2015_ren$secteur_trav[ewcs_2015_ren$secteur_trav == "The not-for-profit sector or an NGO"] <- "ONG"
ewcs_2015_ren$secteur_trav[ewcs_2015_ren$secteur_trav == "The private sector"] <- "sect_privé"
ewcs_2015_ren$secteur_trav[ewcs_2015_ren$secteur_trav == "The public sector"] <- "sect_public"
ewcs_2015_ren$secteur_trav <- factor(ewcs_2015_ren$secteur_trav)


## Recodage de la variable Profession
ewcs_2015_ren$profession <- as.character(ewcs_2015_ren$profession)
ewcs_2015_ren$profession[ewcs_2015_ren$profession == ""] <- NA
ewcs_2015_ren$profession <- factor(ewcs_2015_ren$profession)

## Recodage de ewcs_2015_fra$profession
ewcs_2015_ren$profession <- as.character(ewcs_2015_ren$profession)
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Armed forces occupations"] <- "Professions_des_forces_armées"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Clerical support workers"] <- "Employés_de_soutien_administratif"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Craft and related trades workers"] <- "Artisans_et_travailleurs_des_métiers_connexes"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Elementary occupations"] <- "Professions_élémentaires"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Plant and machine operators, and assemblers"] <- "Conducteurs_d_installations_et_de_machines_et_monteurs"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Professionals"] <- "Professionnels"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Service and sales workers"] <- "Personnel_de_service_et_de_vente"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Skilled agricultural, forestry and fishery workers"] <- "Travailleurs_qualifiés_de_l_agriculture_de_la_sylviculture_et_de_la_pêche"
ewcs_2015_ren$profession[ewcs_2015_ren$profession == "Technicians and associate professionals"] <- "Techniciens_et _professionnels_associés"
ewcs_2015_ren$profession <- factor(ewcs_2015_ren$profession)


## Suppression de toutes les valeurs NA
ewcs_2015_ren <- na.omit(ewcs_2015_ren)

#irec(ewcs_2015_ren, "profession")

## Recodage de ewcs_2015_ren$profession en ewcs_2015_ren$profession_rec
ewcs_2015_ren$profession_rec <- as.character(ewcs_2015_ren$profession)
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Artisans_et_travailleurs_des_métiers_connexes"] <- "A"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Conducteurs_d_installations_et_de_machines_et_monteurs"] <- "B"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Employés_de_soutien_administratif"] <- "C"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Managers"] <- "D"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Personnel_de_service_et_de_vente"] <- "E"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Professionnels"] <- "F"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Professions_des_forces_armées"] <- "G"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Professions_élémentaires"] <- "H"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Techniciens_et _professionnels_associés"] <- "I"
ewcs_2015_ren$profession_rec[ewcs_2015_ren$profession == "Travailleurs_qualifiés_de_l_agriculture_de_la_sylviculture_et_de_la_pêche"] <- "J"
ewcs_2015_ren$profession_rec <- factor(ewcs_2015_ren$profession_rec)


############# fin du pretraitement ###################
######################################################


###########################################
##***************************************##
###          Choix des deux pays        ###
##***************************************##
##*########################################
##*
##*## France premier pays
ewcs_2015_fra <- subset(ewcs_2015_ren, Pays == "France")

## L'Allemagne deuxième pays
ewcs_2015_ger <- subset(ewcs_2015_ren, Pays == "Germany")


################################################
##### Description de la base des deux pays #####
################################################

##### France #####
## Affichage d'une partie de la base
head(ewcs_2015_fra)
dim(ewcs_2015_fra) ## Taille de la base 

###### Summary de la base de donnée
summary(ewcs_2015_fra)

###### Statistique de la base de données ######
## Pour La france:

## Statt du salaire
summary(ewcs_2015_fra$salaire)

## salaire en fonction du genre
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre")], FUN = sd, na.rm = T)

## salaire en fonction du niveau d'étude
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","nivx_etude")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","nivx_etude")], FUN = sd, na.rm = T)

## salaire en fonction de la taille de l'entreprise
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","nivx_etude")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","nivx_etude")], FUN = sd, na.rm = T)

##  salaire en fonction de la profession
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","profession")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","profession")], FUN = sd, na.rm = T)

## salaire en fonction du type de contrat
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre", "type_contrat")], FUN = summary, na.rm = T)

## salaire en fonction du type de contrat
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("type_contrat")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","type_contrat")], FUN = sd, na.rm = T)

## salaire en fonction de la situation
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre", "situation")], FUN = summary, na.rm = T)

## salaire en fonction de la situation
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("situation")], FUN = summary, na.rm = T)


## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","situation")], FUN = sd, na.rm = T)

## salaire en fonction du secteur
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre", "secteur_trav")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","secteur_trav")], FUN = sd, na.rm = T)

## salaire en fonction du secteur
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre", "Taille_entreprise")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_fra[, c("salaire")], INDICES=ewcs_2015_fra[,c("genre","Taille_entreprise")], FUN = sd, na.rm = T)


##### Allemagne #####
## Affichage d'une partie de la base
head(ewcs_2015_ger)

## Affichage de la dimension de la base de données
dim(ewcs_2015_ger)

###### Summary de la base de donnée
summary(ewcs_2015_ger)

## Statt du salaire
summary(ewcs_2015_ger$salaire)

## salaire en fonction du genre
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre")], FUN = sd, na.rm = T)

## salaire en fonction du niveau d'étude
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","nivx_etude")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","nivx_etude")], FUN = sd, na.rm = T)

## salaire en fonction du type de contrat
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","type_contrat")], FUN = summary, na.rm = T)

## salaire en fonction du type de contrat
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("type_contrat")], FUN = summary, na.rm = T)


## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","type_contrat")], FUN = sd, na.rm = T)

## salaire en fonction de la profession
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","profession")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","profession")], FUN = sd, na.rm = T)

## l'Allemagne salaire en fonction de la taille de l'entreprise
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","secteur_trav")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","secteur_trav")], FUN = sd, na.rm = T)

## salaire en fonction du genre
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre", "situation")], FUN = summary, na.rm = T)

## salaire en fonction du genre
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("situation")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre", "situation")], FUN = sd, na.rm = T)


## l'Allemagne salaire en fonction de la taille de l'entreprise
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","Taille_entreprise")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","Taille_entreprise")], FUN = sd, na.rm = T)


## l'Allemagne salaire en fonction de la Profession
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","profession_rec")], FUN = summary, na.rm = T)

## ecart type
by(ewcs_2015_ger[, c("salaire")], INDICES=ewcs_2015_ger[,c("genre","profession_rec")], FUN = sd, na.rm = T)

######################################################################################
#### proportion des individus en fonction d'un certain nombres de variables###########
#### Proportion des individus ##################

#############################
## Repartition des individus
table(ewcs_2015_fra$genre)
table(ewcs_2015_ger$genre)

## Proportion en % des individus en fonction du salaire
table(ewcs_2015_fra$genre,ewcs_2015_fra$salaire)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$salaire), margin = 1),2)

## Proportion en % des individus en fonction du salaire
table(ewcs_2015_ger$genre,ewcs_2015_ger$salaire)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$salaire), margin = 1),2)


## Proportion en % des individus en fonction du type de contrat
table(ewcs_2015_fra$genre,ewcs_2015_fra$type_contrat)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$type_contrat), margin = 1),2)

## Proportion en % des individus en fonction du type de contrat
table(ewcs_2015_fra$genre,ewcs_2015_fra$type_contrat,ewcs_2015_fra$profession)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$type_contrat,ewcs_2015_fra$profession), margin = 1),2)


## Proportion en % des individus en fonction du type de contrat
table(ewcs_2015_ger$genre,ewcs_2015_ger$type_contrat)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$type_contrat), margin = 1),2)


### Proportion en % des individus en fonction du privée et du public
table(ewcs_2015_fra$genre,ewcs_2015_fra$secteur_trav)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$secteur_trav), margin = 1),2)

### Proportion en % des individus en fonction du privée et du public
table(ewcs_2015_ger$genre,ewcs_2015_ger$secteur_trav)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$secteur_trav), margin = 1),2)


## Proposition en % des individu en fonction des ages
table(ewcs_2015_fra$genre,ewcs_2015_fra$age_rec)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$age_rec), margin = 1),2)

## Proposition en % des individu en fonction des ages
table(ewcs_2015_ger$genre,ewcs_2015_ger$age_rec)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$age_rec), margin = 1),2)

## Proposition des individus en fonction de la profession
table(ewcs_2015_fra$genre,ewcs_2015_fra$profession_rec)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$profession_rec),margin = 1), 2)

## Proposition des individus en fonction de la profession
table(ewcs_2015_ger$genre,ewcs_2015_ger$profession_rec)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$profession_rec),margin = 1), 2)

## Proportion des individus en fonction du niveau d'étude
table(ewcs_2015_fra$genre,ewcs_2015_fra$nivx_etude)
round(100*prop.table(table(ewcs_2015_fra$genre,ewcs_2015_fra$nivx_etude),margin = 1), 2)

## Proposition des individus en fonction de la profession et du niveau d'étude
table(ewcs_2015_ger$genre,ewcs_2015_ger$nivx_etude)
round(100*prop.table(table(ewcs_2015_ger$genre,ewcs_2015_ger$nivx_etude),margin = 1), 2)

## Proportion des individu en fonction de la situation
tab4 = table(ewcs_2015_ger$genre, ewcs_2015_ger$situation)
round(100*prop.table(tab4, margin = 1), 2)

## Proportion des individu en fonction de la situation
tab4 = table(ewcs_2015_fra$genre, ewcs_2015_fra$situation)
round(100*prop.table(tab4, margin = 1), 2)

######## Proportion des individu en fonction de la satisfaction
t5 = table(ewcs_2015_fra$genre,ewcs_2015_fra$satisfaction)
round(100*prop.table(t5,margin=1),2)

######## Proportion des individu en fonction de la satisfaction
t5 = table(ewcs_2015_ger$genre,ewcs_2015_ger$satisfaction)
round(100*prop.table(t5,margin=1),2)

## Proposition des individu en fonction de l'ancienneté
t4=table(ewcs_2015_fra$genre,ewcs_2015_fra$ancienneté_rec)
round(100*prop.table(t4,margin=1),2)

## Proposition des individu en fonction de l'ancienneté
t4=table(ewcs_2015_ger$genre,ewcs_2015_ger$ancienneté_rec)
round(100*prop.table(t4,margin=1),2)


## Proposition des individu en fonction de la profession
t4=table(ewcs_2015_fra$genre,ewcs_2015_fra$profession_rec)
round(100*prop.table(t4,margin=1),2)

## Proposition des individu en fonction de la Profession
t4=table(ewcs_2015_ger$genre,ewcs_2015_ger$profession_rec)
round(100*prop.table(t4,margin=1),2)



#################################################
#####******   Datavisualisation  ******##########
#################################################

coul <- brewer.pal(8, "Set2")

######## Visualisation de la repartion du genre en fonction des variables

### Repartion des individus en genre (France)
barplot(table(ewcs_2015_fra$genre),col = coul, main = paste("Repartion des", nrow(ewcs_2015_fra), "individus en fonction du genre"),
        xlab = "genre", ylab = "Nombres d'individus" )

### Repartion des individus en genre (Allemagne)
barplot(table(ewcs_2015_ger$genre),col = coul, main = paste("Repartion des", nrow(ewcs_2015_ger), "individus en fonction du genre"),
        xlab = "genre", ylab = "Nombres d'individus" )

ggplot(data = ewcs_2015_ger)+aes(x = genre)+geom_bar(aes(fill = genre))+labs(ylab = "Nombres d'individus" )+ggtitle("Repartion des ages en fonction du nombre d'individu")


X11()
## Histogramme du salaire (France)
hist(ewcs_2015_fra$salaire,col = coul, main = "Répartition du salaire (France)",
     xlab = "Salaires en euro",las=2,labels = TRUE)

## Histogramme du salaire (Allemagne)
hist(ewcs_2015_ger$salaire,las=2,col = coul, main = "Repartition du salaire (Allemagne)",
     xlab = "Salaires en euro",labels = TRUE )

####### histogramme de la repartition des individus en fonction des ages (France)
ggplot(data = ewcs_2015_fra)+aes(x = age_rec)+geom_bar(aes(fill = genre))+
  ggtitle("Repartion des ages en fonction du nombre d'individu")

####### histogramme de la repartition des individus en fonction des ages (Allemagne)
ggplot(data = ewcs_2015_ger)+aes(x = age_rec)+geom_bar(aes(fill = age_rec))+ggtitle("Repartion des ages en fonction du nombre d'individu")

## Repartition des individus en fonction de la satisfaction (France)
plot(table(ewcs_2015_fra$satisfaction), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction de la satisfaction"),ylab = "Nombre d'individus" )

## Repartition des individus en fonction de la satisfaction (Allemagne)
plot(table(ewcs_2015_ger$satisfaction), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction de la satisfaction"),ylab = "Nombre d'individus")

## Repartition des individus en fonction du niveau d'étude (France)
barplot(height=table(ewcs_2015_fra$nivx_etude), col=coul,las=2 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction du diplome"),ylab = "Nombre d'individus", xlab = "nivx_étude", )

## Repartition des individus en fonction du niveau d'étude (Allemagne)
barplot(height=table(ewcs_2015_ger$nivx_etude), col=coul,las=2 ,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction du diplome"),ylab = "Nombre d'individus", xlab = "nivx_étude", )

## Repartition des individus en fonction du type de contrat (France)
barplot(height=table(ewcs_2015_fra$type_contrat), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction du type de contrat"),ylab = "Nombre d'individus", xlab = "Type contrat" )

## Repartition des individus en fonction du type de contrat (Allemagne)
barplot(table(ewcs_2015_ger$type_contrat), col=coul,las=1,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction du type de contrat"),ylab = "Nombre d'individus", xlab = "Type contrat" )

## Repartition des individus en fonction de la situation (France)
barplot(height=table(ewcs_2015_fra$situation), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction de leur situation"),ylab = "Nombre d'individus", xlab = "situation")

## Repartition des individus en fonction de la situation (Allemagne)
barplot(table(ewcs_2015_ger$situation), col=coul,las=1,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction de leur situation"),ylab = "Nombre d'individus", xlab = "situation")

## Repartition des individus en fonction du secteur (France)
barplot(height=table(ewcs_2015_fra$secteur_trav), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction du secteur"),ylab = "Nombre d'individus", xlab = "secteur" )

## Repartition des individus en fonction du secteur (Allemagne)
barplot(table(ewcs_2015_ger$secteur_trav), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction du secteur"),ylab = "Nombre d'individus", xlab = "secteur", )

## Repartition des individus en fonction de l'ancienneté (France)
barplot(height=table(ewcs_2015_fra$ancienneté_rec), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction de l'ancienneté"),ylab = "Nombre d'individus", xlab = "genre" )

## Repartition des individus en fonction de l'ancienneté (Allemagne)
barplot(table(ewcs_2015_ger$ancienneté_rec), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction de l'ancienneté"),ylab = "Nombre d'individus", xlab = "genre", )

## Repartition des individus en fonction de l'age (France)
barplot(height=table(ewcs_2015_fra$age_rec), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_fra), "individus en fonction de l'age"),ylab = "Nombre d'individus", xlab = "genre" )

## Repartition des individus en fonction de l'age (Allemagne)
barplot(table(ewcs_2015_ger$age_rec), col=coul,las=1 ,
        main = paste("Répartition des", nrow(ewcs_2015_ger), "individus en fonction de l'age"),ylab = "Nombre d'individus", xlab = "genre", )


################################################################
### Graphique en mosaiplot pour voir la repartion des du genre en fonction du niveau d'étude(France) 
mosaicplot(nivx_etude ~ genre, data = ewcs_2015_fra, col=c("green","cyan"),xlab="Niveaux d'étude",ylab="Genres", main ="Genres en fonction du niveau d'étude", las=2)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction du niveau d'étude(Allemagne)
mosaicplot(nivx_etude ~ genre, data = ewcs_2015_ger, col=c("green","cyan"),xlab="Niveaux d'étude",ylab="Genres", main ="Genres en fonction du niveau d'étude", las=2)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction du types de contrat(France)
mosaicplot(type_contrat ~ genre, data = ewcs_2015_fra, col=c("green","cyan"),xlab="Genres",ylab="Type de contrat", main ="Genre en fonction du contrat", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction du type de contrat(Allemagne)
mosaicplot(type_contrat ~ genre, data = ewcs_2015_ger, col=c("green","cyan"),xlab="Genres",ylab="Type de contrat",main ="Genre en fonction du contrat", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction du secteur(France)
mosaicplot(secteur_trav ~ genre, data = ewcs_2015_fra, col=c("green","cyan"),xlab="Genres",ylab="Secteur", main ="Genre en fonction du secteur", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction du secteur(Allemagne)
mosaicplot(secteur_trav ~ genre, data = ewcs_2015_ger, col=c("green","cyan"),xlab="Genres",ylab="Secteur", main ="Genre en fonction du secteur", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la satisfaction(France)
mosaicplot(profession_rec~satisfaction  , data = ewcs_2015_fra, col=c("green","cyan", "yellow","blue"), main ="Profession en fonction de la satisfaction", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la satisfaction(Allemagne)
mosaicplot(profession_rec~satisfaction, data = ewcs_2015_ger,col=c("green","cyan", "yellow","blue"), main ="Profession en fonction de la satisfaction", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la situation(France)
mosaicplot(situation ~ genre, data = ewcs_2015_fra, col=c("green","cyan"),xlab="Genres",ylab="Situation", main ="Genre en fonction de la situation", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la situation(Allemagne)
mosaicplot(situation ~ genre, data = ewcs_2015_ger, col=c("green","cyan"),xlab="Genres",ylab="Situation", main ="Genre en fonction de la situation", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la profession (France)
mosaicplot(profession_rec ~ genre, data = ewcs_2015_fra, col=c("green","cyan"),xlab="Profession",ylab="Situation", main ="Genre en fonction de la profession", las=1)

### Graphique en mosaiplot pour voir la repartion des du genre en fonction de la profession(Allemagne)
mosaicplot(profession_rec ~ genre, data = ewcs_2015_ger, col=c("green","cyan"),xlab="Professions",ylab="Situation", main ="Genre en fonction de la profession", las=1)


## Visualisation des salaire en fonction du genre
ggplot(ewcs_2015_fra, aes(x = genre, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartion du salaire en fonction du genre (France)") +theme_dark()

## (ALL) Visualisation des salaire en fonction du genre
ggplot(ewcs_2015_ger, aes(x = genre, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du genre (Allemagne)") +theme_dark()


## Visualisation des salaire en fonction de la situation et du genre
ggplot(ewcs_2015_fra, aes(x = situation, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de la  situation et du genre(France)") +theme_dark()

## (ALL) Visualisation des salaire en fonction de la situation et du genre
ggplot(ewcs_2015_ger, aes(x = situation, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de la  situation et du genre(Allemagne)") +theme_dark()


## Visualisation des salaire en fonction du contrat
ggplot(ewcs_2015_fra, aes(x = type_contrat, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du type de contrat (France)") +theme_dark()

## (ALL) Visualisation des salaire en fonction du contrat
ggplot(ewcs_2015_ger, aes(x = type_contrat, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du type de contrat (Allemagne)") +theme_dark()


## Visualisation des salaire en fonction du niveau d'études
ggplot(ewcs_2015_fra, aes(x = nivx_etude, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Green", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du niveau_d'étude (France)") +theme_dark()


## (ALL) Visualisation des salaire en fonction du contrat
ggplot(ewcs_2015_ger, aes(x = nivx_etude, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du niveau_d'étude (Allemagne)") +theme_dark()


## Visualisation des salaire en fonction du contrat
ggplot(ewcs_2015_fra, aes(x = Taille_entreprise, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Green", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de la taille_entre (France)") +theme_dark()


## (ALL) Visualisation des salaire en fonction du contrat
ggplot(ewcs_2015_ger, aes(x = Taille_entreprise, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fction de la taille_entre  (Allemagne)") +theme_dark()


## Visualisation des salaire en fonction de l'anciennété et le genre
ggplot(ewcs_2015_fra, aes(x = secteur_trav, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Green", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du secteur_trav (France)") +theme_dark()


## (ALL) Visualisation des salaire en fonction de l'anciennété et le genre
ggplot(ewcs_2015_ger, aes(x = secteur_trav, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction du secteur_trav (Allemagne)") +theme_dark()


## Visualisation des salaire en fonction de la satisfaction et le genre
ggplot(ewcs_2015_fra, aes(x = satisfaction, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Green", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de la satisfaction(France)") +theme_dark()


## (ALL) Visualisation des salaire en fonction de la satisfaction et le genre
ggplot(ewcs_2015_ger, aes(x = satisfaction, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de la satisfaction (Allemagne)") +theme_dark()

## Visualisation des salaire en fonction de la satisfaction et le genre
ggplot(ewcs_2015_fra, aes(x = ancienneté_rec, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="blue", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de l'ancienneté (France)") +theme_dark()

## (ALL) Visualisation des salaire en fonction de la satisfaction et le genre
ggplot(ewcs_2015_ger, aes(x = ancienneté_rec, y = salaire, fill = genre)) + 
  geom_boxplot(outlier.colour="Gold", outlier.shape=16, outlier.size=2)+ggtitle("Repartition du salaire en fonction de l'ancienneté (Allemagne)") +theme_dark()


###################################################"
################## Corrélation #####################
###################################################

#######" France #############

coul <- brewer.pal(8, "Set2")
### La France
m = ewcs_2015_fra[,c("age", "salaire", "ancienneté","heure_par_semaine",
                     "heures_mois", "taux_horaires")]
corr <- round(cor(m),1)
ggcorrplot(corr, p.mat = cor_pmat(ewcs_2015_fra[,c("age", "salaire", "ancienneté","heure_par_semaine","heures_mois", "taux_horaires")]), hc.order = TRUE, type = 
             "lower", color=c("#00AFBB","cyan","#FC4E07"), outline.col = "black", lab = TRUE)

############ Allemagne #############

m1 = ewcs_2015_ger[,c("age", "salaire", "ancienneté","heure_par_semaine",
                      "heures_mois", "taux_horaires")]
corr <- round(cor(m1),1)
ggcorrplot(corr, p.mat = cor_pmat(ewcs_2015_ger[,c("age", "salaire", "ancienneté","heure_par_semaine","heures_mois", "taux_horaires")]), hc.order = TRUE, type = 
             "lower", color=c("#00AFBB","cyan","#FC4E07"), outline.col = "black", lab = TRUE)


#####################################
#####################################
#### Analyse des données AFMD #######
#####################################
#####################################

###########   France ###############

coul <- brewer.pal(8, "Set2") 

data = ewcs_2015_fra[,c("genre","satisfaction","age","nivx_etude","profession_rec","situation",
                        "type_contrat", "secteur_trav", "Taille_entreprise",
                         "ancienneté","heure_par_semaine","heures_mois",
                        "salaire","taux_horaires")]

res.famd <- FAMD(data,ncp = 14,graph = FALSE)
round(head(get_eigenvalue(res.famd)),2)

## Visualiser les proportions de variances expliquées par les différents axes
fviz_screeplot(res.famd,main="Visualisation des proportions de variances  en fonction des dimensions (France)",
               ylab="Pourcentage des variances expliquées",addlabels=TRUE, hjust = -0.3)

# Graphique des variables
X11()
fviz_famd_var (res.famd, repel = TRUE)

# Contribution à la première dimension
fviz_contrib (res.famd, "var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.famd, "var", axes = 2)

#Pour extraire les résultats pour les variables quantitatives
quanti.var <- get_famd_var(res.famd, "quanti.var")

round(head(quanti.var$coord),2)
round(head(quanti.var$contrib),2)
round(head(quanti.var$cos2),2)
#round(head(quanti.var$cor),2)

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Couleur par valeurs cos2: qualité sur le plan des facteurs
X11()
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

#Comme les variables quantitatives, les résultats pour les variables qualitatives:
quali.var <- get_famd_var(res.famd, "quali.var")

head(quali.var$coord)
head(quali.var$contrib)
head(quali.var$cos2)
head(quali.var$cor)


# Contribution à la première dimension
fviz_contrib (res.famd, "quali.var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.famd, "quali.var", axes = 2)
#Pour visualiser les variables qualitatives
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

##Graphique des individus
## Pour obtenir les résultats pour les individus:
ind <- get_famd_ind(res.famd)

#Pour visualiser les individus,
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

## Notez qu'il est possible de colorer les individus en utilisant l'une des variables qualitatives dans le tableau de données initial.
fviz_mfa_ind(res.famd, 
            habillage = "genre", # color by groups 
            palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            addEllipses = TRUE, ellipse.type = "confidence", 
            repel = TRUE ) 

fviz_ellipses(res.famd, c("genre", "type_contrat"), repel = TRUE)


####################################
### Allemagne ###################"
###################################

df = ewcs_2015_ger[,c("genre","nivx_etude","situation","satisfaction","profession_rec","type_contrat", 
                        "secteur_trav", "Taille_entreprise",
                        "age", "ancienneté","heure_par_semaine","heures_mois",
                        "salaire","taux_horaires")]

res.famd <- FAMD(df,ncp = 14, graph = FALSE)

## La proportion de variances expliquées par les différentes dimensions (axes)
eig.val <- get_eigenvalue(res.famd)
round(head(eig.val),2)

## Visualiser les proportions de variances expliquées par les différents axes
fviz_screeplot(res.famd,main="Visualisation des proportions de variances en fonction des dimensions (Allemagne)",ylab="Pourcentage des variances expliquées",
               addlabels=TRUE, hjust = -0.3)

## taper la commande suivante pour eviter les erreurs
 x11()
# Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)

# Contribution à la première dimension
fviz_contrib (res.famd, "var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.famd, "var", axes = 2)

#Pour extraire les résultats pour les variables quantitatives
quanti.var <- get_famd_var(res.famd, "quanti.var")
round(head(quanti.var$coord),2)
round(head(quanti.var$contrib),2)
round(head(quanti.var$cos2),2)
round(head(quanti.var$cor),2)

# Pour visualiser les variables quantitatives
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Couleur par valeurs cos2: qualité sur le plan des facteurs
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

#Comme les variables quantitatives, les résultats pour les variables qualitatives:
quali.var <- get_famd_var(res.famd, "quali.var")

round(head(quali.var$coord),2)
round(head(quali.var$contrib),2)
round(head(quali.var$cos2),2)
round(head(quali.var$cor),2)

#Pour visualiser les variables qualitatives
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

##Graphique des individus
## Pour obtenir les résultats pour les individus:
ind <- get_famd_ind(res.famd)

#Pour visualiser les individus,
X11()
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

## Notez qu'il est possible de colorer les individus en utilisant l'une des variables qualitatives dans le tableau de données initial.
#X11()
fviz_mfa_ind(res.famd, 
            habillage = "genre", # color by groups 
            palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            addEllipses = TRUE, ellipse.type = "confidence", 
            repel = TRUE ) 

fviz_ellipses(res.famd, c("genre", "nivx_etude"), repel = TRUE)


##################################################################
#############################################################
#############  Regression par les MCO (France) ###########   
#############################################################

#############################################################
#####   Regression avec toutes les  #########################
##### variables de tous les individus  #####################
#############################################################

############# France ############
## Regression avec toutes les variables
model = lm(taux_horaires~age+situation+ secteur_trav
           +ancienneté+satisfaction+nivx_etude+
             profession_rec+heures_mois+Taille_entreprise
           , data=ewcs_2015_fra)
anova(model)
summary(model)


### Extration des individu en temps plein
data_temps_plein = filter(ewcs_2015_fra, type_contrat == "Temps_plein")

#############    France    ############
## Regression avec toutes les variables des individu en temps plein
model = lm(taux_horaires~age+situation+secteur_trav+Taille_entreprise
           +ancienneté+satisfaction+nivx_etude+profession_rec+heures_mois
           , data=data_temps_plein)
anova(model)
summary(model)


#####  modèle contraint pour chow avec toute les variables ######
model_c = lm(salaire~age+situation+secteur_trav
           +Taille_entreprise+ancienneté+satisfaction+nivx_etude+
             profession_rec+heures_mois+Taille_entreprise, data=ewcs_2015_fra)
anova(model_c)
summary(model_c)
SCRC = sum(model_c$residuals^2)


#### Objectif faire le test de CHOW
#####  modèle non contraint avec toutes les variables ######
########## Homme ###########

model_nc1 = lm(salaire~age+situation+ secteur_trav
            +ancienneté+satisfaction+nivx_etude+
              profession+heures_mois+Taille_entreprise
            , data=ewcs_2015_fra, subset=(genre == "Homme"))
anova(model_nc1)
summary(model_nc1)

SCR1 = sum(model_nc1$residuals^2)


########## Femme ###########
model_nc2 = lm(salaire~age+type_contrat+situation+ secteur_trav
            +ancienneté+satisfaction+nivx_etude+
              profession+heures_mois+Taille_entreprise
            , data=ewcs_2015_fra,subset=(genre == "Femme"))
anova(model_nc2)
summary(model_nc2)

SCR2 = sum(model_nc2$residuals^2)

SCRnc = SCR1 + SCR2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(ewcs_2015_fra)-(2*length(coefficients(model_c))))
nb_contr <- length(coefficients(model_c))

Ftest <- ((SCRC-(SCR1+SCR2))/nb_contr)/((SCR1+SCR2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
round(Chow_test,3)


#######Tests de normalité (test de jacque bera)
jb.norm.test(model_c$residuals)
hist(resid(model_c),prob=TRUE,col = coul,main = "Fonction de densité estimée",xlab = "Résidu estimé",ylab = "Densité")
lines(density(resid(model_c), na.rm = "TRUE"),lwd=3,col="cyan")
## Le 2
plot(model_c,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(model_c)^2~fitted(model_c)+I(fitted(model_c)^2), data=ewcs_2015_fra)
summary(white)

Ru2<- summary(white)$r.squared
LM <- nrow(ewcs_2015_fra)*Ru2
p.value <- 1-pchisq(LM, 2)
Test.het.White<-c(LM,p.value)
names(Test.het.White)<-c("Stat. LM", "p-value")
Test.het.White

summary(model_c)
library(xtable)          
names(model_c)   
a = resid(model_c)
residuals(model_c)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")


#########################################################
###### Regression par les MCO avec toutes variables #### 
######     des individus en temps plein            ######
#########################################################

### Extration des individu en temps plein
data_temps_plein = subset(ewcs_2015_fra, type_contrat == "Temps_plein")

#############    France    ############

## Regression avec toutes les variables des individu en temps plein
model = lm(salaire~genre+age+situation+secteur_trav+Taille_entreprise
           +ancienneté+satisfaction+nivx_etude+profession_rec+heures_mois
           , data=data_temps_plein)
anova(model)
summary(model)

#####  modèle contraint avec toute les variables ######
model_c = lm(salaire~genre+age+situation+ secteur_trav
             +ancienneté+satisfaction+nivx_etude+
               profession_rec+heures_mois+Taille_entreprise
             , data=data_temps_plein)
anova(model_c)
summary(model_c)

#####  modèle non contraint avec toutes les variables ######
########## Homme ###########

model_nc1 = lm(salaire~genre+age+type_contrat+situation+ secteur_trav
               +ancienneté+satisfaction+nivx_etude+
                 profession+heures_mois+taux_horaires+Taille_entreprise
               , data=data_temps_plein, subset=(genre == "Homme"))
anova(model_nc1)
summary(model_nc1)

SCR1 = sum(model_nc1$residuals^2)

########## Femme ###########
model_nc2 = lm(salaire~genre+age+type_contrat+situation+ secteur_trav
               +ancienneté+satisfaction+nivx_etude+profession+
                 heures_mois+taux_horaires+Taille_entreprise
               , data=data_temps_plein,subset=(genre == "Femme"))
anova(model_nc2)
summary(model_nc2)

SCR2 = sum(model_nc2$residuals^2)

SCRnc = SCR1 + SCR2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(data_temps_plein)-(2*length(coefficients(model_c))))
nb_contr <- length(coefficients(model_c))

Ftest <- ((SCRc-(SCR1+SCR2))/nb_contr)/((SCR1+SCR2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
Chow_test


#######Tests de normalité (test de jacque bera)
jb.norm.test(model_c$residuals)
shapiro.test(resid(model_c))

plot(model_c,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(model_c)^2~fitted(model_c)+I(fitted(model_c)^2), data=ewcs_2015_fra)
summary(white)

summary(model_c)
library(xtable)          
names(model_c)   
a = resid(model_c)
residuals(model_c)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")

hist(resid(model_c),prob=TRUE,col = "cornflowerblue",xlim =
       c(-19,19),main = "Fonction de densité estimée",xlab = "Résiduestimé",ylab = "Densité")


################################################################################
##############################################################
##############################################################
###### Regression avec l'équation de mincer pour la MCO ######
##############################################################
##############################################################


### Tous les contrat en utilisant le taux horaires
model = lm(log(taux_horaires)~nivx_etude+ancienneté+I(ancienneté^2), data=ewcs_2015_fra)
anova(model)
summary(model)


### Extration des individu en temps plein
data_temps_plein = filter(ewcs_2015_fra, type_contrat == "Temps_plein")

############ Temps plein ###########
model_t_plein = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), data=data_temps_plein)
anova(model_t_plein)
summary(model_t_plein)

#####  modèle contraint ######
modelc_t_plein = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), data=data_temps_plein)
anova(modelc_t_plein)
summary(modelc_t_plein)
SCRc = sum(modelc_t_plein$residuals^2)

#####  modèle non contraint ######
########## Homme ###########
modelnc_t_plein_1 = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), 
                  data=data_temps_plein,subset=(genre == "Homme"))
SCR_t_plein_1 = sum(modelnc_t_plein_1$residuals^2)

#####  Femme ######
modelnc_t_plein_2 = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), 
                       data=data_temps_plein,subset=(genre == "Femme"))
SCR_t_plein_2 = sum(modelnc_t_plein_2$residuals^2)
SCRnc = SCR_t_plein_1 + SCR_t_plein_2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(data_temps_plein)-(2*length(coefficients(modelc_t_plein))))
nb_contr <- length(coefficients(modelc_t_plein))

Ftest <- ((SCRc-(SCR_t_plein_1+SCR_t_plein_2))/nb_contr)/((SCR_t_plein_1+SCR_t_plein_2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
round(Chow_test, 3)


#######Tests de normalité (test de jacque bera)
jb.norm.test(modelc_t_plein$residuals)
shapiro.test(resid(modelc_t_plein))
plot(modelc_t_plein,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(modelc_t_plein)^2~fitted(modelc_t_plein)+I(fitted(modelc_t_plein)^2), data=data_temps_plein)
summary(white)

summary(modelc_t_plein)
library(xtable)          
names(modelc_t_plein)   
a = resid(modelc_t_plein)
residuals(modelc_t_plein)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")

hist(resid(modelc_t_plein),prob=TRUE,col = "cornflowerblue",xlim =
       c(-19,19),main = "Fonction de densité estimée",xlab = "Résiduestimé",ylab = "Densité")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#############################################################
#############  Regression par les MCO (Allemagne) ###########   
#############################################################

#############################################################
#####   Regression avec toutes les  #########################
##### variables  de tous les individus  #####################
#############################################################

model = lm(salaire~age+situation+ secteur_trav
           +ancienneté+satisfaction+nivx_etude+profession_rec+
             heures_mois+Taille_entreprise, data=ewcs_2015_ger)

anova(model)
summary(model)

#####  modèle contraint avec toute les variables ######
model_c = lm(salaire~age+situation+ secteur_trav
             +ancienneté+satisfaction+nivx_etude+
               profession+heures_mois+taux_horaires+Taille_entreprise
             , data=ewcs_2015_ger)
anova(model_c)
summary(model_c)
SCRc = sum(model_c$residuals^2)

#######Tests de normalité (test de jacque bera)
jb.norm.test(model_c$residuals)
hist(resid(model_c),prob=TRUE,col = coul,main = "Fonction de densité estimée",xlab = "Résidu estimé",ylab = "Densité")
lines(density(resid(model_c), na.rm = "TRUE"),lwd=3,col="cyan")

#shapiro.test(resid(model_c))

plot(model_c,which=1:6, col = coul)

#####  modèle non contraint avec toutes les variables ######
########## Homme ##########
model_nc1 = lm(salaire~age+type_contrat+situation+secteur_trav+Taille_entreprise+
               +ancienneté+satisfaction+nivx_etude+profession_rec+heures_mois
               ,data=ewcs_2015_ger, subset=(genre == "Homme"))
anova(model_nc1)
summary(model_nc1)
SCR1 = sum(model_nc1$residuals^2)


########## Femme ###########
model_nc2 = lm(salaire~age+type_contrat+situation+ secteur_trav
               +ancienneté+satisfaction+nivx_etude+
                 profession+heures_mois+Taille_entreprise
               , data=ewcs_2015_ger,subset=(genre == "Femme"))
anova(model_nc2)
summary(model_nc2)

SCR2 = sum(model_nc2$residuals^2)
SCRnc = SCR1 + SCR2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(ewcs_2015_ger)-(2*length(coefficients(model_c))))
nb_contr <- length(coefficients(model_c))

Ftest <- ((SCRc-(SCR1+SCR2))/nb_contr)/((SCR1+SCR2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
round(Chow_test,3)


#######Tests de normalité (test de jacque bera)
jb.norm.test(model_c$residuals)
shapiro.test(resid(model_c))
hist(resid(model_c),prob=TRUE,col = coul,main = "Fonction de densité estimée",xlab = "Résidu estimé",ylab = "Densité")
lines(density(resid(model_c), na.rm = "TRUE"),lwd=3,col="cyan")



plot(model_c,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(model_c)^2~fitted(model_c)+I(fitted(model_c)^2), data=ewcs_2015_fra)
summary(white)

summary(white)

Ru2<- summary(white)$r.squared
LM <- nrow(ewcs_2015_fra)*Ru2
p.value <- 1-pchisq(LM, 2)
Test.het.White<-c(LM,p.value)
names(Test.het.White)<-c("Stat. LM", "p-value")
round(Test.het.White,3)


summary(model_c)
library(xtable)          
names(model_c)   
a = resid(model_c)
residuals(model_c)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")

hist(resid(model_c),prob=TRUE,col = "cornflowerblue",xlim =
       c(-19,19),main = "Fonction de densité estimée",xlab = "Résiduestimé",ylab = "Densité")


#########################################################
###### Regression par les MCO avec toutes variables #### 
######     des individus en temps plein            ######
#########################################################

### Extration des individu en temps plein
data_temps_plein = filter(ewcs_2015_ger, type_contrat == "Temps_plein")

## Regression avec toutes les variables des individu en temps plein
model = lm(taux_horaires~genre+age+situation+ secteur_trav
           +ancienneté+satisfaction+nivx_etude+
             profession_rec+heures_mois
           , data=data_temps_plein)
anova(model)
summary(model)

#####  modèle contraint avec toute les variables ######
model_c = lm(salaire~genre+age+type_contrat+situation+ secteur_trav
             +ancienneté+heure_par_semaine+satisfaction+nivx_etude+
               profession+heures_mois+taux_horaires
             , data=data_temps_plein)
anova(model_c)
summary(model_c)


#####  modèle non contraint avec toutes les variables ######
########## Homme ###########

model_nc1 = lm(salaire~age+type_contrat+situation+ secteur_trav
               +ancienneté+heure_par_semaine+satisfaction+nivx_etude+
                 profession+heures_mois+taux_horaires
               , data=data_temps_plein, subset=(genre == "Homme"))
anova(model_nc1)
summary(model_nc1)

SCR1 = sum(model_nc1$residuals^2)

########## Femme ###########
model_nc2 = lm(salaire~age+type_contrat+situation+ secteur_trav
               +ancienneté+Taille_entreprise+satisfaction+nivx_etude+
                 profession+heures_mois+taux_horaires
               , data=data_temps_plein,subset=(genre == "Femme"))
anova(model_nc2)
summary(model_nc2)

SCR2 = sum(model_nc2$residuals^2)

SCRnc = SCR1 + SCR2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(data_temps_plein)-(2*length(coefficients(model_c))))
nb_contr <- length(coefficients(model_c))

Ftest <- ((SCRc-(SCR1+SCR2))/nb_contr)/((SCR1+SCR2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
Chow_test


#######Tests de normalité (test de jacque bera)
jb.norm.test(model_c$residuals)
shapiro.test(resid(model_c))

plot(model_c,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(model_c)^2~fitted(model_c)+I(fitted(model_c)^2), data=ewcs_2015_fra)
summary(white)

summary(model_c)
library(xtable)          
names(model_c)   
a = resid(model_c)
residuals(model_c)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")

hist(resid(model_c),prob=TRUE,col = "cornflowerblue",xlim =
       c(-19,19),main = "Fonction de densité estimée",xlab = "Résiduestimé",ylab = "Densité")


##############################################################
###### Regression avec l'équation de mincer pour la MCO ######
##############################################################

############ Les individus avec tous les contrat ###########
model = lm(log(taux_horaires)~nivx_etude+ancienneté+I(ancienneté^2), data=ewcs_2015_ger)
anova(model)
summary(model)


#####  modèle contraint ######
modelc = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), data=ewcs_2015_ger)
anova(modelc)
summary(modelc)
SCRc = sum(modelc$residuals^2)


#####  modèle non contraint ######
########## Homme ###########
modelnc_1 = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), 
                         data=ewcs_2015_ger,subset=(genre == "Homme"))
SCR_1 = sum(modelnc_1$residuals^2)

#####  Femme ######
modelnc_2 = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), 
                         data=ewcs_2015_ger,subset=(genre == "Femme"))
SCR_2 = sum(modelnc_t_partiel_2$residuals^2)

SCRnc = SCR_1 + SCR_2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(ewcs_2015_ger)-(2*length(coefficients(modelc))))
nb_contr <- length(coefficients(modelc))

Ftest <- ((SCRc-(SCR_1+SCR_2))/nb_contr)/((SCR_1+SCR_2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
Chow_test

#######Tests de normalité (test de jacque bera)
jb.norm.test(modelc$residuals)
shapiro.test(resid(modelc))

plot(modelc,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(modelc)^2~fitted(modelc)+
              I(fitted(modelc)^2), data=ewcs_2015_ger)
summary(white)

summary(modelc)
library(xtable)          
names(modelc)   
a = resid(modelc)
residuals(modelc)
plot(x=a, main = "résidus du modèle", 
     pch = 5, lwd = 3, col="red")

hist(resid(modelc),prob=TRUE,
     col = "cornflowerblue",xlim =c(-19,19),
     main = "Fonction de densité estimée",
     xlab = "Résiduestimé",ylab = "Densité")


### Extration des individu en temps plein
data_temps_plein = filter(ewcs_2015_ger, type_contrat == "Temps_plein")
model= lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), data=data_temps_plein)
anova(model)
summary(model)

############ Les individus en temps plein ###########
#####  modèle contraint ######
modelc_t_plein = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), data=data_temps_plein)

anova(modelc_t_plein)
summary(modelc_t_plein)
SCRc = sum(modelc_t_plein$residuals^2)

#####  modèle non contraint ######
########## Homme ###########
modelnc_t_plein_1 = lm(log(taux_horaires)~nivx_etude+ancienneté+I(ancienneté^2), 
                       data=data_temps_plein,subset=(genre == "Homme"))
SCR_t_plein_1 = sum(modelnc_t_plein_1$residuals^2)

#####  Femme ######
modelnc_t_plein_2 = lm(log(salaire)~nivx_etude+ancienneté+I(ancienneté^2), 
                       data=data_temps_plein,subset=(genre == "Femme"))

SCR_t_plein_2 = sum(modelnc_t_plein_2$residuals^2)
SCRnc = SCR_t_plein_1 + SCR_t_plein_2

#### calcul de la statistique du test de Chow
dlnc <- (nrow(data_temps_plein)-(2*length(coefficients(modelc_t_plein))))
nb_contr <- length(coefficients(modelc_t_plein))

Ftest <- ((SCRc-(SCR_t_plein_1+SCR_t_plein_2))/nb_contr)/((SCR_t_plein_1+SCR_t_plein_2)/dlnc)
pval <- 1-pf(Ftest,nb_contr,dlnc)
Chow_test <- c(Ftest,pval)
names(Chow_test) <- c("Stat F du test de Chow", "p-value")
Chow_test


#######Tests de normalité (test de jacque bera)
jb.norm.test(modelc_t_plein$residuals)
shapiro.test(resid(modelc_t_plein))


plot(modelc_t_plein,which=1:6, col = coul)

################## Test d'hétéroscédasticité Femme ##################
white <- lm(resid(modelc_t_plein)^2~fitted(modelc_t_plein)+I(fitted(modelc_t_plein)^2), data=data_temps_plein)
summary(white)

summary(modelc_t_plein)
library(xtable)          
names(modelc_t_plein)   
a = resid(modelc_t_plein)
residuals(modelc_t_plein)
plot(x=a, main = "résidus du modèle", pch = 5, lwd = 3, col="red")

hist(resid(modelc_t_plein),prob=TRUE,col = "cornflowerblue",xlim =
       c(-19,19),main = "Fonction de densité estimée",xlab = "Résiduestimé",ylab = "Densité")

##################### Fin du code ########################################

