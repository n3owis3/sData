# https://datasciencebeginners.com/2018/11/26/functions-and-packages-for-feature-selection-in-r/
  

# DESCRIPTION: Feature Selection 
#              Zisťovanie prínosu jednotlivych features (prípadná redukcia)

#              0:  CIELENE na  - ECOICOP 5
#                              - ECOICOP 6

#              1:  Kody  - zakladne
#                        - zozlozene jednotlivo
#                        - rozlozene vo dvojiciach
#                        - všetko spolu

#              2:  
#              3:  - 
#                  - 


# Načítanie R balíkov ---------------------------------------------------

# install.packages("excel.link") # manuálna inštalácia
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(stringr)
library(exploratory)
library(caret)


setwd("C:/Users/Veronika Soldanova/Sync/i/kaufland")


# # START -----------------------------------------------------------------

# Načítanie vlastných funkcií:
source("../f_scannerData.R", encoding = "utf-8")

# Načítavanie údajov po feature engineeringu
load("2_featureEng_kaufland.RData")
# OBSAH: DF - kompletny df s vycistenym textom
#        num - extrahovane informacie o mernych jednotkach tovaru
#        kody - rozložené kódy - po jednom
#        kody2 - rozložené kódy - po dvojiciach


# POZOR MAZAT EAN - už je zmazany


# 1. Kódy  ----------------------------------------------------------------

# * najprv kody
# * potom kody2
# * potom spolu


# Varianty zlozenia df s kodmi --------------------------------------------


# ZAKLADNE ----------------------------------------------------------------

kody.df <- kody[1:4]

# doplnenie cielovej triedy
kody.df$ecoicop <- df$ECOICOP5   # 5
kody.df$ecoicop <- df$ECOICOP6   # 6

# konvertovanie tried z char na factor
kody.df <- as.data.frame(unclass(kody.df)) 

# Trvanie:
# After 9 iterations, +12 mins: ec5
# After 9 iterations, +1.5 hours: ec6



# 1 - BEZ POVODNYCH KODOV -----------------------------------------------

kody.df <- kody 
kody.df <- kody2 

# Premenujem kody 1 a 2 aby bolo zrejme, ktory stlpec pochadza odkial
colnames(kody.df) <- paste0("c1_", colnames(kody.df) )
colnames(kody.df) <- paste0("c2_", colnames(kody.df) )

# Odstranenie povodnych kodov
kody.df <- kody.df[5:ncol(kody.df)]

# doplnenie cielovej triedy
kody.df$ecoicop <- df$ECOICOP5   # 5
kody.df$ecoicop <- df$ECOICOP6   # 6

# konvertovanie tried z char na factor
kody.df <- as.data.frame(unclass(kody.df)) 

# kody1
# After 12 iterations, +2.7 mins - EC5 
# After 12 iterations, +6.5 mins: - EC6

# kody2
# After 11 iterations, +2.2 mins:  - EC5 
# After 11 iterations, +8.4 mins:  - EC6



# 1 + 2 SPOLU -----------------------------------------------


# Premenujem kody 1 a 2 aby bolo zrejme, ktory stlpec pochadza odkial
colnames(kody) <- paste0("c1_", colnames(kody) )
colnames(kody2) <- paste0("c2_", colnames(kody2) )

kody.df <- cbind(kody[5:ncol(kody)], kody2[5:ncol(kody2)])

# doplnenie cielovej triedy
kody.df$ecoicop <- df$ECOICOP5   # 5
kody.df$ecoicop <- df$ECOICOP6   # 6

# konvertovanie tried z char na factor
kody.df <- as.data.frame(unclass(kody.df)) 


# After 13 iterations, +3.9 mins: EC5
# After 13 iterations, +11 mins: 



#  SPOLU -----------------------------------------------


# Premenujem kody 1 a 2 aby bolo zrejme, ktory stlpec pochadza odkial
colnames(kody) <- paste0("c1_", colnames(kody) )
colnames(kody2) <- paste0("c2_", colnames(kody2) )

kody.df <- cbind(kody, kody2[5:ncol(kody2)])

# doplnenie cielovej triedy
kody.df$ecoicop <- df$ECOICOP5   # 5
kody.df$ecoicop <- df$ECOICOP6   # 6

# konvertovanie tried z char na factor
kody.df <- as.data.frame(unclass(kody.df)) 

# After 13 iterations, +8 mins: ec5






# * BORUTA 
# https://www.machinelearningplus.com/machine-learning/feature-selection/
  
# Boruta je algoritmus na hodnotenie a výber funkcií založený na algoritme náhodných lesov. Výhodou Boruty je,
# že jasne rozhoduje, či je nejaká premenná dôležitá alebo nie, a pomáha pri výbere premenných, ktoré sú štatisticky významné.
# Okrem toho môžete upraviť prísnosť algoritmu úpravou hodnôt p, ktoré sú predvolené na 0,01 a maxRuns.
# maxRuns je počet spustení algoritmu. Čím vyššia je maximálna rýchlosť, tým selektívnejší je výber premenných.
# Predvolená hodnota je 100. V procese rozhodovania, či je prvok dôležitý alebo nie, môže Boruta niektoré vlastnosti označiť ako „predbežný“.
# Niekedyzvýšenie maxRunov môže pomôcť vyriešiť „citlivosť“ funkcie.


# V R. existuje veľa balíkov na výber funkcií.
# Vynára sa otázka „Čo robí balík boruta tak zvláštnym“.
# Pozrite si nasledujúce dôvody použitia balíka boruta na výber funkcií.
# Funguje dobre pre problém klasifikácie aj regresie. Zohľadňujú sa v ňom
# vzťahy s rôznymi premennými. Je to zlepšenie náhodného merania dôležitosti
# lesnej premeny, čo je veľmi populárny spôsob výberu premennej.
# Dodržiava sa úplne relevantná metóda výberu premennej, 
# pri ktorej sa zvažujú všetky vlastnosti, ktoré sú relevantné pre výslednú premennú.
# Zatiaľ čo väčšina ostatných algoritmov s variabilným výberom sa riadi metódou minimálneho optima,
# keď sa spolieha na malú podmnožinu funkcií, ktorá vyvoláva minimálnu chybu vybraného klasifikátora.
# Dokáže zvládnuť interakcie medzi premennými.
# Môže zvládnuť kolísavý charakter náhodného náhodného merania dôležitosti lesa

# * BORUTA ------------------------------------------------------------------

# 
summary(kody.df)
sum(is.na(kody.df))

# install.packages('Boruta')
library(Boruta)

# Spustenie algoritmu Boruta
boruta_output <- Boruta(ecoicop ~ ., data=na.omit(kody.df), doTrace=1)
# ..........................

# obsah outputu
names(boruta_output)

# boruta_output$pValue
# boruta_output$maxRuns
# boruta_output$light
# boruta_output$mcAdj
# boruta_output$timeTaken
# boruta_output$roughfixed
# boruta_output$call
# boruta_output$impSource
  
# Vypisanie dolezitych premennych so zahrnutim tzv. nerozhodnutých
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

# Fix nerozhodnutych premennych
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Skore dolezitosti jednotlivych vstupov - kodov
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # vzostupne poradie

# Plot dolezitosti premennych
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Kody 1 + 2 + zakladne - ECOICOP5")  

# getConfirmedFormula(boruta_output)

# !!!!!!! PREPISUJE SA NAZOV !!!!!!!
write.table(imps, "./featureSelection/imps_kody_1plus2plusZ_ec5.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# 6x5
# 7.5 x 5.5



# Zoznam parametrov použitých v Boruta
# maxRuns:
#   maximálny počet náhodných lesných cyklov. Predvolená hodnota je 100.
# doTrace:
#   Vzťahuje sa na úroveň výrečnosti.
# 0 znamená žiadne sledovanie.
# 1 znamená nahlasovanie rozhodnutia o atribúte ihneď po jeho zúčtovaní.
# 2 znamená všetkých 1 plus hlásenie každej iterácie. Predvolená hodnota je 0.
# getImp:
#   funkcia použitá na získanie dôležitosti atribútu.
# Predvolená hodnota je getImpRfZ, ktorá spúšťa náhodnú doménovú štruktúru
# z balíka Ranger a zhromažďuje Z-skóre mierky priemernej redukcie presnosti.
# holdHistory:
#   Celá história dôležitých cyklov sa uloží, ak je nastavená na hodnotu TRUE (Predvolené).






# Variable Importance from Machine Learning Algorithms --------------------

# Train an rpart model and compute variable importance.
library(caret)
set.seed(100)
rPartMod <- train(res ~ ., data=na.omit(kody2), method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)


# Train an RRF model and compute variable importance.
set.seed(100)
rrfMod <- train(res ~ ., data=na.omit(kody2), method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp


plot(rrfImp, top = 20, main='Variable Importance')

# Some of the other algorithms available in train() that you can use to compute varImp are the following:
#   
# ada, AdaBag, AdaBoost.M1, adaboost, bagEarth, bagEarthGCV, bagFDA, bagFDAGCV,
# bartMachine, blasso, BstLm, bstSm, C5.0, C5.0Cost, C5.0Rules, C5.0Tree, cforest,
# chaid, ctree, ctree2, cubist, deepboost, earth, enet, evtree, extraTrees, fda, gamboost,
# gbm_h2o, gbm, gcvEarth, glmnet_h2o, glmnet, glmStepAIC, J48, JRip, lars, lars2,
# lasso, LMT, LogitBoost, M5, M5Rules, msaenet, nodeHarvest, OneR, ordinalNet,
# ORFlog, ORFpls, ORFridge, ORFsvm, pam, parRF, PART, penalized, PenalizedLDA,
# qrf, ranger, Rborist, relaxo, rf, rFerns, rfRules, rotationForest, rotationForestCp,
# rpart, rpart1SE, rpart2, rpartCost, rpartScore, rqlasso,
# rqnc, RRF, RRFglobal, sdwd, smda, sparseLDA, spikeslab, wsrf, xgbLinear, xgbTree.



# 6. Recursive Feature Elimination (RFE) ----------------------------------


set.seed(100)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, c(1:3, 5:13)], y=trainData$ozone_reading,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile






# FILTRACNE METODY:
 - korelacia
 - testovanie hypotez 
 - USING INFORMATION GAIN FOR VARIABLE SELECTION:

  
library(FSelector)
nformation.gain(Species~., iris)




