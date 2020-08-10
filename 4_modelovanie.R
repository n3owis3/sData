
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


# Premenujem kody 1 a 2 aby bolo zrejme, ktory stlpec pochadza odkial
colnames(kody) <- paste0("c1_", colnames(kody) )
colnames(kody2) <- paste0("c2_", colnames(kody2) )

kody.df <- cbind(kody, kody2[5:ncol(kody2)])

# doplnenie cielovej triedy
kody.df$ecoicop <- df$ECOICOP5   # 5
kody.df$ecoicop <- df$ECOICOP6   # 6

# konvertovanie tried z char na factor
kody.df <- as.data.frame(unclass(kody.df)) 



dataset <- kody.df

# Pridam cielovu premennu
dataset$ecoicop <- df$ECOICOP6
# Konvertujem na faktor
dataset <- as.data.frame(unclass(dataset)) 

# Odstranim zbytocne stlpce
dataset$ID_PRODUKT <- NULL
dataset$EAN <- NULL


set.seed(3456)

# Rozdelenie na train test data
trainIndex <- createDataPartition(dataset$ecoicop, list=FALSE, p = 0.8)
trainData <- dataset[trainIndex,]
testData <- dataset[-trainIndex,]

trainData  %>% count(ecoicop)
testData  %>% count(ecoicop)


# A tibble: 61 x 2
# ecoicop      n
# <fct>    <int>
# 
# 10 01.1.2.2    88
# # ... with 51 more rows
# > testData  %>% count(ecoicop)
# # A tibble: 59 x 2




# Ranger ------------------------------------------------------------------
library(caret)
metoda <- "A4"

library(parallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores); registerDoParallel(cl)
ss <- Sys.time()
set.seed(10)  # pre srandu - aj ked zmenis set.seed mozes dostat ine vysledky

fitControl = trainControl(
  method = 'repeatedcv',number = 1,repeats = 1,
  allowParallel = TRUE)

s1 <- Sys.time()
timestamp()


kody$id1 <- NULL
kody$ID_PRODUKT <- NULL
kody$EAN <- NULL

sum(is.na(kody))

model <- train(
  ecoicop ~ (.), #^2
  data       = kody,
  method     = 'ranger',
  trControl  = fitControl,
  # num.trees = 63,
  # mtry = 20,
  # tuneGrid = tgrid,
  # importance = 'impurity' # ?
)


# model <- train(Qmm_p2~(.)^2, data = trainData[,-1], method="ranger",tuneLength=2)
Sys.time() - s1

stopCluster(cl)


plot(model)    # takto prebiehalo jeho ladenie        # uložiť 
# best tune
model$bestTune    # toto je najlepšia hodnota parametrov       # uložiť






# 8. AutoML --------------------------------------------------------------
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

# kody1
# 1.016954 hours -  3 modely
# Time difference of 1.636219 hours  - 10 modelov

library(h2o)
h2o.shutdown(prompt = FALSE)

# library(parallel)
# no_cores <- detectCores() - 1
# library(doParallel)

metoda = "A8" # model 1 

# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores); registerDoParallel(cl)
ss <- Sys.time()

# Treba mat nainstalovanu javu (to asi je) a nastavit na nu cestu takto:
# Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/64')
# h2o.init( nthreads = no_cores, max_mem_size = "4G" )

#   Initializing H2O Cluster
h2o.init()


write.table(trainData,'trainH2o.txt', quote=FALSE, row.names = FALSE)
write.table(testData,"testH2o1.txt", quote=FALSE, row.names = FALSE)

train_hex <- h2o.importFile('trainH2o.txt')
test_hex <- h2o.importFile("testH2o1.txt")

train=as.data.frame(train_hex)
test=as.data.frame(test_hex)


s1 <- Sys.time()
timestamp()


# aml ---------------------------------------------------------------------
aml <- h2o.automl(y = "ecoicop", training_frame = train_hex,
                  # max_runtime_secs = 10,
                  max_models = 10,
                  # exclude_algos = "DeepLearning",
                  stopping_rounds = 7, seed = 316
)

Sys.time() - s1



# aml@leaderboard
# h2o.get_leaderboard(aml, extra_columns = "ALL")

  prediction <- h2o.predict(aml, test_hex) # predict
  prediction <- as.data.frame(prediction)
  
  
  confusionMatrix(
    factor(prediction$predict),
    factor(testData$ecoicop)
  )
  
  confusionMatrix(
    prediction$predict,
    testData$ecoicop
  )
  
  # confusionMatrix(
  #   factor(prediction$predict, levels = 1:148),
  #   factor(testData$ecoicop, levels = 1:148)
  # )
  
  
  lb <- aml@leaderboard
  lb
  
  aml@leader
  
  
  
  
  # View the AutoML Leaderboard
  lb <- aml@leaderboard
  print(lb, n = nrow(lb))
  
  
  
  # Get model ids for all models in the AutoML Leaderboard
  model_ids <- as.data.frame(lb$model_id)[,1]
  
  # View variable importance for all the models (besides Stacked Ensemble)
  for (model_id in model_ids) {
    print(model_id)
    m <- h2o.getModel(model_id)
    h2o.varimp(m)
    h2o.varimp_plot(m)
  }
  
  
  
  library(h2o)
  library(lime)
  
  ## Train explainer
  explainer <- lime(trainData, aml)
  
  ## Get explanations for a subset of samples
  explanation <- explain(trainData, explainer, n_features = 10)
  
  ## Plot global explanations
  plot_explanations(explanation)
  
  ## Plot local explanations
  plot_features(explanation)
  
  
  