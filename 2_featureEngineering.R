 
# DESCRIPTION: Feature engineering 
#              Modifikácie údajov s cielom zlepsenia predikcie Ecoicop
#              1: Regularne vyrazy 
#              2: Čistenie textu 
#              3: Rozklad kodov - po jednom cisle
#                               - po dvojiciach


# Načítanie R balíkov ---------------------------------------------------

# install.packages("excel.link") # manuálna inštalácia
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(exploratory)
  
  

# Načítanie vlastných funkcií:
source("../f_scannerData.R", encoding = "utf-8")

# Načítavanie údajov po analyze
load("analyzovane_kaufland.RData")

  

# ************************************************************************
# 1. REGEX  ------------------------------------------------------------------
# ************************************************************************

# Stlpec PRODUKT_POPIS obsahuje viacero vyuzitelnych ciselnych informacii,
# ktore mozno pouzit ako doplnok k stlpcu OBSAH a MERNA_JEDNOTKA.


# EXTRAHOVANIE NUMERICKYCH INFORMACII suvisiacich s mernymi jednotkami: ***
num <- df["PRODUKT_POPIS"]

# nahlad struktury
str(num$PRODUKT_POPIS)

# pri jednotkach nie je mozne pouzit rovnaky regex
num$g <-
  str_extract_all(
    num$PRODUKT_POPIS,
    "\\d+(\\.\\d+)?[\\. ]?g|\\d+(\\.\\d+)?[x]?\\d+(\\.\\d+)?[\\.\\s]?g")
num$kg <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?kg")
num$perc <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?%")
num$l <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?l")
num$ml <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?ml")
num$ks <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?ks") # |(ks)|kusy
num$pl <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?pl")
num$m <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?m")
num$pet <-
  str_extract_all(num$PRODUKT_POPIS, "\\d+(\\.\\d+)?[\\.\\s]?pet")


# KONTROLA č.1 - kontrola unikatnych jednotiek, hladanie chyb.
# Chybou je, ak sa tam vyskytne ina jednotka alebo chybny znak.
# V prípade potreby prisposobenie reg. vyrazu. 
kontrola <- unique(num$g)
kontrola <- unique(num$kg)
kontrola <- unique(num$perc)
kontrola <- unique(num$l)
kontrola <- unique(num$ml)
kontrola <- unique(num$ks)
kontrola <- unique(num$pl)
kontrola <- unique(num$m)
kontrola <- unique(num$pet)


# Vzniknute hodnoty "character(0)" su v tomto kroku nahradene s nulou:
num[c("g", "kg", "perc", "l", "ml", "ks", "pl", "m", "pet")] <-
  lapply(num[c("g", "kg", "perc", "l", "ml", "ks", "pl", "m", "pet")], function(x)
    ifelse(x == "character(0)", "0", x))
# Mazanie medzier medzi cislom a jednotkou:
num[c("g", "kg", "perc", "l", "ml", "ks", "pl", "m", "pet")] <-
  lapply(num[c("g", "kg", "perc", "l", "ml", "ks", "pl", "m", "pet")], function(x)
    gsub(" ", "", x))

# KONTROLA č.2 - kontrola riadkov bez jednotky (suctovou metodou)

# ! Ak sa zo stlpca nepodarilo extrahovat ziadnu jednotku,
# ! bude mat v stlpci NA priradenu hodnotu TRUE
# ! a v dalsom kroku sa vyfiltruje do df "nuly"
num$na <- NA

for (i in (1:nrow(num))) {
  NAsum <- sum(num[i, 2:10] == "0")
  if (NAsum == 9)
    num$na[i] <- TRUE
}

# DF nuly
# Tu možno posudit vyuzitelnych ciselnych informacii v stlpci PRODUKT_POPIS,
# pripadne prisposobit regex.
nuly <- num %>%
  filter(na == TRUE)
# pocet riadkov 
nrow(nuly)
# mazanie nepotrebneho stlpca
num$na <- NULL

table(num$g)
table(num$kg)
table(num$perc)
table(num$l)
table(num$ml)
table(num$ks)
table(num$pl)
table(num$m)
table(num$pet)

# V niektorých prípadoch sa v bunke nachádza viac ako 1 číslo,
# preto sa extrahujú dp zvlášť bunky
num <- separate(num, "g", c("g1", "g2"), sep = ",", remove = TRUE)
num <- separate(num, "kg", c("kg1", "kg2"), sep = ",", remove = TRUE)
num <- separate(num, "perc", c("perc1", "perc2"), sep = ",", remove = TRUE)
num <- separate(num, "l", c("l1", "l2"), sep = ",", remove = TRUE)
num <- separate(num, "ml", c("ml1", "ml2"), sep = ",", remove = TRUE)
num <- separate(num, "ks", c("ks1", "ks2"), sep = ",", remove = TRUE)
num <- separate(num, "pl", c("pl1", "pl2"), sep = ",", remove = TRUE)
num <- separate(num, "m", c("m1", "m2"), sep = ",", remove = TRUE)
num <- separate(num, "pet", c("pet1", "pet2"), sep = ",", remove = TRUE)

table(num$g1)
table(num$g2)
table(num$kg1)
table(num$kg2)
table(num$perc1)
table(num$perc2)
table(num$l1)
table(num$l2)
table(num$ml1)
table(num$ml2)
table(num$ks1)
table(num$ks2)
table(num$pl1)
table(num$pl2)
table(num$m1)
table(num$m2)
table(num$pet1)
table(num$pet2)

# V niektorych prípadoch vznikli prázdne stĺpce, tie treba zmazať
num <- Filter(function(x)!all(is.na(x)), num)

# Mazanie medzier medzi cislom a jednotkou:
num[-1] <- lapply(num[-1], function(x) stringr::str_replace_all(x, '[c|(|)|"|\\.]', ''))

# Niektoré merné jednotky možno vynásobiť a získať tak čistú váhu.
# Tvorba sucinoveho stplca pri tovaroch typu 2x20g:
num$g1_nw <- sucinovyStlpec(df = num, col = "g1")
num$g2_nw <- sucinovyStlpec(df = num, col = "g2")

num$kg1_nw <- sucinovyStlpec(df = num, col = "kg1")
num$kg2_nw <- sucinovyStlpec(df = num, col = "kg2")

num$perc1_nw <- sucinovyStlpec(df = num, col = "perc1")
num$perc2_nw <- sucinovyStlpec(df = num, col = "perc2")

num$l1_nw <- sucinovyStlpec(df = num, col = "l1")
num$l2_nw <- sucinovyStlpec(df = num, col = "l2")

num$ml1_nw <- sucinovyStlpec(df = num, col = "ml1")
num$ml2_nw <- sucinovyStlpec(df = num, col = "ml2")

num$ks1_nw <- sucinovyStlpec(df = num, col = "ks1")
num$ks2_nw <- sucinovyStlpec(df = num, col = "ks2")

num$pl1_nw <- sucinovyStlpec(df = num, col = "pl1")
num$pl2_nw <- sucinovyStlpec(df = num, col = "pl2")

num$m1_nw <- sucinovyStlpec(df = num, col = "m1")
num$m2_nw <- sucinovyStlpec(df = num, col = "m2")

# Po dokončení predošlých krokov možno zmazať nasledovný stĺpec:
num$PRODUKT_POPIS <- NULL
# num <- data.frame(sapply(num, function(x)
#     gsub("[^0-9]", "", as.character(x))), stringsAsFactors = FALSE)

# NUM - REGEX - HOTOVO



# 2. CISTENIE TEXTU ---------------------------------------------------------------
# Predtym nebolo vhodne cistit text kvoli potrebe  %, desatinnych ciarok, atd.

# Zoznam nazvov stlpcov s textovymi informaciami
textCols <-
  c("PRODUKT_POPIS",
    "SKUPINA_1_POPIS",
    "SKUPINA_2_POPIS",
    "POPIS_5",
    "POPIS_6")

# Aplikovanie vlastnej funkcie formatujucej text
df[textCols] <- lapply(df[textCols], cleanText)



# ************************************************************************
# 3. ROZKLAD KODOV  -------------------------------------------------------
# ************************************************************************

# stlpce s kodmi
kody <- df[, c("ID_PRODUKT", "EAN", "SKUPINA_1_KOD", "SKUPINA_2_KOD")]


# !!! MAZANIE KONTROLNEJ CISLICE PRI GTIN !!!
# nie celkom koser, lebo nie vsetky gtin su koreknte
kody$EAN <- gsub('.{1}$', '', kody$EAN)

# ROZKLAD PO JEDNOM ***
# spustenie vlastnej funkcie, ktora rozlozi kod do separatnych stlpcov
kody <- cbind(kody, splitNumberCode(kody$ID_PRODUKT, id = "id"))
kody <- cbind(kody, splitNumberCode(kody$EAN, id = "ean"))
kody <- cbind(kody, splitNumberCode(kody$SKUPINA_1_KOD, id = "sk1_"))
kody <- cbind(kody, splitNumberCode(kody$SKUPINA_2_KOD, id = "sk2_"))
# kody_U <- as.data.frame(unclass(kody)) # ako faktor?

# ROZKLAD PO DVOJICIACH ***
kody2 <- df[, c("ID_PRODUKT", "EAN", "SKUPINA_1_KOD", "SKUPINA_2_KOD")]
kody2$EAN <- gsub('.{1}$', '', kody2$EAN)

# spustenie vlastnej funkcie, ktora rozlozi kod do zvlast stlpcov, po 2-cisliach
kody2 <- cbind(kody2, splitNumberCode2(kody2$ID_PRODUKT, id = "id"))
kody2 <- cbind(kody2, splitNumberCode2(kody2$EAN, id = "ean"))
kody2 <- cbind(kody2, splitNumberCode2(kody2$SKUPINA_1_KOD, id = "sk1_"))
kody2 <- cbind(kody2, splitNumberCode2(kody2$SKUPINA_2_KOD, id = "sk2_"))
# kody2 <- as.data.frame(unclass(kody2))# ako faktor?



# VYSTUP SKRIPTU: DF
#                 num
#                 kody
#                 kody2


num <- as.data.frame(num)

getwd()
rm(list=ls()[! ls() %in% c("df", "num", "kody", "kody2")]) # !!!!!! remove
save.image("2_featureEng_kaufland.RData")










