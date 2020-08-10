

# DESCRIPTION: Preprocesing dat - Kaufland
#              základné formátovanie údajov
# testovanie zmeny kodu

# Načítanie R balíkov ---------------------------------------------------

# install.packages("excel.link") # manuálna inštalácia
library(excel.link)
library(stringr) # Common String Operations
library(dplyr) # A Grammar of Data Manipulation
library(tidyr) # Tidy Messy Data
require(tm) # Text Mining


# Nastavenie pracovného priečinka: --------------------------------------
# setwd("C:/Users/Veronika Soldanova/Sync/i")
setwd("C:/Users/Veronika/Sync/i/kaufland")
getwd()

# Načítanie vlastných funkcií:
source("../f_scannerData.R", encoding = "utf-8")




# Načítanie údajov z obchodného reťazca -----------------------------------
# Zadáva sa celá cesta k súborom.

# a) zo zaheslovaného .xslx súboru:
df <- xl.read.file("Z:/Zdroje_SUSR/Kaufland.xlsx", password="machinelearning")
# pre rýchlejšie budúce načítanie možno data frame uložiť ako .RData

# b) alternatívny spôsob - načítavanie súboru už ako .RData
load("raw_data_kaufland.RData") # as df

# záloha pôvodného df
df.0 <- df
# df <- df.0

# Podľa štruktúry DF možno posúdiť potrebný preprocessing údajov
str(df)


# * Medzery * odstranenenie dvojitych a na zaciatku a konci stringu -----------
df <- data.frame(sapply(df, function(x)
  tm::stripWhitespace(as.character(x))), stringsAsFactors = FALSE)

df <- data.frame(sapply(df, function(x)
  trimws(as.character(x))), stringsAsFactors = FALSE)




# * Tolower * prevod veľkých písmen na malé -----------------------------
df <- data.frame(sapply(df, function(x)
  tolower(as.character(x))), stringsAsFactors = FALSE)



# Kontrolné porovnanie
porovnanie <- data.frame(df$PRODUKT_POPIS, df.0$PRODUKT_POPIS, df$MERNA_JEDNOTKA)
# View(porovnanie)



# *Ukladanie* ----------------------------------------------------------

getwd()
rm(list=ls()[! ls() %in% c("df")]) # !!!!!! remove

save.image("preprocData_kaufland.RData")
