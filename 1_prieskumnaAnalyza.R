

# DESCRIPTION: Prieskumná analýza - Kaufland
#              Skúmanie obsahu stlpcov, chybajucich udajov, duplikatov, atd.


# Načítanie R balíkov ---------------------------------------------------

# install.packages("excel.link") # manuálna inštalácia
library("excel.link")
library("DataExplorer")
library("dplyr")
library("grDevices")
library("ggplot2")
library("xlsx")
library("dplyr")




# Načítanie vlastných funkcií:
source("../f_scannerData.R", encoding = "utf-8")

# Načítavanie údajov po preprocessingu
load("preprocData_kaufland.RData")

# Vytvorenie priečinka pre ukladanie výstupov 
dir.create("prieskumnaAnalyza")





# Prvý náhľad -----------------------------------------------------------------

# Náhľad náhodnej vzorky dát
View(df[sample(1:nrow(df), 15), ])

# Výpis stĺpcov, ich tried a náhľad obsahu 
str(df[sample(1:nrow(df), 15), ])

# POZN: stlpec OBSAH - numeric
df$OBSAH <- as.numeric(df$OBSAH)
str(df[sample(1:nrow(df), 15), ])



# 1. Chýbajúce údaje ---------------------------------------------

# grafické znázornenie chýbajúcich údajov
plot_missing(df)

# ukladanie grafu s chýbajúcimi údajmi
png(file=paste0("./prieskumnaAnalyza/chybajuce_udaje.png"),
  width=2000, height=1000, res = 250)
plot_missing(df)
dev.off()


# * POUŽITIE VLASTNEJ FUNKCIE: findMissingData() ****

# Funkčnosť:
# 1. vyhľadá stĺpce s chýbajúcimi údajmi;
# 2. do konzoly vypíše ich počty a krátky náhľad;
# 3. do priečinka vyexportuje všetky riadky s NA v súbore s formátom .xslx

# ARGUMENTY: df v ktorom majú byť vyhľadané chýbajúce údaje
# RETURN: zoznam s chýbajúcimi udajmi uloženými v DF podľa názvov stĺpcov

naData <- findMissingData(df, writeXLSX = FALSE)

# **** 


# chýbajúce údaje:
MJ <- naData$MERNA_JEDNOTKA
nrow(MJ) # 3273

P6 <- naData$POPIS_6
nrow(P6) # 3


# RIEŠENIE:

# MERNA_JEDNOTKA - zmazať, následne doplniť zo stlpca PRODUKT_POPIS 
df$MERNA_JEDNOTKA <- NULL

# POPIS_6 - iba 3 riadky - zmazat 
# chybajuci popis ku kodu: "01.1.1.8.6"
df <- df %>% 
  filter(!is.na(df$POPIS_6))







# 2. Stlpce s kódmi ----------------------------------------------------------


# * POUŽITIE FUNKCIE: checkCodeQuality() ****

# Funkčnosť:
# 1. Selekcia stlpcov obsahujúcich kódy
# 2. Export súboru .txt s početnosťou jednotlivých unikátnych kódov

# ARGUMENTY: df - df v ktorom majú byť kontrolované kódy
#            minus - vektor numerických stlpcov, ktore sa nemaju brat v uvahu
#            nazovTXT - nazov vystupneho txt suboru, default: "checkCodeQuality.txt" 
#            export - ak je TRUE, vyexportuje txt, inak vypise vysledok do konzoly
# RETURN:    výpis fo formate txt ulozeny do pracovneho priecinka

checkCodeQuality(df, minus=c("OBSAH"), nazovTXT = "checkCodeQuality.txt", export = FALSE)

# **** 

# VÝSLEDOK:

# *****************
# 1: CHÝBAJÚCE KÓDY :
# *****************

# [1] "Hlavička stĺpca:"
# ECOICOP6
# 1     0000
# [1] "Počet výskytov:"
# [1] 145


# [1] "Hlavička stĺpca:"
# ECOICOP5
# 1     0000
# [1] "Počet výskytov:"
# [1] 145


# RIEŠENIE: ODSTRÁNENIE RIADKOV

df <- df %>%  # ZMAZAT
  filter(ECOICOP5 != "0000") # na blackliste su produkty s nulovym ECOICOP

df <- df %>%  # ZMAZAT
  filter(ECOICOP6 != "0000") # na blackliste su produkty s nulovym ECOICOP


# VÝSLEDOK:

# *****************
# 1: NESPRÁVNE DĹŽKY KÓDOV :
# *****************

# [1] "Dĺžky kódov podľa stĺpcov:"
# [1] "***********************"
# 
# $EAN
# [1] 13 11  8  4 12

# *****************
# 2: 4-MIESTNY EAN
# *****************

# [1] "STLPEC:"
# [1] "EAN"
# 
# [1] "Dĺžka kódu:"
# [1] 4
# 
# [1] "Hlavička stĺpca:"
# EAN
# 1 3334
# 2 7221
# 3 7222
# 4 7223
# 5 7224
# 6 7225
# 
# [1] "Počet výskytov:"
# [1] 743

# RIEŠENIE: DOČASNÉ PONECHANIE RIADKOV
#           ZISTENIE VPLYVU ICH ODSTRANENIA NA PRESNOST PREDIKCIE TRIED

df <- df %>%  # ZMAZAT
  filter(ECOICOP5 != "0000") # na blackliste su produkty s nulovym ECOICOP

df <- df %>%  # ZMAZAT
  filter(ECOICOP6 != "0000") # na blackliste su produkty s nulovym ECOICOP


# kontrola:
checkCodeQuality(df, minus=c("OBSAH"), export = FALSE)

# OK:
# $ECOICOP5
# [1] 8
# 
# $ECOICOP6
# [1] 10




# 3. Kontrola GTIN checkNumber -----------------------------------------------------


# * POUŽITIE FUNKCIE: checkNumber() ****

# Vytvorene podla:
# https://www.gs1.org/services/how-calculate-check-digit-manually
# https://www.swinglabels.com/checkdigits/cd_gtin8.aspx

# Funkčnosť:
# 1. Automaticky preverí zhodu checkNumber z kódu s vypočítanou číslicou

# ARGUMENTY: df$EAN - stlpec obsahujúci kód GTIN
# RETURN:   LIST obsahujuci DF s problematickými kodmi GTIN
#               -  ak majú kódy rôzne dĺžky, sú uložené zvlášť
#           VÝPIS v konzole s počtom zhodných a nezhodných kontrolných číslic

# vlastná funkcia
checkN <- checkNumber(df$EAN)

# ****

# štruktúra outputu z funkcie
str(checkN)
# nove df s vysledkom
checkN11 <- checkN$`11`
checkN13 <- checkN$`13`


# RIEŠENIE: DOČASNÉ PONECHANIE RIADKOV
#           ZISTENIE VPLYVU ICH ODSTRANENIA NA PRESNOST PREDIKCIE TRIED





# 4. Duplikáty ---------------------------------------------------------------


# * POUŽITIE FUNKCIE: findDuplicates() ****

# Funkčnosť:
# 1. vyhľadá stĺpce s duplikovanými údajmi;
# 2. do konzoly vypíše ich počty a krátky náhľad
# 3. do priečinka "Chybajuce_udaje" vyexportuje kompletné riadky s NA ako .xslx
# 4. vráti zoznam s chybajucimi udajmi ulozenými v DF podľa názvov stĺpcov

# ARGUMENTY: df - rieseny data frame
# RETURN:   LIST obsahujuci DF s duplikatmi
#           VÝPIS v konzole s počtom duplikatov podla jednotlivych stlpcov

# vlastná funkcia
duplikaty <-
  findDuplicates(df,
    stlpce = c(
      "ID_PRODUKT",
      "EAN",
      "SKUPINA_1_KOD",
      "SKUPINA_2_KOD",
      "ECOICOP5"
    ))

# ****

dupEAN <- duplikaty$EAN

head(duplikaty$EAN)
head(duplikaty$EAN[order(duplikaty$EAN$EAN),])
head(duplikaty$SKUPINA_1_KOD[order(duplikaty$SKUPINA_1_KOD$SKUPINA_1_KOD),])
head(duplikaty$SKUPINA_2_KOD[order(duplikaty$SKUPINA_2_KOD$SKUPINA_2_KOD),])

# RIEŠENIE: DOČASNÉ PONECHANIE DUKLIKÁTOV
#           ZISTENIE VPLYVU ICH ODSTRANENIA NA PRESNOST PREDIKCIE TRIED






# 5. Numerické stlpce --------------------------------------------------------

# Zobrazenie histogramov vsetkych numerickych stlpcov

# V DF kaufland ide len o stlpec OBSAH
plot_histogram(df$OBSAH, geom_histogram_args = list(bins = 270L))
# logaritmovane
plot_histogram(log(df$OBSAH), geom_histogram_args = list(bins = 270L))

# Quartily
summary(df$OBSAH)
with(df, table(OBSAH)) # mozno neskor skusit zmazat alebo nahradit obsah > 100



obsahy <- df %>%
  group_by(POPIS_5) %>%
  summarise(OBSAH_mean = mean(OBSAH, na.rm = T))


hist(tvarohSyry$OBSAH)
boxplot(tvarohSyry$OBSAH)
# IDEA: zmazat pri kazdom druhu tovaru outliers
# skusat klastrovat s nimi aj bez nich


# vykresli frekvenciu kategorickych premennych
plot_bar(df)

with(df, table(ZAKLAD_MNOZSTVO_MJ))
with(df, table(MERNA_JEDNOTKA))

with(df, table(ID_PRODUKT))
with(df, table(EAN))

with(df, table(SKUPINA_1_KOD))
with(df, table(SKUPINA_1_POPIS))
with(df, table(SKUPINA_2_KOD))
with(df, table(SKUPINA_2_POPIS))

with(df, table(ECOICOP5))
with(df, table(ECOICOP6))


# vyhladanie DUPLIKATOV:
sum(duplicated(df$ID_PRODUKT))
sum(duplicated(df$PRODUKT_POPIS))
sum(duplicated(df$EAN))
sum(duplicated(df$SKUPINA_1_KOD))
sum(duplicated(df$SKUPINA_1_POPIS))
sum(duplicated(df$ECOICOP5))
sum(duplicated(df$POPIS_6))


duplikovanePP <- df[duplicated(df$PRODUKT_POPIS), ]
dup <- duplikovanePP$PRODUKT_POPIS
duplikaty <- df %>% 
  filter(PRODUKT_POPIS%in%dup)

duplikovaneEAN <- df[duplicated(df$EAN), ]
dup <- duplikovaneEAN$EAN
duplikaty <- df %>% 
  filter(EAN%in%dup)

write.csv(duplikaty, "duplikaty.csv", quote = F, dec = 10)
d <- duplikaty[1:2, ]
View(d)

library(ggplot2)
ggplot(data = df) +
geom_bar(mapping = aes(x = ECOICOP5))

ggplot(data = df) +
geom_bar(mapping = aes(x = ECOICOP6))






# TESTOVANIE VYMAZANIA OUTLIERS
df.out <- df[0, ]
df.outliers <- df[0, ]

# opacna funkcia k %in%
`%notin%` = function(x,y) !(x %in% y)

i = 10
# zmazanie outliers
for (i in 1:length(unique(df$POPIS_5))){
  
  kat <- unique(df$POPIS_5)[i]
  
  kat.df <- df %>% 
    filter(POPIS_5 == kat)
  summary(kat.df$OBSAH)
  boxplot(kat.df$OBSAH)
  
  x <- kat.df$OBSAH
 
  outliers <-  x[which(x %in% boxplot.stats(x)$out)]
  
  if (length(outliers) == 0) {
    
    df.out <- rbind(df.out, kat.df)

      
  } else {
    
    kat.df.out <- kat.df %>% 
      filter(OBSAH %notin% outliers)
    
    kat.df.in <- kat.df %>% 
      filter(OBSAH %in% outliers)
    
    df.out <- rbind(df.out, kat.df.out)

  }

  # print(kat)
  # # Sys.sleep(1)
}


# krajsi histogram
# ggplot(df.0, aes(x=OBSAH)) + geom_histogram(bins=5, alpha=0.5, fill = "blue")





# UKLADANIE
getwd()
rm(list=ls()[! ls() %in% c("df")]) # !!!!!! remove
save.image("analyzovane_kaufland.RData")


