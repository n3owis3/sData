

findMissingData <- function(df, writeXLSX = TRUE) {
  
  # Funkčnosť:
  # 1. vyhľadá stĺpce s chýbajúcimi údajmi;
  # 2. do konzoly vypíše ich počty a krátky náhľad;
  # 3. do priečinka vyexportuje všetky riadky s NA v súbore s formátom .xslx
  
  # ARGUMENTY: df v ktorom majú byť vyhľadané chýbajúce údaje
  # RETURN: zoznam s chýbajúcimi udajmi uloženými v DF podľa názvov stĺpcov
  
  naStlpce <- character()
  nazvyStlpcov <- colnames(df)
  
  for (stlpec in nazvyStlpcov) {
    # zistuje, ci sa nachadzaju NA hodnoty
    if (sum(is.na(df[stlpec])) > 0) {
      df.ch <- df[is.na(df[stlpec]), ]
      
      print("Stĺpec:")
      print(stlpec)
      print("Počet riadkov s chýbajúcimi údajmi:")
      print(nrow(df.ch))
      print("-------------")
      # print("Nahlad chybajucich udajov:")
      # print(head(df.ch, 3))
      
      assign(paste0("missData_", stlpec), df.ch)
      
      if (writeXLSX == TRUE) {
        write.xlsx2(
          df.ch,
          paste0("./prieskumnaAnalyza/chybajuceUdaje_", stlpec, ".xlsx"),
          sheetName = "missingData",
          col.names = TRUE,
          row.names = FALSE,
          append = FALSE,
          showNA = TRUE
        )
      }

      
      if (!exists("df.chybajuce")) {
        df.chybajuce <- df.ch
        naStlpce <- append(naStlpce, stlpec)
      } else {
        df.chybajuce <- list(df.chybajuce, df.ch)
        naStlpce <- append(naStlpce, stlpec)
      }
    }
  }
  names(df.chybajuce) <- naStlpce
  return(df.chybajuce)
}





findDuplicates <- function(df, stlpce) {
  
  # Funkčnosť:
  # 1. vyhľadá stĺpce s duplikovanými údajmi;
  # 2. do konzoly vypíše ich počty a krátky náhľad
  # 3. do priečinka "Chybajuce_udaje" vyexportuje kompletné riadky s NA ako .xslx
  # 4. vráti zoznam s chybajucimi udajmi ulozenými v DF podľa názvov stĺpcov
  
  # ARGUMENTY: df - rieseny data frame
  # RETURN:   LIST obsahujuci DF s duplikatmi
  #           VÝPIS v konzole s počtom duplikatov podla jednotlivych stlpcov
  
  nazvyDF <- character()
  nazvyStlpcov <- stlpce
  
  for (stlpec in nazvyStlpcov) {
    
    duplikatySum <- sum(duplicated(df[stlpec]))
    
    if (duplikatySum > 0) {
      
      # >  vec %in% unique(vec[ duplicated(vec)]) 
      sum(df[, stlpec] %in% unique(df[duplicated(df[stlpec]), stlpec]) )
      duplikatyFilter <- df[, stlpec] %in% unique(df[duplicated(df[stlpec]), stlpec])
      
      duplikaty <- df[duplikatyFilter, ]
      
      
      print("Počet riadkov s duplikátmi v STLPCI:")
      print(stlpec)
      print(nrow(duplikaty))
      print("-----------------------------------")
      # print("Nahlad chybajucich udajov:")
      # print(head(df.dup, 3))
      
      # assign(paste0("duplicatedData_", stlpec), duplikaty)
      
      # write.xlsx2(
      #   duplikaty,
      #   paste0("duplikaty_", stlpec, ".xlsx"),
      #   sheetName = "Sheet1",
      #   col.names = TRUE,
      #   row.names = FALSE,
      #   append = FALSE,
      #   showNA = TRUE
      # )
      
      if (!exists("df.duplikaty")) {
        df.duplikaty <- duplikaty
        nazvyDF <- append(nazvyDF, stlpec)
      } else if (is.data.frame(df.duplikaty)){
        df.duplikaty <- list(duplikaty, df.duplikaty)
        nazvyDF <- append(nazvyDF, stlpec)
      } else {
        df.duplikaty <- append(df.duplikaty, list(duplikaty), 0)
        nazvyDF <- append(nazvyDF, stlpec)
      }
    } else {
      print("Počet riadkov s duplikátmi v STLPCI:")
      print(stlpec)
      print("0")
      print("-----------------------------------")
    }
  }
  
  if(!is.data.frame(df.duplikaty)){
  names(df.duplikaty) <- rev(nazvyDF)
  }
  
  df.duplikaty <- df.duplikaty[1:length(nazvyDF)]
  
  return(df.duplikaty)
}




# kody musia mat rovnaku dlzku , asi
splitNumberCode <- function(column, id = "id") {
  l <- nchar(column[1]) # dlzka kodu v prvej bunke
  empty <- as.data.frame(matrix(nrow = length(column), ncol = l)) # tvorba cisteho df
  # cyklus prejde kazdym cislom a ulozi ho zvlast
  for (j in 1:length(column)) {
    a <- column[j]
    for (i in (1:l)){
      num <- substr(a, i, i)
      empty[j, i] <- num
      print(j)
    }
  }
  colnames(empty) <- paste0(id, seq(1:l))
  empty
}

# test <- cbind(df, splitNumberCode(df$ID_PRODUKT, id = "id"))
# test2 <- cbind(df, splitNumberCode(df$EAN, id = "ean"))

# kody musia mat rovnaku dlzku , asi
splitNumberCode2 <- function(column = df$EAN, id = "ean") {
  l <- nchar(column[1]) # dlzka kodu v prvej bunke
  # tvorba cisteho df
  # if (l%%2==0) {
  empty <- as.data.frame(matrix(nrow = length(column), ncol = l))
  # }  else {
  # empty <- as.data.frame(matrix(nrow = length(column), ncol = l/2+1))
  # }
  
  # cyklus prejde kazdym cislom a ulozi ho zvlast
  for (j in 1:length(column)) {
    a <- column[j]
    for (i in seq(1,l,2)){
      num <- substr(a, i, i+1)
      empty[j, i] <- num
      print(j)
    }
  }
  df.empty <- empty[, seq(1,l,2)]
  colnames(df.empty) <- paste0(id, seq(1,l,2))
  df.empty
}





checkCodeQuality <- function(df, minus = "OBSAH",
  nazovTXT = "checkCodeQuality.txt",
  export = TRUE) {
  
  # Funkčnosť:
  # 1. Selekcia stlpcov obsahujúcich kódy
  # 2. Export súboru .txt s početnosťou jednotlivých unikátnych kódov
  
  # ARGUMENTY: df - df v ktorom majú byť kontrolované kódy
  #            minus - vektor numerických stlpcov, ktore sa nemaju brat v uvahu
  #            nazovTXT - nazov vystupneho txt suboru, default: "checkCodeQuality.txt" 
  #            export - ak je TRUE, vyexportuje txt, inak vypise vysledok do konzoly
  # RETURN:    výpis fo formate txt ulozeny do pracovneho priecinka
  
  
  nazovTXT = paste0("./prieskumnaAnalyza/", nazovTXT)
  
  if(export == TRUE){
  sink(nazovTXT) 
  }
  
  require(janitor)
  require(dplyr)
  
  df.trim <- data.frame(sapply(df, trimws), stringsAsFactors=FALSE)
  
  # kontrola stlpcov s kodmi
  # odfiltrujem textove stlpce a zistim unikatne hodnoty kodov
  df.num <- na.omit(df)
  
  df.num <- data.frame(sapply(df.num,function(x) gsub("\\.","",as.character(x))), stringsAsFactors=FALSE)
  df.num <- suppressWarnings(as.data.frame(sapply(df.num, as.numeric)))
  df.num <- df.num %>% remove_empty("cols")
  # nazvy stlpcov s kodmi
  numNames <- colnames(df.num)
  numNames <- setdiff(numNames, minus)
  # print
  print("Názvy stĺpcov s kódmi:")
  print("***********************")
  print(head(df[numNames], 3))
  print("***********************")
  cat("\n")
  
  # zistenie unikatnych dlzok kodov
  uni <- data.frame(sapply(df.trim[numNames], nchar))
  uni <- sapply(uni, unique)
  # print
  print("Dĺžky kódov podľa stĺpcov:")
  print("***********************")
  print(uni)
  print("*********************")
  cat("\n")
  # ak je pocet unikatnych dlzok > 1 printnem
  filter <- lapply(uni, length) > 1
  filterNames <- names(uni)[filter]
  
  for (nm in filterNames) {
  df.sel <- df.trim
  df.sel[paste0("nchar")] <- as.numeric(sapply(df.trim[nm], nchar))
    for (u in unlist(uni[nm])) {
      # print(u)
        print("Stlpec:")
        print(nm)
        cat("\n")
        print("Dĺžka kódu:")
        print(u)
        cat("\n")
        tt <- dplyr::filter(df.sel, df.sel["nchar"] == u)
        print("Hlavička stĺpca:")
        print(head(unique(tt[nm])))
        cat("\n")
        print("Počet výskytov:")
        print(nrow(tt))
        cat("\n")
        print("**************")
    }
  }
  if(export == TRUE){
  sink()
  }
}





# CHECK GTIN
# calculate manualy
# https://www.gs1.org/services/how-calculate-check-digit-manually
# https://www.swinglabels.com/checkdigits/cd_gtin8.aspx
# checkNumber <- function(col = df$EAN) {
#   
#   # orezanie kodu od zbytocnych medzier
#   col.trim <- sapply(col, trimws)
#   # zistenie poctu cislic v nednotlivych kodoch
#   nchars <- sapply(col.trim, nchar)
#   # vyber iba unikatnych hodnot
#   uniValues <- sort(unique(nchars))
#   
#   df.col <- data.frame(col.trim, nchars, stringsAsFactors = FALSE)
#   
#   
#   for (uni in uniValues) {
#     
#     print("*************")
#     print("Kód s dĺžkou:")
#     print(uni)
#     
#     if (uni %in% c(8, 12, 13, 14, 17, 18)) {
#       
#       # vytvorenie df s unikatnymi dlzkami
#       col.select <- df.col %>%
#         filter(nchars == uni)
#       
#       coll <- col.select
#       coll$nchars <- NULL
#       
#       # rozdelim cislice kodu do samostatnych stlpcov
#       for (i in 1:uni) {
#         # print(i)
#         coll[paste0("n", i)] <- as.numeric(substr(coll$col, i, i))
#       }
#       # odstranenie celeho tvaru kodu a poslednej check cislice
#       r.col <- coll[, c(2:(ncol(coll) - 1))]
#       
#       
#       
#       # podla dlzky gtinu si vyberiem maticu k nasobeniu
#       if (uni == 8) {
#         mat <- c(3, 1, 3, 1, 3, 1, 3)
#       } else if (uni == 12) {
#         mat <- c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
#       } else if (uni == 13) {
#         mat <- c(1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
#       } else if (uni == 14) {
#         mat <- c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
#       } else if (uni == 17) {
#         mat <- c(1,3,1,  3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
#       } else if (uni == 18) {
#         mat <- c(3,1,3,1,  3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
#       } else {
#         break
#       }
#       
#       
#       # replikujem
#       mat <-
#         do.call("rbind", replicate(nrow(r.col), mat, simplify = FALSE))
#       multi <- r.col * mat
#       
#       multi$sum <- rowSums(multi)
#       multi$desat <- multi$sum / 10
#       multi$desatUP <- (ceiling(multi$desat)) * 10
#       
#       multi$check <- multi$desatUP - multi$sum
#       
#       print("Počet nezhodných Check Numbers:")
#       print(sum(coll[ncol(coll)] != multi$check))
#       
#       filter <- coll[ncol(coll)] != multi$check
#       
#       
#       print("Počet zhodných Check Numbers:")
#       print(sum(coll[ncol(coll)] == multi$check))
#       
#       if (sum(filter) > 0) {
#         
#         if (!exists("out.df")) {
#           out.df <- data.frame(
#             ean = coll$col.trim,
#             nchar = sapply(coll$col.trim, nchar),
#             checkNum = coll[ncol(coll)],
#             checkNum.calc = multi$check
#           )
#           out.df <- out.df[filter,]
#           names(out.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
#         } else {
#           temp.df <- data.frame(
#             ean = coll$col.trim,
#             nchar = sapply(coll$col.trim, nchar),
#             checkNum = coll[ncol(coll)],
#             checkNum.calc = multi$check
#           )
#           temp.df <- temp.df[filter,]
#           names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
#           
#           out.df <- rbind(out.df, temp.df)
#         }
#         
#       }
#       
#     } else {
#       print("Pre kód tejto dĺžky neexistuje výpočet kontrolnej číslice.")
#       
#       # vytvorenie df s unikatnymi dlzkami
#       col.select <- df.col %>%
#         filter(nchars == uni)
#       
#       if (!exists("out.df")) {
#         out.df <- data.frame(
#           ean = col.select$col.trim,
#           nchar = sapply(col.select$col.trim, nchar),
#           checkNum = NA,
#           checkNum.calc = NA
#         )
#         names(out.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
#       } else {
#         temp.df <- data.frame(
#           ean = col.select$col.trim,
#           nchar = sapply(col.select$col.trim, nchar),
#           checkNum = NA,
#           checkNum.calc = NA
#         )
#         names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
#         
#         out.df <- rbind(out.df, temp.df)
#       }
#       
#     }
#   }
#   out.df
# }



checkNumber <- function(col = df$EAN) {
  
  # * POUŽITIE FUNKCIE: checkNumber() ****
  
  # Vytvorene podla:
  # https://www.gs1.org/services/how-calculate-check-digit-manually
  # https://www.swinglabels.com/checkdigits/cd_gtin8.aspx
  
  # Funkčnosť:
  # 1. Automaticky preverí zhodu checkNumber z kódu s vypočítanou číslicou
  
  # ARGUMENTY: df$EAN - stlpec obsahujúci kód GTIN
  # RETURN:   LIST obsahujucu DF s problematickými kodmi GTIN
  #               -  ak majú kódy rôzne dĺžky, sú uložené zvlášť
  #           VÝPIS v konzole s počtom zhodných a nezhodných kontrolných číslic
  
  naStlpce <- character()
  # nazvyStlpcov <- colnames(df)
  
  # orezanie kodu od zbytocnych medzier
  # malo by to uz byt, ale pre istotu
  col.trim <- sapply(col, trimws)
  # zistenie poctu cislic v nednotlivych kodoch
  nchars <- sapply(col.trim, nchar)
  # vyber iba unikatnych hodnot
  uniValues <- sort(unique(nchars))
  
  df.col <- data.frame(col.trim, nchars, stringsAsFactors = FALSE)
  
  
  for (uni in uniValues) {
    
    print("*************")
    print("Kód s dĺžkou:")
    print(uni)
    
    if (uni %in% c(8, 12, 13, 14, 17, 18)) {
      
      # vytvorenie df s unikatnymi dlzkami
      col.select <- df.col %>%
        filter(nchars == uni)
      
      coll <- col.select
      coll$nchars <- NULL
      
      # rozdelim cislice kodu do samostatnych stlpcov
      for (i in 1:uni) {
        # print(i)
        coll[paste0("n", i)] <- as.numeric(substr(coll$col, i, i))
      }
      # odstranenie celeho tvaru kodu a poslednej check cislice
      r.col <- coll[, c(2:(ncol(coll) - 1))]
      
      
      
      # podla dlzky gtinu si vyberiem maticu k nasobeniu
      if (uni == 8) {
        mat <- c(3, 1, 3, 1, 3, 1, 3)
      } else if (uni == 12) {
        mat <- c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
      } else if (uni == 13) {
        mat <- c(1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
      } else if (uni == 14) {
        mat <- c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
      } else if (uni == 17) {
        mat <- c(1,3,1,  3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
      } else if (uni == 18) {
        mat <- c(3,1,3,1,  3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3)
      } else {
        break
      }
      
      
      # replikujem
      mat <-
        do.call("rbind", replicate(nrow(r.col), mat, simplify = FALSE))
      multi <- r.col * mat
      
      multi$sum <- rowSums(multi)
      multi$desat <- multi$sum / 10
      multi$desatUP <- (ceiling(multi$desat)) * 10
      
      multi$check <- multi$desatUP - multi$sum
      
      print("Počet nezhodných Check Numbers:")
      print(sum(coll[ncol(coll)] != multi$check))
      
      filter <- coll[ncol(coll)] != multi$check
      
      
      print("Počet zhodných Check Numbers:")
      print(sum(coll[ncol(coll)] == multi$check))
      
      if (sum(filter) > 0) {
        
        
        
        
        if (!exists("out.df")) {
          out.df <- data.frame(
            ean = coll$col.trim,
            nchar = sapply(coll$col.trim, nchar),
            checkNum = coll[ncol(coll)],
            checkNum.calc = multi$check
          )
          
          naStlpce <- append(naStlpce, uni)
          
          out.df <- out.df[filter,]
          names(out.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
          
        } else if (is.data.frame(out.df)){
          
          temp.df <- data.frame(
            ean = coll$col.trim,
            nchar = sapply(coll$col.trim, nchar),
            checkNum = coll[ncol(coll)],
            checkNum.calc = multi$check
          )
          temp.df <- temp.df[filter,]
          names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
          
          
          out.df <- list(out.df, temp.df)
          naStlpce <- append(naStlpce, uni)
        } else {
          temp.df <- data.frame(
            ean = coll$col.trim,
            nchar = sapply(coll$col.trim, nchar),
            checkNum = coll[ncol(coll)],
            checkNum.calc = multi$check
          )
          temp.df <- temp.df[filter,]
          names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
          
          out.df <- append(out.df, list(temp.df), 0)
          naStlpce <- append(naStlpce, uni)
        }
        
      }
      
    } else {
      
      print("Pre kód tejto dĺžky neexistuje výpočet kontrolnej číslice.")
      
      # vytvorenie df s unikatnymi dlzkami
      col.select <- df.col %>%
        filter(nchars == uni)
      
      if (!exists("out.df")) {
        out.df <- data.frame(
          ean = col.select$col.trim,
          nchar = sapply(col.select$col.trim, nchar),
          checkNum = NA,
          checkNum.calc = NA
        )
        names(out.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
        
      } else if (is.data.frame(out.df)){
        
        temp.df <- data.frame(
          ean = col.select$col.trim,
          nchar = sapply(col.select$col.trim, nchar),
          checkNum = NA,
          checkNum.calc = NA
        )
        names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
        
        # out.df <- rbind(out.df, temp.df)
        
        out.df <- list(out.df, temp.df)
        naStlpce <- append(naStlpce, uni)
        
      } else {
        
        temp.df <- data.frame(
          ean = col.select$col.trim,
          nchar = sapply(col.select$col.trim, nchar),
          checkNum = NA,
          checkNum.calc = NA
        )
        names(temp.df) <- c("ean",  "nchar", "checkNum", "checkNum_calc")
        
        # out.df <- rbind(out.df, temp.df)
        out.df <- append(out.df, list(temp.df), 0)
        naStlpce <- append(naStlpce, uni)
        
      }
      
    }
  }
  # names(out.df) <- naStlpce
  # df.duplikaty <- df.duplikaty[1:length(nazvyDF)]
  # return(out.df)
  
  if(!is.data.frame(out.df)){
    names(out.df) <- rev(naStlpce)
  }
  
  out.df <- out.df[1:length(naStlpce)]
  
  return(out.df)
  
  
}




sucinovyStlpec <- function(df = num, col = "g1"){
  # tvorba sucinoveho stplca pri tovaroch typu 2x20g 
  
  df <- separate(df, col, c("x", "y"), sep = "x", remove = FALSE)
  df$x <- stringr::str_replace_all(df$x,"[a-zA-Z\\s]", " ")
  df$y <- stringr::str_replace_all(df$y,"[a-zA-Z\\s]", " ")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  df$new <- df$x * df$y
  df$new <- ifelse(is.na(df$new), df$x, df$new)
  
  return(df$new)
}



# funkcia na cistenie textu
cleanText <- function(col=df.0$PRODUKT_POPIS){
  
  rem <- c("s", "so", "vo", "pre")
  
  require(tm)
  
  temp <- iconv(col, to = "ASCII//TRANSLIT") # odstranenie diakritiky
  temp <- tolower(temp) # lowercase
  
  # niektore slova su oddelene apostrofmi
  # ich zlucenie moze pomoct
  temp <- gsub("'", "", temp)
  temp <- gsub("´", "", temp)
  
  # zmaze vsetko, co nie je znak alebo text
  
  # temp <- stringr::str_replace_all(temp,"[^[:alnum:]]", " ")
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # zmaze stoppwords
  temp <- tm::removeWords(temp, rem)
  
  # zachova len 1 medzeru a oreze
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- tm::stripWhitespace(temp)
  temp <- gsub("  ", " ", temp)
  temp <- trimws(temp)
  
  temp
}

