
# ---------------------------------
#          StemmeR SK
# ---------------------------------


# slovensky stemmer prerobeny podla funkcie z pythonu
# v komentaroch nad R kodom vzdy bude jeho pythonovska verzia

word="stavenište"
word="kolotoče"
word="kolotoče"

palatalise <- function(word){
  
  require(dplyr)  
  # v komentaroch nad R kodom vzdy bude jeho pythonovska verzia
  # if word[-2:] in ("ci", "ce", "či", "če"):
  #   return word[:-2] + "k"
  if (substr(word,nchar(word)-1, nchar(word)) %in% c("ci", "ce", "či", "če")){
    out_word = paste0(substr(word, 1, nchar(word)-2)) # odstranila som "k" a -1 namiesto -2
  }  
  # if word[-2:] in ("zi", "ze", "ži", "že"):
  #   return word[:-2] + "h"
  if (substr(word,nchar(word)-1, nchar(word)) %in% c("zi", "ze", "ži", "že")){
    out_word = paste0(substr(word, 1, nchar(word)-1))
  }  
  # if word[-3:] in ("čte", "čti", "čtí"):
  #   return word[:-3] + "ck"
  if (substr(word,nchar(word)-2, nchar(word)) %in% c("čte", "čti", "čtí")){
    out_word = paste0(substr(word, 1, nchar(word)-3),"ck")
  }  
  # if word[-3:] in ("šte", "šti", "ští"):
  #   return word[:-3] + "sk"
  if (substr(word,nchar(word)-2, nchar(word)) %in% c("šte", "šti", "ští")){
    out_word = paste0(substr(word, 1, nchar(word)-3),"sk")
  }  
  else {
    out_word
  }
}

word = "kolotoče"
word = substr(word, 1, nchar(word)-2)
palatalise(word)

# testovanie kvoli porovnaniu s pythonom
palatalise("prísediaci")
palatalise("čarujúci")
palatalise("letiaci")
palatalise("priplávajúci")
palatalise("kolotoče")
palatalise("roztoče")
palatalise("nevädnúce")
palatalise("spiace")
palatalise("vykrádači")
palatalise("nasierači")
palatalise("betonáže")
palatalise("naviaže")
palatalise("priviaže")
palatalise("nože")
palatalise("krádeže")
palatalise("američtí")
palatalise("nástupište")
palatalise("nevyveští")
palatalise("učilišti")






word = "zvieracích"

remove_case <- function(word) {
  
  require(dplyr)
  
  # if len(word) > 7 and word.endswith("atoch"):
  #   return word[:-5]
  if (nchar(word) > 7 &
      substr(word, nchar(word) - 4, nchar(word)) %in% c("atoch")) {
    out_word = paste0(substr(word, 1, nchar(word) - 5))
  }
  
  # if len(word) > 6:
  #   if word.endswith("aťom"):
  #   return _palatalise(word[:-3])
  if (nchar(word) > 6) {
    if (substr(word, nchar(word) - 4, nchar(word)) %in% c("aťom")) {
      out_word = palatalise(substr(word, 1, nchar(word) - 3))
    }
  }
  
  
  # if len(word) > 5:
  #   if word[-3:] in ("och", "ich", "ích", "ého", "ami", "emi", "ému",
  #     "ete", "eti", "iho", "ího", "ími", "imu", "aťa"):
  #   return _palatalise(word[:-2])
  if (nchar(word) > 5) {
    if (substr(word, nchar(word) - 2, nchar(word)) %in% c(
      "och",
      "ich",
      "ích",
      "ého",
      "ami",
      "emi",
      "ému",
      "ete",
      "eti",
      "iho",
      "ího",
      "ími",
      "imu",
      "aťa"
    )) {
      palatalise(substr(word, 1, nchar(word) - 3))
    }
    
    
    # if word[-3:] in ("ách", "ata", "aty", "ých", "ami",
    #   "ové", "ovi", "ými"):
    #   return word[:-3]
    if (substr(word, nchar(word) - 2, nchar(word)) %in% c("ách", "ata", "aty", "ých", "ami", "ové", "ovi", "ými")) {
      out_word = palatalise(substr(word, 1, nchar(word) - 3))
    }
  }
  # IF len > 4
  # if len(word) > 4:
  #   if word.endswith("om"):
  #   return _palatalise(word[:-1])
  # if word[-2:] in ("es", "ém", "ím"):
  #   return _palatalise(word[:-2])
  # if word[-2:] in ("úm", "at", "ám", "os", "us", "ým", "mi", "ou", "ej"):
  #   return word[:-2]
  
  if (nchar(word) > 4) {
    if (substr(word, nchar(word) - 1, nchar(word)) %in% c( "om")) {
      palatalise(substr(word, 1, nchar(word) - 1))
    }
    if (substr(word, nchar(word) - 1, nchar(word)) %in% c("es", "ém", "ím")) {
      palatalise(substr(word, 1, nchar(word) - 2))
    }
    if (substr(word, nchar(word) - 1, nchar(word)) %in% c("úm", "at", "ám", "os", "us", "ým", "mi", "ou", "ej")) {
      palatalise(substr(word, 1, nchar(word) - 2))
    }
  }
  
  # len > 3 --
  # if len(word) > 3:
  #   if word[-1] in "eií":
  #   return _palatalise(word)
  # if word[-1] in "úyaoáéý":
  #   return word[:-1]
  
  if (nchar(word) > 3) {
    if (substr(word, nchar(word) - 1, nchar(word)) %in% c( "eií")) {
      palatalise(word)
    }
    if (substr(word, nchar(word) - 1, nchar(word)) %in% c("úyaoáéý")) {
      palatalise(substr(word, 1, nchar(word) - 1))
    }
  }
  
}




def _remove_derivational(word):
  if len(word) > 8 and word.endswith("obinec"):
  return word[:-6]
if len(word) > 7:
  if word.endswith("ionár"):
  return _palatalise(word[:-4])
if word[-5:] in ("ovisk", "ovstv", "ovišt", "ovník"):
  return word[:-5]
if len(word) > 6:
  if word[-4:] in ("ások", "nosť", "teln", "ovec", "ovík",
    "ovtv", "ovin", "štin"):
  return word[:-4]
if word[-4:] in ("enic", "inec", "itel"):
  return _palatalise(word[:-3])
if len(word) > 5:
  if word.endswith("árn"):
  return word[:-3]
if word[-3:] in ("enk", "ián", "ist", "isk", "išt", "itb", "írn"):
  return _palatalise(word[:-2])
if word[-3:] in ("och", "ost", "ovn", "oun", "out", "ouš",
  "ušk", "kyn", "čan", "kář", "néř", "ník",
  "ctv", "stv"):
  return word[:-3]
if len(word) > 4:
  if word[-2:] in ("áč", "ač", "án", "an", "ár", "ar", "ás", "as"):
  return word[:-2]
if word[-2:] in ("ec", "en", "ér", "ír", "ic", "in", "ín",
  "it", "iv"):
  return _palatalise(word[:-1])
if word[-2:] in ("ob", "ot", "ov", "oň", "ul", "yn", "čk", "čn",
  "dl", "nk", "tv", "tk", "vk"):
  return word[:-2]
if len(word) > 3 and word[-1] in "cčklnt":
  return word[:-1]
return word


remove_case <- function(word) {
  
  # if len(word) > 8 and word.endswith("obinec"):
  #   return word[:-6]
  if (nchar(word) > 8) {
    if (endsWith(word, "obinec")) {
      word = substr(word, 1, nchar(word) - 5)
    }
}
    # if len(word) > 7:
    #   if word.endswith("ionár"):
    #   return _palatalise(word[:-4])
    # if word[-5:] in ("ovisk", "ovstv", "ovišt", "ovník"):
    #   return word[:-5]
  if (nchar(word) > 7) {
    if (endsWith(word, "ionár")) {
      word = palatalise(substr(word, 1, nchar(word) - 3)) 
      }
    
    if (substr(word, nchar(word) - 4, nchar(word)) %in% c("ovisk", "ovstv", "ovišt", "ovník")) {
      word = substr(word, 1, nchar(word) - 4)
    }}
  
  
  # robit
  if (nchar(word) > 6) {
    if (endsWith(word, "ionár")) {
      word = palatalise(substr(word, 1, nchar(word) - 3)) 
    }
    
    if (substr(word, nchar(word) - 4, nchar(word)) %in% c("ovisk", "ovstv", "ovišt", "ovník")) {
      word = substr(word, 1, nchar(word) - 4)
    }}
  

}

def _remove_augmentative(word):
  if len(word) > 6 and word.endswith("ajzn"):
  return word[:-4]
if len(word) > 5 and word[-3:] in ("izn", "isk"):
  return _palatalise(word[:-2])
if len(word) > 4 and word.endswith("ák"):
  return word[:-2]
return word


word = "abcgshhajzn"

remove_augmentative <- function(word){
  require(dplyr)  
  # if len(word) > 6 and word.endswith("ajzn"):
  #   return word[:-4]
  if (substr(word,nchar(word)-3, nchar(word)) %in% c("ajzn") & nchar(word > 6)) {
    substr(word, 1, nchar(word)-4)
  }
  # if len(word) > 5 and word[-3:] in ("izn", "isk"):
  #   return _palatalise(word[:-2])
  if (substr(word,nchar(word)-3, nchar(word)) %in% c("izn", "isk") & nchar(word > 5)) {
    substr(word, 1, nchar(word)-2)
  }
  # if len(word) > 4 and word.endswith("ák"):
  #   return word[:-2]
  if (substr(word,nchar(word)-3, nchar(word)) %in% c("ák") & nchar(word > 4)) {
    substr(word, 1, nchar(word)-2)
  }
}

remove_augmentative(word)
remove_augmentative("širák")




def _remove_case(word):
  # if len(word) > 7 and word.endswith("atoch"):
  # return word[:-5]
if len(word) > 6:
  if word.endswith("aťom"):
  return _palatalise(word[:-3])
if len(word) > 5:
  if word[-3:] in ("och", "ich", "ích", "ého", "ami", "emi", "ému",
    "ete", "eti", "iho", "ího", "ími", "imu", "aťa"):
  return _palatalise(word[:-2])
if word[-3:] in ("ách", "ata", "aty", "ých", "ami",
  "ové", "ovi", "ými"):
  return word[:-3]
if len(word) > 4:
  if word.endswith("om"):
  return _palatalise(word[:-1])
if word[-2:] in ("es", "ém", "ím"):
  return _palatalise(word[:-2])
if word[-2:] in ("úm", "at", "ám", "os", "us", "ým", "mi", "ou", "ej"):
  return word[:-2]
if len(word) > 3:
  if word[-1] in "eií":
  return _palatalise(word)
if word[-1] in "úyaoáéý":
  return word[:-1]
return word



