#data cleaning and transformation
#stamming
#stop words

#useful stuffs:
#https://www.tidytextmining.com/index.html

#garbage collector if running out of memory
gc()
#allocationg memory from HDD/SSD
memory.limit(size=90000)

library(openxlsx)
library(hunspell)
library(dplyr)
library(tidytext)
library(data.table)

########################################################
#Szótár beolvasása
hu <- dictionary("./szotarak/hu.dic")

#Szotarban nem szereplo kifejezesek hozzadasa
#COVID szavak hozzadasa
#uj szavak excel a driveon
ujszavak <- read.xlsx("uj_szavak_0514.xlsx", colNames = FALSE)
ujszavak <- tolower(unlist(ujszavak))

#nevek
nevek <- read.xlsx("ferfi_noi_nevek.xlsx", colNames = TRUE)
nevek <- tolower(unlist(nevek$Search.item))

#helységnevek
helysegnevek <- read.xlsx("helysegek.xlsx", colNames = FALSE)
helysegnevek <- tolower(unlist(helysegnevek))

extra_szavak <- read.xlsx("extra_szavak.xlsx", colNames = FALSE)
extra_szavak <- tolower(unlist(extra_szavak))

#szotar
#"hu" vagy hu
#szavak hozzadasa az "add_words" után 
dictionary <- dictionary(lang = "hu", add_words = c(ujszavak, nevek, helysegnevek, extra_szavak))

#stopszavak beolvasása /kiszedése
# stopszo <- read.csv(file="https://raw.githubusercontent.com/stopwords-iso/stopwords-hu/master/stopwords-hu.txt", header = FALSE, sep = ";", dec = ".", encoding="UTF-8", quote = "\"")
stopszo <- readRDS("stopszo.RDS")
colnames(stopszo)=c("word")
stopszo$word <- as.character(stopszo$word)
########################################################

#Adatbazis beolvasasa
#Eredeti, stemmeles elott, a bejegyzesek szovege egyben tarolt
adat <- readRDS("vegleges_2_potlassal.rds")

#Adatbazis tisztitasa
#https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r
adat <- adat[rowSums(is.na(adat)) != ncol(adat),]

#NA
adat <- adat[rowSums(is.na(adat)) != ncol(adat),]

#üres blogbejegyzesek kidobolasa
adat <- adat[-which(nchar(adat$szoveg)==0 | adat$szoveg == 'none' | adat$szoveg == 'none_999'),]

#szovegek tokenizalasa
#a nagy mennyisegu adat miatt reszekre bontottam
adat11 <- adat[(floor(length(adat$hyperlink1)/2)+1):(length(adat$hyperlink1)-100000),]
adat11 <- adat[(length(adat$hyperlink1)-100000+1):length(adat$hyperlink1),]
#szoveg oszlop tokenizalasa szavakra
adat2 <- adat11 %>% unnest_tokens(word, szoveg)
rm(adat11)
# saveRDS(adat3, paste0("adat_unnested_3", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))


#Egyesites
#adat2 <- rbindlist(list(adat12, adat3))
#adat2 <- rbindlist(list(adat12, adat13))
#adat2 <- rbindlist(list(adat1, adat2))
#saveRDS(adat2, paste0("adat2_osszes_unnested_", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))



#anti_join stopszavak
adat2 <- adat2 %>% anti_join(stopszo)

#Ha reszletekben stopszvazunk
# file_list <- list.files(pattern = "adat2_anti_join_")
# adat2 <- data.table()
#összefuzes
# for (i in 1:length(file_list)){
#   temp_data <- readRDS(file_list[i]) #read in files using the fread function from the data.table package
#   adat2 <- rbindlist(list(adat2, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
# }
# saveRDS(adat2, "adat2_osszefuzott_anti_join.RDS")


#Mentes es beolvassas
# saveRDS(adat2, "adat2_osszefuzott_anti_join.RDS")
# adat <- readRDS("adat.RDS")
# adat2 <- readRDS("adat2_osszefuzott_anti_join.rds")
# adat2 <- data.table(adat2)

#szamok kiszedese
adat2 <- adat2[-which(!is.na(as.numeric(adat2$word))),]


#Amennyiben a szovegben a hasznalt karakterkeszlettol eltero karakterek is szerepelnek ki kell oket valogatni
#lassu folyamat
#hibas szavak kiszedese stem elott
rossz_stem_szavak_sorszam <- numeric()
rossz_stem_szavak <- character()
for(i in 1:length(adat2$word)){
  if(class(try(

    hunspell_stem(adat2$word[i], dict = dictionary)))== "try-error"){

    rossz_stem_szavak_sorszam <- append(rossz_stem_szavak_sorszam , i)
    rossz_stem_szavak <- append(rossz_stem_szavak , adat2$word[i])
    cat("\n", i , "\n", adat2$word[i], "\n")
  }
}
# rossz_stem_szavak
# rossz_stem_szavak_sorszam

#saveRDS(rossz_stem_szavak_sorszam, file = paste0("rossz_stem_szavak_sorszam" , format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))
#saveRDS(rossz_stem_szavak, file = paste0("rossz_stem_szavak" , format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))
#beolvasas, ha mar lefuttattuk
#rossz_stem_szavak <- readRDS("rossz_stem_szavak.RDS")

#rossz_stem_szavak_sorszam <- readRDS("rossz_stem_szavak_sorszam2021_05_09_21_38_29.RDS")

#rossz karakterkodolasu szavak torlese
adat2 <- adat2[-rossz_stem_szavak_sorszam, ]

szotovezett[which(szotovezett == "character(0)")] <- "nincs_szoveg_999"

#saveRDS(szotovezett, "szotovezett_3.RDS")
#szotovezett3 <- readRDS("szotovezett_3.RDS")
# #stem utan charater(0) erteket ado szavak
# plusz_szavak <- adat2[which(szotovezett == "character(0)"), ]
# plusz_szavak <- plusz_szavak %>%
#                     count(word, sort = TRUE)
# saveRDS(plusz_szavak, file = paste0("extra_szavak_" , format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))
# View(plusz_szavak)
#character(0) csere
#szotovezett <- readRDS("szotovezett_plain.RDS")
szotovezett[which(szotovezett == "character(0)")] <- "nincs_szoveg_999"


#csak az elso talalat
szotovezett <- lapply(szotovezett, '[[', 1)
szotovezett <- unlist(szotovezett)

#saveRDS(szotovezett, "szotovezett_2.RDS")
#hozzafüzzük az tokenizalt melle a szotovezett szavakat
#length(szotovezett3)
adat2 <- cbind(adat2, szotovezett)

#stopszavazas 2nd time
colnames(stopszo)=c("szotovezett")
adat2 <- adat2 %>% anti_join(stopszo)

#szamok kiszedese
#adat2 <- adat2[-which(!is.na(as.numeric(adat2$szotovezett))),]
#saveRDS(adat2, file= paste0("tisztitott_blog_data_", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))
adat2 <- readRDS("data/tisztitott_blog_data_2021_05_10_00_49_01.RDS")

