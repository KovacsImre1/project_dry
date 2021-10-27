#term search in the word database

library(openxlsx)
library(hunspell)
library(dplyr)
library(tidytext)
library(data.table)
library(stringr)

#szamok kiszedese
#adat2 <- adat2[-which(!is.na(as.numeric(adat2$szotovezett))),]
#saveRDS(adat2, file= paste0("tisztitott_blog_data_", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".rds"))
adat1 <- readRDS("3_Data_Transformation/data/adat.RDS")

#COVID szavak
ujszavak <- read.xlsx("uj_szavak_0514.xlsx", colNames = FALSE)
ujszavak <- tolower(unlist(ujszavak))
ujszavak

#Adott szavak keresese blogbejegyzesben
#Csak a koronavirusosak
#examples
#adat3 <- adat2[adat2$szotovezett %in% "sajt",]
#adat3 <- adat2[adat2$szotovezett %in% c("sajt, "sajtoskocka"),]

#adott ev kivalasztasa
adat1[,ev := str_match(datum,"\\d{4}")] #ev valtozo letrehozasa
adat2 <- adat1[ev %in% c(2019,2020)]

#keresett_kifejezesek <- c("...")
#adat3 <- adat2[adat2$szotovezett %in% keresett_kifejezesek,]
adat3 <- adat2[adat2$szotovezett %in% ujszavak,]


#mely bejegyzesekben szerepeltek a keresett szavak
adat_covid <- adat3 %>%
  count(hyperlink1, hyperlink1, sort=TRUE)

#csak a COVIDOS bejegyzesek adatai tokenizalt C)s szotovezett szavakkal
covidos <- adat[which(adat$hyperlink1 %in% adat_covid$hyperlink1),]
szoveg_hossz <- nchar(covidos$szoveg)

