#KDD2
#blogok root cimenek leszedese es kiirasa useroldalakrol
#todo: SQL-re atirni

rm(list = ls()) #deleting the memory
setwd("D:/KRE/Projekt/rawdata") #munkakonyvtar

#install.packages("rvest")
#install.packages("xml2")
#install.packages("Rcrawler")
#install.packages("httr")
#install.packages("xml2")

system("java -version")
library("xml2")
library("Rcrawler")
library("rvest")
library("httr")


today1<-Sys.Date()
load(file="1_usersts_justlinks.Rdata") #user oldalak betoltese
l_usrs<- length(usersts)

for (i in c(1054219:90000)) #vegigfutunk a userek oldalain (usersts), 2020.10.28: 1062965-90000
{
  cat("----",i,"----", "\n")
	Rcrawler(Website = usersts[i], no_cores = 1, no_conn = 1, Obeyrobots = TRUE, MaxDepth=1) #user keresenek megfeleloen engedelmeskedunk a robotoknak
	pages <- LinkExtractor(url = unname(as.character(usersts[i])), ExternalLInks=FALSE) #belso linkek kigyujtese, osszes

  if (length(pages$InternalLinks)!=0) #van-e egy link is az oldalon?
    {
    for (j in c(1:length(pages$InternalLinks))) #vegigmegyunk a kinyert linkeken es csak a bloglinkeket taroljuk el!
        {
          if (!grepl("https://blog.hu/", pages$InternalLinks[j], ignore.case = TRUE))
                      {
												cat(as.character(pages$InternalLinks[j]) , ";" , as.character(usersts[i]) , ";" , i , ";", j , ";" , as.Date(today1), "\n", file = "blogrootpgs95.csv", append=TRUE)
										  }
        } #vegigfutunk az leszedett aloldalakon ciklus
    }#van-e egy link is az oldalon?
} #osszes useren veggifutas ciklus vege
