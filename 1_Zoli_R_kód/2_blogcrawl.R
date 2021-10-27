#KDD2
#

#blog.hu website crawler
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
library(xml2)

#Adatok betoltese, a weboldalak nyitooldalanak betoltese
setwd("D:/KRE/Projekt/rawdata")
load(file="1_webcmk.Rdata")
l_wbcmk1 <- dim(webcmk1)[1]

#felepitese: oldanev=as.character("weboldal_https_cime"),lastmod="-0001-11-30T00:00:00+0100",changefreq=as.character("yearly"),priority=0.1, letoltve=as.character("2020-10-14 13:28:18 CEST"), alxmlid=0, alxmlnev=as.character("alxml_cime_adott_idopontban")

#


#tisztitas, elso 3 az tesztoldal, nem belepheto
wbcmk1<-webcmk1[3:l_wbcmk1 ,c(1:2)]
wbcmk<-data.frame(hlnk=as.character(wbcmk1$oldanev), lstmd=as.character(wbcmk1$lastmod), lstmdy=as.numeric(substr(wbcmk1$lastmod, 0, 4)))

#van par lastmy==0, lastmy==9999, ezek, ahol nincs bejegyzes vagy valami gond van a bloggal
wbcmk_cln<-wbcmk[ ((wbcmk$lstmdy>1990) & (wbcmk$lstmdy<9999)),]



sitemap1 <- data.frame(pages=as.character("oldalnev"), status=as.character("http_status"), lekerve=as.character("lekeres_datuma"), wbcmk1id=0)

for (i in c(1:l_wbcmk1)) #vegigfutunk a webblogcimeken
{
	Rcrawler(Website = webcmk1[i], no_cores = 1, no_conn = 1, Obeyrobots = TRUE) #user keresenek megfeleloen engedelmeskedunk a robotoknak
	pages <- LinkExtractor(url = unname(as.character(webcmk1[i,1])), ExternalLInks=FALSE) #belso linkek kigyujtese
  #ideiglenes sitemap osszeallitasa
	sitemapt <- data.frame(pages=pages$InternalLinks, status=NA, lekerve=as.character(Sys.time()), wbcmk1id=i)
	#sitemapon vegigmegy es lekeri a http statust
	#for(i in 1:dim(sitemapcsv)[1])
	#	{
	#	sitemapcsv[i,2]<-http_status(GET(as.character(sitemapcsv[i,1])))$message
	#	sitemapcsv[i,3]<-Sys.time()
	#	}

	cbind(sitemap1,sitemapt) #a nagy sitemaphoz a leszedett sitemap hosszafuzese
}
save(sitemap1, file="sitemap1.rmd", compression_level=9, eval.promises = TRUE, precheck = TRUE)
